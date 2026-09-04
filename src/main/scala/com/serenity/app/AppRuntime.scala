package com.serenity.app

import java.awt.Color
import java.nio.file.Path

import scala.concurrent.duration.*

import cats.effect.*
import cats.effect.std.Dispatcher
import cats.syntax.parallel.*
import cats.syntax.semigroup.*
import com.serenity.config.{AppConfig, CursorMode, MotionFamily, RenderFpsTarget}
import com.serenity.diagnostics.Trace
import com.serenity.input.*
import com.serenity.keystroke.KeyboardFidelityTier
import com.serenity.keystroke.events.{Event, UnhandledEvent}
import com.serenity.keystroke.translators.TextEntryTranslator
import com.serenity.lsp.LspManager
import com.serenity.state.manager.*
import com.serenity.state.models.{AppState, BufferId, Damage, Focus}
import com.serenity.ui.layout.ViewportSize
import com.serenity.ui.renderer.RenderController
import com.serenity.ui.theme.ColorFormat.withAlpha
import fs2.Stream
import fs2.concurrent.SignallingRef
import org.typelevel.log4cats.{Logger, LoggerFactory}

object AppRuntime:

  private[serenity] type RenderFn =
    (AppState, Boolean, Option[Color], Damage, Map[BufferId, com.serenity.animation.AnimationState]) => IO[Unit]

  private val NanosPerSecond: Long                      = 1_000_000_000L
  private val DefaultCursorIdleInterval: FiniteDuration = 500.millis

  private[serenity] def fastFrameInterval(target: RenderFpsTarget): FiniteDuration =
    FiniteDuration(NanosPerSecond / target.framesPerSecond.toLong, NANOSECONDS)

  private[serenity] def fastFrameDelay(
    frameInterval: FiniteDuration,
    isInitialFrame: Boolean = false
  ): FiniteDuration =
    if isInitialFrame then Duration.Zero else frameInterval

  /** The idle phase's per-tick cadence, or `None` when it has nothing to tick for and should sleep indefinitely instead
    * (see [[awaitFocusedIdleTick]]).
    *
    * `isTuiMode` adds a second, TUI-specific reason to return `None` on top of the existing motion-disabled one
    * (#1170): in TUI blink mode the caret is delegated to the terminal's own cursor (`Renderer.presentHardwareCursor`),
    * which owns blink timing entirely, so the app has no idle work left to do. Breathe mode is the documented exception
    * -- it animates color/opacity over time, which a terminal cursor style can't represent -- so it keeps the normal
    * cadence.
    */
  private[serenity] def cursorIdleInterval(config: AppConfig, isTuiMode: Boolean = false): Option[FiniteDuration] =
    if isTuiMode && config.cursorMode == CursorMode.Blink then None
    else
      val cursorMotion =
        config.surfaceConfig.effectiveMotionConfiguration.family(com.serenity.config.MotionFamily.Cursor)
      val scale = AppConfig.clampElementTransitionSpeedScale(cursorMotion.speedScale)
      Option.when(cursorMotion.enabled && scale > 0.0)(
        FiniteDuration(
          math.max(1L, math.round(DefaultCursorIdleInterval.toNanos.toDouble * scale)),
          NANOSECONDS
        )
      )

  private[serenity] def resetCursorActivity(cursorVisible: Ref[IO, Boolean], breathIndex: Ref[IO, Int]): IO[Unit] =
    cursorVisible.set(true) >> breathIndex.set(0)

  /** React to a Swing window focus transition. Losing focus parks the cursor visible-and-steady (reset to the start of
    * its blink/breathe cycle) and forces one fast render so the steady caret paints immediately, regardless of where
    * the idle loop was in its own cadence. Regaining focus only flips the signal the idle loop is waiting on --
    * `awaitFocusedIdleTick` picks that up and resumes the normal cadence on its own.
    */
  private[serenity] def onWindowFocusChanged(
    focused: Boolean,
    windowFocused: SignallingRef[IO, Boolean],
    cursorVisible: Ref[IO, Boolean],
    breathIndex: Ref[IO, Int],
    requestFastRender: IO[Unit]
  ): IO[Unit] =
    if focused then windowFocused.set(true)
    else windowFocused.set(false) >> resetCursorActivity(cursorVisible, breathIndex) >> requestFastRender

  /** The idle loop's per-tick wait: the normal cursor idle cadence while the window is focused, or an indefinite,
    * wakeup-free wait otherwise -- the mechanism that actually stops idle wakeups, rather than merely skipping the
    * render they'd otherwise trigger. Two things can make focused waiting indefinite instead of cadenced:
    * [[cursorIdleInterval]] returning `None` (motion disabled, or #1170's TUI-blink caret delegation), racing here
    * against [[Stream.interruptWhen]]'s `fastModeSignal` in [[idleRenderPhase]] so a real input event still wakes it
    * immediately -- and losing focus entirely, which waits on `windowFocused` turning true again instead.
    */
  private[serenity] def awaitFocusedIdleTick(
    loadState: IO[AppState],
    windowFocused: SignallingRef[IO, Boolean]
  ): IO[Unit] =
    windowFocused.get.flatMap {
      case true =>
        loadState.flatMap { state =>
          cursorIdleInterval(state.persisted.config, state.runtime.isTuiMode) match
            case Some(interval) => IO.sleep(interval)
            case None           => IO.never
        }
      case false =>
        windowFocused.discrete.find(identity).compile.drain
    }

  /** The fast phase may stand down once nothing is animating and no fresh damage arrived while it was running --
    * `pendingDamage` is drained to `Damage.Nothing` when the phase starts, so any non-`Nothing` value here means
    * `emitDamage` was called again since, and the phase should carry straight on rather than idle even one tick.
    */
  private[serenity] def shouldClearFastMode(stillActive: Boolean, pendingDamage: Damage): Boolean =
    !stillActive && pendingDamage == Damage.Nothing

  final private[serenity] case class AnimationTickCadence(remainderNanos: Long):

    def advance(frameInterval: FiniteDuration): (AnimationTickCadence, Int) =
      // The tick bucket follows the caller's own frame interval (i.e. the configured renderFpsTarget) rather than a
      // fixed 60Hz constant, so animation state advances once per actual paint frame -- lowering render FPS also
      // lowers animation-tick CPU cost instead of ticking internally at 60Hz regardless of paint rate.
      val totalNanos     = remainderNanos + frameInterval.toNanos
      val animationNanos = math.max(1L, frameInterval.toNanos)
      val ticks          = (totalNanos / animationNanos).toInt
      val nextRemainder  = totalNanos % animationNanos
      (AnimationTickCadence(nextRemainder), ticks)

  private[serenity] object AnimationTickCadence:
    val empty: AnimationTickCadence = AnimationTickCadence(0L)

  final private[serenity] case class RuntimeFailure(
      loopName: String,
      phase: String,
      diagnostics: String,
      cause: Throwable
  ) extends RuntimeException(s"$loopName failed in phase=$phase; $diagnostics", cause)

  def run(
    initialViewportSize: ViewportSize,
    makeInputHandler: InputRouter[IO, Event] => IO[InputHandler[IO]],
    checkResize: IO[Option[ViewportSize]],
    renderFull: RenderFn,
    renderCursorOnly: RenderFn,
    appConfig: AppConfig,
    makeStateManager: Option[Logger[IO] => IO[StateManager]] = None,
    awaitExternalQuit: IO[Unit] = IO.never,
    registerResizeCallback: (() => Unit) => Unit = _ => (),
    registerFocusCallback: (Boolean => Unit) => Unit = _ => (),
    registerMarkdownPreviewCloseCallback: (() => Unit) => Unit = _ => (),
    openPath: Option[Path] = None,
    systemClipboard: SystemClipboard[IO] = SystemClipboard.awt[IO],
    isTuiMode: Boolean = false,
    keyboardFidelityTier: KeyboardFidelityTier = KeyboardFidelityTier.Full,
    configNotice: Option[String] = None
  )(using logger: Logger[IO], loggerFactory: LoggerFactory[IO], balance: com.serenity.rope.Balance): IO[Unit] =
    Dispatcher.parallel[IO].use { resizeCallbackDispatcher =>
      for
        _ <- logger.info("Starting Serenity text editor")
        themeManager = com.serenity.ui.theme.config.AppThemeManager.create
        stateManager <- makeStateManager.getOrElse(logger => StateManager.apply(logger, initialConfig = appConfig))(
          logger
        )
        startupTheme <- AppStartup.startupTheme(stateManager, themeManager)
        initialState <- AppStartup.initializeState(
          stateManager,
          startupTheme,
          initialViewportSize,
          appConfig,
          openPath,
          isTuiMode,
          keyboardFidelityTier,
          configNotice
        )
        inputRouter    <- InputRouter.create[IO, Event](new TextEntryTranslator(appConfig))
        inputHandler   <- makeInputHandler(inputRouter)
        _              <- inputRouter.setActiveTranslator(FocusedInputTranslator.forState(initialState))
        fastModeSignal <- SignallingRef.of[IO, Boolean](false)
        pendingDamage  <- Ref.of[IO, Damage](Damage.Nothing)
        // Separate from pendingDamage: that ref answers "should the fast loop keep running," reset once per phase and
        // deliberately blind to the phase's own animation ticks (see shouldClearFastMode). This one answers "what has
        // changed since the last frame was actually drawn," fed by both input events and animation ticks alike, and
        // drained by every render call rather than once per phase -- the render-surface-side accumulator #999 is
        // building keeps this from growing unbounded, since a real render drains it dozens of times a second.
        pendingPaintDamage <- Ref.of[IO, Damage](Damage.Nothing)
        emitDamage = (damage: Damage) =>
          pendingDamage.update(_ |+| damage) >> pendingPaintDamage.update(_ |+| damage) >> fastModeSignal.set(true)
        // The resize/idle-recovery paths don't have a before/after AppState to diff, so they report the coarsest
        // damage rather than none -- inputEventPhase is the one caller that reports real per-event damage.
        requestFastRender = emitDamage(Damage.Everything)
        _             <- IO(registerResizeCallback(resizeCallbackBridge(requestFastRender, resizeCallbackDispatcher)))
        cursorVisible <- Ref.of[IO, Boolean](true)
        breathIndex   <- Ref.of[IO, Int](0)
        windowFocused <- SignallingRef.of[IO, Boolean](true)
        _ <- IO(
          registerFocusCallback(
            focusCallbackBridge(windowFocused, cursorVisible, breathIndex, requestFastRender, resizeCallbackDispatcher)
          )
        )
        _ <- IO(
          registerMarkdownPreviewCloseCallback(
            markdownPreviewCloseCallbackBridge(stateManager, resizeCallbackDispatcher)
          )
        )
        animationTickCadence <- Ref.of[IO, AnimationTickCadence](
          AnimationTickCadence.empty
        )
        currentStateForDiagnostics = stateManager.getCurrentState.map(Some(_))
        checkResizeAndHandle = checkResize.flatMap(RenderController.handleResize(_, stateManager, requestFastRender))
        inputFunnel = inputEventPhase(
          stateManager,
          inputRouter,
          systemClipboard,
          checkResizeAndHandle,
          cursorVisible,
          breathIndex,
          emitDamage
        )
        inputLoop = runInputLoop(stateManager, inputHandler, inputFunnel)
        _ <-
          Resource.make(inputLoop.start)(_.cancel).use { inputFiber =>
            renderFull(initialState, true, None, Damage.Everything, Map.empty) >>
              logger.info("Initial render completed, starting main loop") >>
              {
                val idlePhase = idleRenderPhase(
                  loadState = stateManager.getCurrentState,
                  loadBufferAnimations = stateManager.getBufferAnimations,
                  fastModeSignal = fastModeSignal,
                  windowFocused = windowFocused,
                  pendingPaintDamage = pendingPaintDamage,
                  currentStateForDiagnostics = currentStateForDiagnostics,
                  checkResizeAndHandle = checkResizeAndHandle,
                  cursorVisible = cursorVisible,
                  breathIndex = breathIndex,
                  renderCursorOnly = renderCursorOnly,
                  requestFastRender = requestFastRender
                )

                val fastPhase = fastRenderPhase(
                  stateManager,
                  fastModeSignal,
                  pendingDamage,
                  pendingPaintDamage,
                  animationTickCadence,
                  currentStateForDiagnostics,
                  checkResizeAndHandle,
                  renderFull,
                  renderCursorOnly
                )

                val renderLoop: Stream[IO, Unit] =
                  Stream.repeatEval(IO.unit).flatMap(_ => idlePhase ++ fastPhase)

                runRuntimeLoops(
                  stateManager,
                  inputHandler,
                  inputFiber.joinWithNever,
                  renderLoop,
                  awaitExternalQuit,
                  appConfig
                )
              }
          }
        _ <- logger.info("Serenity editor shutdown complete")
      yield ()
    }

  private def runRuntimeLoops(
    stateManager: StateReader & EventApplier & RuntimeLifecycle & LspEffectSource,
    inputHandler: InputHandler[IO],
    awaitInputLoop: IO[Unit],
    renderLoop: Stream[IO, Unit],
    awaitExternalQuit: IO[Unit],
    appConfig: AppConfig
  )(using logger: Logger[IO]): IO[Unit] =
    val quitSignal = stateManager.awaitQuit.attempt
    (
      awaitInputLoop,
      superviseLoop("render loop", stateManager.forceQuit())(renderLoop.interruptWhen(quitSignal).compile.drain),
      stateManager.awaitQuit,
      superviseLoop("interval save loop", stateManager.forceQuit())(stateManager.intervalSaveStream.compile.drain),
      superviseLoop("external quit coordinator", stateManager.forceQuit())(
        coordinateExternalQuit(awaitExternalQuit, stateManager.forceQuit(), stateManager.awaitQuit)
      ),
      superviseLoop("input shutdown", stateManager.forceQuit())(
        shutdownInputAfterQuit(stateManager.awaitQuit, inputHandler.shutdown)
      ),
      superviseLoop("LSP loop", stateManager.forceQuit())(
        LspManager.run(
          stateManager.lspEffectStream,
          stateManager.applyEvent,
          logger,
          appConfig.languageToolsConfig.lspUserConfig
        )
      )
    ).parMapN((_, _, _, _, _, _, _) => ())

  private def runInputLoop(
    stateManager: StateReader & EventApplier & RuntimeLifecycle,
    inputHandler: InputHandler[IO],
    inputFunnel: Stream[IO, Event] => Stream[IO, Unit]
  )(using logger: Logger[IO]): IO[Unit] =
    val quitSignal = stateManager.awaitQuit.attempt
    superviseLoop("input loop", stateManager.forceQuit())(
      inputHandler.eventStream
        .evalTap(event =>
          stateManager.getCurrentState.flatMap(s => logSelectiveEvents(event, s.persisted.focus, logger))
        )
        .through(inputFunnel)
        .interruptWhen(quitSignal)
        .compile
        .drain
    )

  private[serenity] def coordinateExternalQuit(
    awaitExternalQuit: IO[Unit],
    forceQuit: IO[Unit],
    awaitQuit: IO[Unit]
  ): IO[Unit] =
    IO.race(awaitExternalQuit >> forceQuit, awaitQuit).void

  private[serenity] def shutdownInputAfterQuit(awaitQuit: IO[Unit], shutdownInput: IO[Unit]): IO[Unit] =
    awaitQuit >> shutdownInput

  private[serenity] def idleRenderPhase(
    loadState: IO[AppState],
    loadBufferAnimations: IO[Map[BufferId, com.serenity.animation.AnimationState]],
    fastModeSignal: SignallingRef[IO, Boolean],
    windowFocused: SignallingRef[IO, Boolean],
    pendingPaintDamage: Ref[IO, Damage],
    currentStateForDiagnostics: IO[Option[AppState]],
    checkResizeAndHandle: IO[Unit],
    cursorVisible: Ref[IO, Boolean],
    breathIndex: Ref[IO, Int],
    renderCursorOnly: RenderFn,
    requestFastRender: IO[Unit]
  )(using Logger[IO]): Stream[IO, Unit] =
    Stream
      .repeatEval(awaitFocusedIdleTick(loadState, windowFocused))
      .interruptWhen(fastModeSignal.discrete)
      .evalMap(_ =>
        runIdleRenderStep(
          currentStateForDiagnostics,
          loadState,
          loadBufferAnimations,
          pendingPaintDamage,
          checkResizeAndHandle,
          cursorVisible,
          breathIndex,
          renderCursorOnly,
          requestFastRender
        )
      )

  private[serenity] def inputEventPhase(
    stateManager: StateReader & StateUpdater & EventApplier,
    inputRouter: InputRouter[IO, Event],
    systemClipboard: SystemClipboard[IO],
    checkResizeAndHandle: IO[Unit],
    cursorVisible: Ref[IO, Boolean],
    breathIndex: Ref[IO, Int],
    emitDamage: Damage => IO[Unit]
  )(using balance: com.serenity.rope.Balance): Stream[IO, Event] => Stream[IO, Unit] =
    _.evalMap { event =>
      for
        before           <- stateManager.getCurrentState
        beforeAnimations <- stateManager.getBufferAnimations
        _ <-
          checkResizeBeforeInput(event, checkResizeAndHandle) >>
            ClipboardEventSync.beforeEvent(event, stateManager, systemClipboard) >>
            observeWindowSitterTyping(event, stateManager) >>
            stateManager.applyEvent(event) >>
            ClipboardEventSync.afterEvent(event, stateManager, systemClipboard) >>
            refreshFocusedInputTranslator(stateManager, inputRouter) >>
            resetCursorActivity(cursorVisible, breathIndex)
        after           <- stateManager.getCurrentState
        afterAnimations <- stateManager.getBufferAnimations
        _               <- emitDamage(DamageProducer.forTransition(before, after, beforeAnimations, afterAnimations))
      yield ()
    }.drain

  private[serenity] def observeWindowSitterTyping(
    event: Event,
    stateManager: StateUpdater
  ): IO[Unit] =
    event match
      case _: com.serenity.keystroke.events.InsertChar =>
        stateManager.updateState { state =>
          val motion =
            state.persisted.config.surfaceConfig.effectiveMotionConfiguration.family(MotionFamily.UiTransitions)
          if motion.enabled && state.persisted.config.windowSitterConfig.enabled then
            state.copy(runtime =
              state.runtime.copy(windowSitter =
                state.runtime.windowSitter.observeTyping(System.nanoTime(), state.persisted.config.windowSitterConfig)
              )
            )
          else state
        }
      case _ => IO.unit

  private def checkResizeBeforeInput(event: Event, checkResizeAndHandle: IO[Unit]): IO[Unit] =
    event match
      case _: com.serenity.keystroke.events.MouseInputEvent => checkResizeAndHandle
      case _                                                => IO.unit

  private def refreshFocusedInputTranslator(
    stateManager: StateReader,
    inputRouter: InputRouter[IO, Event]
  ): IO[Unit] =
    stateManager.getCurrentState.flatMap(state =>
      inputRouter.setActiveTranslator(FocusedInputTranslator.forState(state))
    )

  private[serenity] def fastRenderPhase(
    stateManager: StateReader & AnimationTicker,
    fastModeSignal: SignallingRef[IO, Boolean],
    pendingDamage: Ref[IO, Damage],
    pendingPaintDamage: Ref[IO, Damage],
    animationTickCadence: Ref[IO, AnimationTickCadence],
    currentStateForDiagnostics: IO[Option[AppState]],
    checkResizeAndHandle: IO[Unit],
    renderFull: RenderFn,
    renderCursorOnly: RenderFn,
    sleep: FiniteDuration => IO[Unit] = IO.sleep
  )(using logger: Logger[IO], balance: com.serenity.rope.Balance): Stream[IO, Unit] =
    Stream.eval(pendingDamage.getAndSet(Damage.Nothing)).flatMap { _ =>
      Stream
        .repeatEval(stateManager.getCurrentState)
        .zipWithIndex
        .evalMap {
          case (stateAtFrameStart, frameIndex) =>
            for
              isInitialFrame <- IO.pure(frameIndex == 0L)
              interval <- IO.pure(fastFrameInterval(stateAtFrameStart.persisted.config.surfaceConfig.renderFpsTarget))
              _        <- sleep(fastFrameDelay(interval, isInitialFrame))
              _ <- withRuntimeDiagnostics("render loop", "fast.resize", currentStateForDiagnostics)(
                checkResizeAndHandle
              )
              active <-
                if isInitialFrame then
                  for
                    initialState     <- stateManager.getCurrentState
                    bufferAnimations <- stateManager.getBufferAnimations
                  yield hasActiveAnimations(initialState, bufferAnimations)
                else
                  animationTickCadence.modify(_.advance(interval)).flatMap { animationTicks =>
                    withRuntimeDiagnostics("render loop", "fast.animation-tick", currentStateForDiagnostics)(
                      advanceAnimationsForCadence(animationTicks, stateManager, pendingPaintDamage)
                    )
                  }
              state <- withRuntimeDiagnostics("render loop", "fast.state", currentStateForDiagnostics)(
                stateManager.getCurrentState
              )
              bufferAnimations <- stateManager.getBufferAnimations
              paintDamage      <- pendingPaintDamage.getAndSet(Damage.Nothing)
              _ <-
                if canStandDownToCursorOnly(state, bufferAnimations, paintDamage) then
                  withRuntimeDiagnostics("render loop", "fast.cursor-only-render", IO.pure(Some(state)))(
                    renderCursorOnly(state, true, None, paintDamage, bufferAnimations)
                  )
                else
                  withRuntimeDiagnostics("render loop", "fast.full-render", IO.pure(Some(state)))(
                    renderFull(state, true, None, paintDamage, bufferAnimations)
                  )
            yield active
        }
        .takeWhile(identity)
        .map(_ => ())
        .onFinalize {
          stateManager.getCurrentState.flatMap { state =>
            stateManager.getBufferAnimations.flatMap { bufferAnimations =>
              pendingDamage.get.flatMap { damage =>
                if shouldClearFastMode(hasActiveAnimations(state, bufferAnimations), damage) then
                  fastModeSignal.set(false)
                else IO.unit
              }
            }
          }
        }
    }

  private[serenity] def withRuntimeDiagnostics[A](
    loopName: String,
    phase: String,
    stateForDiagnostics: IO[Option[AppState]]
  )(effect: IO[A])(using logger: Logger[IO]): IO[A] =
    Trace.timed(s"$loopName.$phase") {
      effect.handleErrorWith {
        case failure: RuntimeFailure =>
          IO.raiseError(failure)
        case error =>
          stateForDiagnostics.attempt.flatMap {
            case Right(Some(state)) =>
              IO.raiseError(RuntimeFailure(loopName, phase, describeStateForDiagnostics(state), error))
            case Right(None) =>
              IO.raiseError(RuntimeFailure(loopName, phase, "state=unavailable", error))
            case Left(stateError) =>
              val reason = Option(stateError.getMessage).getOrElse(stateError.getClass.getSimpleName)
              IO.raiseError(RuntimeFailure(loopName, phase, s"state=unavailable reason=$reason", error))
          }
      }
    }

  private[serenity] def superviseLoop(
    name: String,
    forceQuit: IO[Unit]
  )(effect: IO[Unit])(using logger: Logger[IO]): IO[Unit] =
    effect.handleErrorWith { error =>
      val (phase, diagnostics, loggedError) = error match
        case RuntimeFailure(_, failedPhase, failureDiagnostics, cause) =>
          (s" phase=$failedPhase", s"; $failureDiagnostics", cause)
        case other =>
          ("", "", other)
      logger.error(loggedError)(s"[RUNTIME] $name failed$phase$diagnostics; forcing safe shutdown") >>
        forceQuit.attempt.void
    }

  private[serenity] def computeIdleCursorFrame(
    state: AppState,
    cursorVisible: Ref[IO, Boolean],
    breathIndex: Ref[IO, Int]
  ): IO[(Boolean, Option[Color])] =
    state.persisted.config.cursorMode match
      case CursorMode.Blink =>
        cursorVisible.updateAndGet(!_).map(vis => (vis, None))
      case CursorMode.Breathe =>
        for
          i <- breathIndex.updateAndGet(i => (i + 1) % 48)
          c     = state.persisted.config.cursorColors.activeOr(state.persisted.theme.cursor)
          alpha = ((math.sin(i * math.Pi / 24) + 1.0) / 2.0 * 255).toInt
        yield (true, Some(c.withAlpha(alpha)))

  private[serenity] def recoverIdleCursorRenderFailure(
    error: Throwable,
    requestFastRender: IO[Unit]
  )(using logger: Logger[IO]): IO[Unit] =
    val (phase, diagnostics, cause) = error match
      case RuntimeFailure(_, failedPhase, failureDiagnostics, failureCause) =>
        (failedPhase, failureDiagnostics, failureCause)
      case other =>
        ("idle.cursor-render", "state=unavailable", other)
    logger.warn(cause)(
      s"[RUNTIME] idle cursor render failed phase=$phase; $diagnostics; requesting full render"
    ) >> requestFastRender

  private[serenity] def runIdleRenderStep(
    currentStateForDiagnostics: IO[Option[AppState]],
    loadState: IO[AppState],
    loadBufferAnimations: IO[Map[BufferId, com.serenity.animation.AnimationState]],
    pendingPaintDamage: Ref[IO, Damage],
    checkResizeAndHandle: IO[Unit],
    cursorVisible: Ref[IO, Boolean],
    breathIndex: Ref[IO, Int],
    renderCursorOnly: RenderFn,
    requestFastRender: IO[Unit]
  )(using logger: Logger[IO]): IO[Unit] =
    for
      _ <- withRuntimeDiagnostics("render loop", "idle.resize", currentStateForDiagnostics)(
        checkResizeAndHandle
      )
      state <- withRuntimeDiagnostics("render loop", "idle.state", currentStateForDiagnostics)(
        loadState
      )
      _ <- cursorIdleInterval(state.persisted.config, state.runtime.isTuiMode) match
        case Some(_) =>
          for
            (visible, cursor) <- withRuntimeDiagnostics(
              "render loop",
              "idle.cursor",
              IO.pure(Some(state))
            )(computeIdleCursorFrame(state, cursorVisible, breathIndex))
            // Read without draining: this frame paints the cursor overlay, never content, so consuming content
            // damage here would lose it -- an input event that lands just as an idle tick fires would have its
            // glyphs dropped until something else damaged the same rows. The fast phase that same event wakes
            // drains it instead.
            paintDamage      <- pendingPaintDamage.get
            bufferAnimations <- loadBufferAnimations
            _ <- withRuntimeDiagnostics(
              "render loop",
              "idle.cursor-render",
              IO.pure(Some(state))
            )(renderCursorOnly(state, visible, cursor, paintDamage, bufferAnimations))
              .handleErrorWith(recoverIdleCursorRenderFailure(_, requestFastRender))
          yield ()
        case None =>
          IO.unit
    yield ()

  private[serenity] def resizeCallbackBridge(
    signalResize: IO[Unit],
    dispatcher: Dispatcher[IO]
  )(using logger: Logger[IO]): () => Unit =
    () =>
      dispatcher.unsafeRunAndForget(
        signalResize.handleErrorWith(error => logger.error(error)("[RUNTIME] resize callback failed"))
      )

  private[serenity] def focusCallbackBridge(
    windowFocused: SignallingRef[IO, Boolean],
    cursorVisible: Ref[IO, Boolean],
    breathIndex: Ref[IO, Int],
    requestFastRender: IO[Unit],
    dispatcher: Dispatcher[IO]
  )(using logger: Logger[IO]): Boolean => Unit =
    focused =>
      dispatcher.unsafeRunAndForget(
        onWindowFocusChanged(focused, windowFocused, cursorVisible, breathIndex, requestFastRender)
          .handleErrorWith(error => logger.error(error)("[RUNTIME] focus callback failed"))
      )

  /** Bridges the TUI's spawned Markdown preview window (issue #1113) closing via its own native close control back into
    * application state: the window only hides itself (see `MarkdownPreviewWindow.resource`), so this callback's sole
    * job is toggling `markdownPreviewWindowBuffer` back off rather than orphaning a dead window reference.
    */
  private[serenity] def markdownPreviewCloseCallbackBridge(
    stateManager: StateUpdater,
    dispatcher: Dispatcher[IO]
  )(using logger: Logger[IO]): () => Unit =
    () =>
      dispatcher.unsafeRunAndForget(
        stateManager
          .updateState(closeMarkdownPreviewWindowInState)
          .handleErrorWith(error => logger.error(error)("[RUNTIME] markdown preview close callback failed"))
      )

  private[serenity] def closeMarkdownPreviewWindowInState(state: AppState): AppState =
    state.copy(runtime = state.runtime.copy(markdownPreviewWindowBuffer = None))

  private def advanceAnimationsForCadence(
    ticks: Int,
    stateManager: StateReader & AnimationTicker,
    pendingPaintDamage: Ref[IO, Damage]
  )(using balance: com.serenity.rope.Balance): IO[Boolean] =
    if ticks <= 0 then
      for
        state            <- stateManager.getCurrentState
        bufferAnimations <- stateManager.getBufferAnimations
      yield hasActiveAnimations(state, bufferAnimations)
    else
      for
        before           <- stateManager.getCurrentState
        beforeAnimations <- stateManager.getBufferAnimations
        stillActive <- (0 until ticks).toList.foldLeft(IO.pure(false)) { (previous, _) =>
          previous.flatMap(_ => stateManager.advanceAnimationsOnTick())
        }
        after           <- stateManager.getCurrentState
        afterAnimations <- stateManager.getBufferAnimations
        _ <- pendingPaintDamage.update(
          _ |+| DamageProducer.forTransition(before, after, beforeAnimations, afterAnimations)
        )
      yield stillActive

  private[serenity] def hasActiveAnimations(
    state: AppState,
    bufferAnimations: Map[BufferId, com.serenity.animation.AnimationState]
  ): Boolean =
    needsFullContentRender(state, bufferAnimations) || state.runtime.windowSitter.isActive

  /** Whether the fast render loop's current frame needs a full content repaint, as opposed to the cheaper cursor-only
    * overlay path. Character-reveal animations paint into document glyphs, and a theme transition cross-fades every
    * visible glyph/background colour (see Renderer.withEffectiveTheme) -- both require the full canvas. Surface
    * animations (command palette, panel fades) are drawn through the same overlay-scene machinery as full renders, not
    * the cursor-only path, so they need it too. The window sitter is the one exception: its glyph lives entirely in the
    * window chrome (SwingWindow.updateWindowSitter, driven by syncChromeTheme, which runs before either render path
    * every frame regardless) and never touches the canvas -- so it alone keeps the fast loop running (see
    * hasActiveAnimations) without forcing a full repaint each frame.
    */
  private[serenity] def needsFullContentRender(
    state: AppState,
    bufferAnimations: Map[BufferId, com.serenity.animation.AnimationState]
  ): Boolean =
    state.persisted.buffers.keys.exists(id => bufferAnimations.get(id).exists(_.hasActiveAnimations)) ||
      state.runtime.themeTransition.isDefined ||
      state.runtime.surfaceAnimations.nonEmpty

  /** Whether this frame may take the cheap cursor-only path instead of repainting content. The window sitter's glyph
    * lives in the window chrome and needs no canvas work, so a frame that only advances it can stand down -- but only
    * when the frame has nothing else to paint. Typing arms the sitter for its whole activity window
    * (`WindowSitterConfig.activeTicks`), so gating on the sitter alone held every keystroke's own glyphs and wrapped
    * reflow off screen until that window expired: the cursor moved immediately and the text caught up a fraction of a
    * second later. Any pending paint damage means content changed, and content is exactly what `renderCursorOnly` does
    * not draw.
    */
  private[serenity] def canStandDownToCursorOnly(
    state: AppState,
    bufferAnimations: Map[BufferId, com.serenity.animation.AnimationState],
    paintDamage: Damage
  ): Boolean =
    paintDamage == Damage.Nothing &&
      state.runtime.windowSitter.isActive &&
      !needsFullContentRender(state, bufferAnimations)

  private def logSelectiveEvents(
    event: Event,
    currentFocus: Focus,
    logger: Logger[IO]
  ): IO[Unit] =
    event match
      case unhandled: UnhandledEvent[?] if !isSystemEvent(unhandled) =>
        logger.warn(s"[UNHANDLED] $event")
      case _: UnhandledEvent[?] =>
        logger.debug(s"[SYSTEM] $event")
      case _ if shouldLogFocusChange(event) =>
        logger.debug(s"[FOCUS] Event: $event, Focus: $currentFocus")
      case _ => IO.unit

  private def shouldLogFocusChange(event: Event): Boolean =
    event match
      case com.serenity.keystroke.events.MoveUp         => false
      case com.serenity.keystroke.events.MoveDown       => false
      case com.serenity.keystroke.events.MoveLeft       => false
      case com.serenity.keystroke.events.MoveRight      => false
      case com.serenity.keystroke.events.InsertChar(_)  => false
      case com.serenity.keystroke.events.DeleteBackward => false
      case com.serenity.keystroke.events.DeleteForward  => false
      case _                                            => true

  private def isSystemEvent(event: UnhandledEvent[?]): Boolean =
    import com.serenity.keystroke.InputKey
    event.info.keyType match
      case InputKey.EOF     => false
      case InputKey.Unknown => true
      case InputKey.Character =>
        event.info.character.exists { char =>
          char.toInt == 0 ||
          char.toInt == 4 ||
          char.toInt == 26
        }
      case _ => false

  private[serenity] def describeStateForDiagnostics(state: AppState): String =
    val viewport   = state.runtime.viewportSize.map(size => s"${size.width}x${size.height}").getOrElse("unknown")
    val activePane = state.persisted.layout.activeEditorPaneId
    val activeBuffer =
      activePane.flatMap(paneId =>
        state.persisted.layout.editorPanes.get(paneId).flatMap(_.bufferId).flatMap(state.persisted.buffers.get)
      )
    val activeBufferSummary = activeBuffer match
      case Some(buffer) =>
        val language = buffer.document.language.map(_.id).getOrElse("plaintext")
        val cursor   = buffer.editing.cursors.headOption.map(c => s"${c.line}:${c.column}").getOrElse("none")
        List(
          s"activeBuffer=${buffer.id}",
          s"chars=${buffer.document.content.weight}",
          s"lines=${buffer.document.content.lineCount}",
          s"dirty=${buffer.document.isDirty}",
          s"language=$language",
          s"cursor=$cursor"
        ).mkString(" ")
      case None =>
        "activeBuffer=none"
    List(
      s"focus=${state.persisted.focus}",
      s"viewport=$viewport",
      s"buffers=${state.persisted.buffers.size}",
      s"panes=${state.persisted.layout.editorPanes.size}",
      s"surfaces=${state.runtime.uiSurfaces.size}",
      s"activePane=${activePane.map(_.toString).getOrElse("none")}",
      activeBufferSummary,
      s"themeTransition=${state.runtime.themeTransition.isDefined}",
      s"surfaceAnimations=${state.runtime.surfaceAnimations.size}"
    ).mkString(" ")
