package com.serenity.app

import java.awt.Color
import java.nio.file.Path

import scala.concurrent.duration.*

import cats.effect.*
import cats.effect.std.Dispatcher
import cats.syntax.foldable.*
import cats.syntax.parallel.*
import cats.syntax.semigroup.*
import com.serenity.config.{AppConfig, CursorMode, RenderFpsTarget}
import com.serenity.input.*
import com.serenity.keystroke.KeyboardFidelityTier
import com.serenity.keystroke.events.Event
import com.serenity.keystroke.translators.TextEntryTranslator
import com.serenity.lsp.LspManager
import com.serenity.state.manager.*
import com.serenity.state.models.{AppState, BufferId, Damage}
import com.serenity.ui.layout.ViewportSize
import com.serenity.ui.renderer.RenderController
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
    * (#1170): in TUI blink mode the caret is delegated to the terminal's own cursor
    * (`RendererCursorOverlay.presentHardwareCursor`), which owns blink timing entirely, so the app has no idle work
    * left to do. Breathe mode is the documented exception -- it animates color/opacity over time, which a terminal
    * cursor style can't represent -- so it keeps the normal cadence.
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
    * the idle loop was in its own cadence. Regaining focus flips the signal the idle loop is waiting on --
    * `awaitFocusedIdleTick` picks that up and resumes the normal cadence on its own -- and runs `onFocusGained` (#1623:
    * re-checking the focused buffer's file for external changes), defaulted to a no-op for callers that don't need it
    * (most existing tests).
    */
  private[serenity] def onWindowFocusChanged(
    focused: Boolean,
    windowFocused: SignallingRef[IO, Boolean],
    cursorVisible: Ref[IO, Boolean],
    breathIndex: Ref[IO, Int],
    requestFastRender: IO[Unit],
    onFocusGained: IO[Unit] = IO.unit
  ): IO[Unit] =
    if focused then windowFocused.set(true) >> onFocusGained
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
        startupTheme <- AppStartup.startupTheme(stateManager.sessionStartupInfo, themeManager)
        initialState <- AppStartup.initializeState(
          stateManager,
          stateManager.sessionStartupInfo,
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
            focusCallbackBridge(
              windowFocused,
              cursorVisible,
              breathIndex,
              requestFastRender,
              resizeCallbackDispatcher,
              stateManager.fileService.checkExternalChangesOnFocus
            )
          )
        )
        _ <- IO(
          registerMarkdownPreviewCloseCallback(
            markdownPreviewCloseCallbackBridge(stateManager, resizeCallbackDispatcher)
          )
        )
        animationTickCadence <- Ref.of[IO, AnimationTickCadence](AnimationTickCadence.empty)
        translatorCache      <- Ref.of[IO, Option[AppRuntimeRenderLoops.FocusedTranslatorCacheEntry]](None)
        currentStateForDiagnostics = stateManager.getCurrentState.map(Some(_))
        checkResizeAndHandle = checkResize.flatMap(RenderController.handleResize(_, stateManager, requestFastRender))
        inputFunnel = AppRuntimeRenderLoops.inputEventPhase(
          stateManager,
          inputRouter,
          systemClipboard,
          checkResizeAndHandle,
          cursorVisible,
          breathIndex,
          emitDamage,
          translatorCache
        )
        inputLoop = runInputLoop(stateManager, inputHandler, inputFunnel)
        _ <-
          Resource.make(inputLoop.start)(_.cancel).use { inputFiber =>
            renderFull(initialState, true, None, Damage.Everything, Map.empty) >>
              logger.info("Initial render completed, starting main loop") >>
              {
                val idlePhase = AppRuntimeRenderLoops.idleRenderPhase(
                  loadModel = stateManager.getModel,
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

                val fastPhase = AppRuntimeRenderLoops.fastRenderPhase(
                  stateManager,
                  stateManager.animationTicker,
                  fastModeSignal,
                  pendingDamage,
                  pendingPaintDamage,
                  animationTickCadence,
                  currentStateForDiagnostics,
                  checkResizeAndHandle,
                  renderFull
                )

                val renderLoop: Stream[IO, Unit] =
                  Stream.repeatEval(IO.unit).flatMap(_ => idlePhase ++ fastPhase)

                com.serenity.io.FileChangeWatcher.create.use(watcher =>
                  runRuntimeLoops(
                    stateManager,
                    inputHandler,
                    inputFiber.joinWithNever,
                    renderLoop,
                    watcher,
                    awaitExternalQuit,
                    appConfig
                  )
                )
              }
          }
        _ <- logger.info("Serenity editor shutdown complete")
      yield ()
    }

  private def runRuntimeLoops(
    stateManager: StateManager,
    inputHandler: InputHandler[IO],
    awaitInputLoop: IO[Unit],
    renderLoop: Stream[IO, Unit],
    fileChangeWatcher: com.serenity.io.FileChangeWatcher,
    awaitExternalQuit: IO[Unit],
    appConfig: AppConfig
  )(using logger: Logger[IO]): IO[Unit] =
    val (lifecycle, quitSignal) = (stateManager.runtimeLifecycle, stateManager.runtimeLifecycle.awaitQuit.attempt)
    (
      awaitInputLoop,
      AppRuntimeRenderLoops.superviseLoop("render loop", lifecycle.forceQuit)(
        renderLoop.interruptWhen(quitSignal).compile.drain
      ),
      lifecycle.awaitQuit,
      AppRuntimeRenderLoops.superviseLoop("interval save loop", lifecycle.forceQuit)(
        lifecycle.intervalSaveStream.compile.drain
      ),
      AppRuntimeRenderLoops.superviseLoop("external quit coordinator", lifecycle.forceQuit)(
        coordinateExternalQuit(awaitExternalQuit, lifecycle.forceQuit, lifecycle.awaitQuit)
      ),
      AppRuntimeRenderLoops.superviseLoop("input shutdown", lifecycle.forceQuit)(
        shutdownInputAfterQuit(lifecycle.awaitQuit, inputHandler.shutdown)
      ),
      AppRuntimeRenderLoops.superviseLoop("LSP loop", lifecycle.forceQuit)(
        LspManager.run(
          stateManager.lspEffectSource.lspEffectStream,
          stateManager.applyEvent,
          logger,
          appConfig.languageToolsConfig.lspUserConfig
        )
      ),
      AppRuntimeRenderLoops.superviseLoop("external change watch loop", lifecycle.forceQuit)(
        externalChangeWatchLoop(
          fileChangeWatcher,
          stateManager.fileService.openBufferPaths,
          stateManager.fileService.checkBufferForExternalChanges
        ).interruptWhen(quitSignal).compile.drain
      )
    ).parMapN((_, _, _, _, _, _, _, _) => ())

  /** Background half of external-change detection (#1623), complementing the focus-in re-check: each cycle, re-derives
    * the watched directory set from the currently open local buffers (`FileChangeWatcher.sync` handles buffers
    * opening/closing since the last cycle), polls for real filesystem events, and re-checks every buffer whose file a
    * poll window actually saw change -- reload-or-prompt exactly like the focus-in path, just not gated on the window
    * regaining focus.
    *
    * `WatchService.poll` is a genuine blocking OS call, so it only runs when there is at least one directory to watch
    * -- with nothing open, the cycle sleeps instead. This isn't just an efficiency nicety: a real blocking call left
    * running unconditionally makes this loop, and therefore any `AppRuntime.run` caller, incompatible with a
    * virtual-time test harness (`VirtualTime.runVirtual`'s own `TestControl` treats `IO.blocking` as non-terminating)
    * -- a plain buffer-less startup (the common case every such test starts from) must stay virtual-time-compatible.
    */
  private[serenity] def externalChangeWatchLoop(
    watcher: com.serenity.io.FileChangeWatcher,
    openBufferPaths: IO[Map[Path, BufferId]],
    checkBufferForExternalChanges: BufferId => IO[Unit],
    pollInterval: FiniteDuration = 2.seconds
  ): Stream[IO, Unit] =
    Stream.repeatEval(
      for
        paths <- openBufferPaths
        _     <- watcher.sync(paths.keys.flatMap(path => Option(path.getParent)).toSet)
        _ <-
          if paths.isEmpty then IO.sleep(pollInterval)
          else
            watcher
              .pollChangedFiles(pollInterval)
              .flatMap(changed => changed.flatMap(paths.get).toList.traverse_(checkBufferForExternalChanges))
      yield ()
    )

  private def runInputLoop(
    stateManager: StateManager,
    inputHandler: InputHandler[IO],
    inputFunnel: Stream[IO, Event] => Stream[IO, Unit]
  )(using logger: Logger[IO]): IO[Unit] =
    val quitSignal = stateManager.runtimeLifecycle.awaitQuit.attempt
    AppRuntimeRenderLoops.superviseLoop("input loop", stateManager.runtimeLifecycle.forceQuit)(
      inputHandler.eventStream
        .evalTap(event =>
          stateManager.getCurrentState.flatMap(s =>
            AppRuntimeLogging.logSelectiveEvents(event, s.persisted.focus, logger)
          )
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

  /** Dispatches an AWT/JLine callback's effect onto the runtime, tolerating the dispatcher having already shut down.
    *
    * On quit the runtime's `Dispatcher` closes while native callbacks (WINCH resize, window focus-lost) can still fire.
    * `unsafeRunAndForget` then throws `IllegalStateException: Dispatcher already closed` synchronously -- before the
    * effect's own error handler can run -- onto the AWT/pump thread, where `CrashReporter` logs it as a crash. A
    * callback arriving after shutdown has nothing left to do, so swallow only that specific closed-dispatcher state and
    * let every other failure propagate.
    */
  private def dispatchIfRunning(dispatcher: Dispatcher[IO])(effect: IO[Unit]): Unit =
    try dispatcher.unsafeRunAndForget(effect)
    catch case _: IllegalStateException => ()

  private[serenity] def resizeCallbackBridge(
    signalResize: IO[Unit],
    dispatcher: Dispatcher[IO]
  )(using logger: Logger[IO]): () => Unit =
    () =>
      dispatchIfRunning(dispatcher)(
        signalResize.handleErrorWith(error => logger.error(error)("[RUNTIME] resize callback failed"))
      )

  private[serenity] def focusCallbackBridge(
    windowFocused: SignallingRef[IO, Boolean],
    cursorVisible: Ref[IO, Boolean],
    breathIndex: Ref[IO, Int],
    requestFastRender: IO[Unit],
    dispatcher: Dispatcher[IO],
    onFocusGained: IO[Unit] = IO.unit
  )(using logger: Logger[IO]): Boolean => Unit =
    focused =>
      dispatchIfRunning(dispatcher)(
        onWindowFocusChanged(focused, windowFocused, cursorVisible, breathIndex, requestFastRender, onFocusGained)
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
          .updateStateValidated(closeMarkdownPreviewWindowInState)
          .handleErrorWith(error => logger.error(error)("[RUNTIME] markdown preview close callback failed"))
      )

  private[serenity] def closeMarkdownPreviewWindowInState(state: AppState): AppState =
    state.copy(runtime = state.runtime.copy(markdownPreviewWindowBuffer = None))

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
        val cursor   = buffer.editing.cursorPositions.headOption.map(c => s"${c.line}:${c.column}").getOrElse("none")
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
