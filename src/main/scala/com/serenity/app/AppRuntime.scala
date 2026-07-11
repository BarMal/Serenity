package com.serenity.app

import java.awt.Color
import java.nio.file.Path

import scala.concurrent.duration.*

import cats.effect.*
import cats.effect.std.Dispatcher
import cats.syntax.parallel.*
import com.serenity.config.{AppConfig, CursorMode, RenderFpsTarget}
import com.serenity.input.*
import com.serenity.keystroke.events.{Event, UnhandledEvent}
import com.serenity.keystroke.translators.TextEntryTranslator
import com.serenity.lsp.LspManager
import com.serenity.state.manager.StateManager
import com.serenity.state.models.{AppState, Focus}
import com.serenity.ui.layout.ViewportSize
import com.serenity.ui.renderer.RenderController
import fs2.Stream
import fs2.concurrent.SignallingRef
import org.typelevel.log4cats.{Logger, LoggerFactory}

object AppRuntime:

  private val NanosPerSecond: Long                      = 1_000_000_000L
  private val DefaultCursorIdleInterval: FiniteDuration = 500.millis

  private[serenity] def fastFrameInterval(target: RenderFpsTarget): FiniteDuration =
    FiniteDuration(NanosPerSecond / target.framesPerSecond.toLong, NANOSECONDS)

  private[serenity] def cursorIdleInterval(config: AppConfig): Option[FiniteDuration] =
    val scale = AppConfig.clampElementTransitionSpeedScale(config.effectiveCursorTransitionSpeedScale)
    Option.when(scale > 0.0)(
      FiniteDuration(
        math.max(1L, math.round(DefaultCursorIdleInterval.toNanos.toDouble * scale)),
        NANOSECONDS
      )
    )

  private[serenity] def resetCursorActivity(cursorVisible: Ref[IO, Boolean], breathIndex: Ref[IO, Int]): IO[Unit] =
    cursorVisible.set(true) >> breathIndex.set(0)

  private[serenity] def shouldClearFastMode(
    stillActive: Boolean,
    phaseStartRenderRequest: Long,
    currentRenderRequest: Long
  ): Boolean =
    !stillActive && currentRenderRequest == phaseStartRenderRequest

  final private[serenity] case class AnimationTickCadence(remainderNanos: Long):

    def advance(frameInterval: FiniteDuration): (AnimationTickCadence, Int) =
      val totalNanos     = remainderNanos + frameInterval.toNanos
      val animationNanos = fastFrameInterval(RenderFpsTarget.Fps60).toNanos
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
    makeInputHandler: InputRouter[IO, Event] => InputHandler[IO],
    checkResize: IO[Option[ViewportSize]],
    renderFull: (AppState, Boolean, Option[Color]) => IO[Unit],
    renderCursorOnly: (AppState, Boolean, Option[Color]) => IO[Unit],
    appConfig: AppConfig,
    makeStateManager: Option[Logger[IO] => IO[StateManager]] = None,
    awaitExternalQuit: IO[Unit] = IO.never,
    registerResizeCallback: (() => Unit) => Unit = _ => (),
    openPath: Option[Path] = None
  )(using logger: Logger[IO], loggerFactory: LoggerFactory[IO], balance: com.serenity.rope.Balance): IO[Unit] =
    Dispatcher.parallel[IO].use { resizeCallbackDispatcher =>
      for
        _ <- logger.info("Starting Serenity text editor")
        themeManager = com.serenity.ui.theme.config.AppThemeManager.create
        stateManager <- makeStateManager.getOrElse(logger => StateManager.apply(logger, initialConfig = appConfig))(
          logger
        )
        startupTheme <- AppStartup.startupTheme(stateManager, themeManager)
        initialState <- AppStartup.initializeState(stateManager, startupTheme, initialViewportSize, appConfig, openPath)
        inputRouter  <- InputRouter.create[IO, Event](new TextEntryTranslator(appConfig))
        systemClipboard = SystemClipboard.awt[IO]
        inputHandler    = makeInputHandler(inputRouter)
        _                      <- inputRouter.setActiveTranslator(FocusedInputTranslator.forState(initialState))
        _                      <- renderFull(initialState, true, None)
        _                      <- logger.info("Initial render completed, starting main loop")
        fastMode               <- SignallingRef.of[IO, Boolean](false)
        fastRenderRequestEpoch <- Ref.of[IO, Long](0L)
        requestFastRender = fastRenderRequestEpoch.update(_ + 1L) >> fastMode.set(true)
        _             <- IO(registerResizeCallback(resizeCallbackBridge(requestFastRender, resizeCallbackDispatcher)))
        cursorVisible <- Ref.of[IO, Boolean](true)
        breathIndex   <- Ref.of[IO, Int](0)
        animationTickCadence <- Ref.of[IO, AnimationTickCadence](
          AnimationTickCadence.empty
        )
        currentStateForDiagnostics = stateManager.getCurrentState.map(Some(_))
        checkResizeAndHandle = checkResize.flatMap(RenderController.handleResize(_, stateManager, requestFastRender))
        inputFunnel = (s: Stream[IO, Event]) =>
          s.evalMap(event =>
            checkResizeAndHandle >>
              ClipboardEventSync.beforeEvent(event, stateManager, systemClipboard) >>
              stateManager.applyEvent(event) >>
              ClipboardEventSync.afterEvent(event, stateManager, systemClipboard) >>
              stateManager.getCurrentState
                .flatMap(state => inputRouter.setActiveTranslator(FocusedInputTranslator.forState(state))) >>
              resetCursorActivity(cursorVisible, breathIndex) >>
              requestFastRender
          ).drain
        _ <-
          val idlePhase = idleRenderPhase(
            loadState = stateManager.getCurrentState,
            fastMode = fastMode,
            currentStateForDiagnostics = currentStateForDiagnostics,
            checkResizeAndHandle = checkResizeAndHandle,
            cursorVisible = cursorVisible,
            breathIndex = breathIndex,
            renderCursorOnly = renderCursorOnly,
            requestFastRender = requestFastRender
          )

          def fastPhase: Stream[IO, Unit] =
            Stream.eval(fastRenderRequestEpoch.get).flatMap { phaseStartRenderRequest =>
              Stream
                .repeatEval(
                  stateManager.getCurrentState.map(state => fastFrameInterval(state.config.renderFpsTarget))
                )
                .evalMap { interval =>
                  for
                    _ <- IO.sleep(interval)
                    _ <- withRuntimeDiagnostics("render loop", "fast.resize", currentStateForDiagnostics)(
                      checkResizeAndHandle
                    )
                    animationTicks <- animationTickCadence.modify(_.advance(interval))
                    active <- withRuntimeDiagnostics(
                      "render loop",
                      "fast.animation-tick",
                      currentStateForDiagnostics
                    )(advanceAnimationsForCadence(animationTicks, stateManager))
                    state <- withRuntimeDiagnostics("render loop", "fast.state", currentStateForDiagnostics)(
                      stateManager.getCurrentState
                    )
                    _ <- withRuntimeDiagnostics(
                      "render loop",
                      "fast.full-render",
                      IO.pure(Some(state))
                    )(renderFull(state, true, None))
                  yield active
                }
                .takeWhile(identity)
                .map(_ => ())
                .onFinalize {
                  stateManager.getCurrentState.flatMap { state =>
                    val stillActive = hasActiveAnimations(state)
                    fastRenderRequestEpoch.get.flatMap { currentRenderRequest =>
                      if shouldClearFastMode(stillActive, phaseStartRenderRequest, currentRenderRequest) then
                        fastMode.set(false)
                      else IO.unit
                    }
                  }
                }
            }

          val renderLoop: Stream[IO, Unit] =
            Stream.repeatEval(IO.unit).flatMap(_ => idlePhase ++ fastPhase)

          val quitSignal = stateManager.awaitQuit.attempt
          (
            superviseLoop("input loop", stateManager.forceQuit())(
              inputHandler.eventStream
                .evalTap(event => stateManager.getCurrentState.flatMap(s => logSelectiveEvents(event, s.focus, logger)))
                .through(inputFunnel)
                .interruptWhen(quitSignal)
                .compile
                .drain
            ),
            superviseLoop("render loop", stateManager.forceQuit())(renderLoop.interruptWhen(quitSignal).compile.drain),
            stateManager.awaitQuit,
            superviseLoop("interval save loop", stateManager.forceQuit())(
              stateManager.intervalSaveStream.compile.drain
            ),
            superviseLoop("external quit coordinator", stateManager.forceQuit())(
              coordinateExternalQuit(awaitExternalQuit, stateManager.forceQuit(), stateManager.awaitQuit)
            ),
            superviseLoop("input shutdown", stateManager.forceQuit())(
              shutdownInputAfterQuit(stateManager.awaitQuit, inputHandler.shutdown)
            ),
            superviseLoop("LSP loop", stateManager.forceQuit())(
              LspManager.run(stateManager.lspEffectStream, stateManager.applyEvent, logger, appConfig.lspUserConfig)
            )
          ).parMapN((_, _, _, _, _, _, _) => ())
        _ <- logger.info("Serenity editor shutdown complete")
      yield ()
    }

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
    fastMode: SignallingRef[IO, Boolean],
    currentStateForDiagnostics: IO[Option[AppState]],
    checkResizeAndHandle: IO[Unit],
    cursorVisible: Ref[IO, Boolean],
    breathIndex: Ref[IO, Int],
    renderCursorOnly: (AppState, Boolean, Option[Color]) => IO[Unit],
    requestFastRender: IO[Unit]
  )(using Logger[IO]): Stream[IO, Unit] =
    Stream
      .repeatEval(
        loadState
          .map(state => cursorIdleInterval(state.config).getOrElse(DefaultCursorIdleInterval))
          .flatMap(IO.sleep)
      )
      .interruptWhen(fastMode.discrete)
      .evalMap(_ =>
        runIdleRenderStep(
          currentStateForDiagnostics,
          loadState,
          checkResizeAndHandle,
          cursorVisible,
          breathIndex,
          renderCursorOnly,
          requestFastRender
        )
      )

  private[serenity] def withRuntimeDiagnostics[A](
    loopName: String,
    phase: String,
    stateForDiagnostics: IO[Option[AppState]]
  )(effect: IO[A]): IO[A] =
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
    state.config.cursorMode match
      case CursorMode.Blink =>
        cursorVisible.updateAndGet(!_).map(vis => (vis, None))
      case CursorMode.Breathe =>
        for
          i <- breathIndex.updateAndGet(i => (i + 1) % 48)
          c     = state.config.cursorColors.activeOr(state.theme.cursor)
          alpha = ((math.sin(i * math.Pi / 24) + 1.0) / 2.0 * 255).toInt
        yield (true, Some(new Color(c.getRed, c.getGreen, c.getBlue, alpha)))

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
    checkResizeAndHandle: IO[Unit],
    cursorVisible: Ref[IO, Boolean],
    breathIndex: Ref[IO, Int],
    renderCursorOnly: (AppState, Boolean, Option[Color]) => IO[Unit],
    requestFastRender: IO[Unit]
  )(using logger: Logger[IO]): IO[Unit] =
    for
      _ <- withRuntimeDiagnostics("render loop", "idle.resize", currentStateForDiagnostics)(
        checkResizeAndHandle
      )
      state <- withRuntimeDiagnostics("render loop", "idle.state", currentStateForDiagnostics)(
        loadState
      )
      _ <- cursorIdleInterval(state.config) match
        case Some(_) =>
          for
            (visible, cursor) <- withRuntimeDiagnostics(
              "render loop",
              "idle.cursor",
              IO.pure(Some(state))
            )(computeIdleCursorFrame(state, cursorVisible, breathIndex))
            _ <- withRuntimeDiagnostics(
              "render loop",
              "idle.cursor-render",
              IO.pure(Some(state))
            )(renderCursorOnly(state, visible, cursor))
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

  private def advanceAnimationsForCadence(ticks: Int, stateManager: StateManager): IO[Boolean] =
    if ticks <= 0 then stateManager.getCurrentState.map(hasActiveAnimations)
    else
      (0 until ticks).toList.foldLeft(IO.pure(false)) { (previous, _) =>
        previous.flatMap(_ => stateManager.advanceAnimationsOnTick())
      }

  private def hasActiveAnimations(state: AppState): Boolean =
    state.buffers.values.exists(_.animations.hasActiveAnimations) ||
      state.themeTransition.isDefined ||
      state.surfaceAnimations.nonEmpty

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
    val viewport   = state.viewportSize.map(size => s"${size.width}x${size.height}").getOrElse("unknown")
    val activePane = state.layout.activeEditorPaneId
    val activeBuffer =
      activePane.flatMap(paneId => state.layout.editorPanes.get(paneId).flatMap(_.bufferId).flatMap(state.buffers.get))
    val activeBufferSummary = activeBuffer match
      case Some(buffer) =>
        val language = buffer.language.map(_.id).getOrElse("plaintext")
        val cursor   = buffer.cursors.headOption.map(c => s"${c.line}:${c.column}").getOrElse("none")
        List(
          s"activeBuffer=${buffer.id}",
          s"chars=${buffer.content.weight}",
          s"lines=${buffer.content.lineCount}",
          s"dirty=${buffer.isDirty}",
          s"language=$language",
          s"cursor=$cursor",
          s"bufferAnimations=${buffer.animations.hasActiveAnimations}"
        ).mkString(" ")
      case None =>
        "activeBuffer=none"
    List(
      s"focus=${state.focus}",
      s"viewport=$viewport",
      s"buffers=${state.buffers.size}",
      s"panes=${state.layout.editorPanes.size}",
      s"surfaces=${state.uiSurfaces.size}",
      s"activePane=${activePane.map(_.toString).getOrElse("none")}",
      activeBufferSummary,
      s"themeTransition=${state.themeTransition.isDefined}",
      s"surfaceAnimations=${state.surfaceAnimations.size}"
    ).mkString(" ")
