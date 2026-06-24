package com.serenity.app

import java.awt.Color

import scala.concurrent.duration.*

import cats.effect.*
import cats.effect.std.Dispatcher
import cats.syntax.parallel.*
import com.serenity.config.{AppConfig, CursorMode}
import com.serenity.input.*
import com.serenity.keystroke.events.*
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
    registerResizeCallback: (() => Unit) => Unit = _ => ()
  )(using logger: Logger[IO], loggerFactory: LoggerFactory[IO], balance: com.serenity.rope.Balance): IO[Unit] =
    Dispatcher.parallel[IO].use { resizeCallbackDispatcher =>
      for
        _ <- logger.info("Starting Serenity text editor")
        themeManager = com.serenity.ui.theme.config.AppThemeManager.create
        defaultTheme <- themeManager.initializeWithTheme()
        stateManager <- makeStateManager.getOrElse(logger => StateManager.apply(logger, initialConfig = appConfig))(
          logger
        )
        initialState <- AppStartup.initializeState(stateManager, defaultTheme, initialViewportSize, appConfig)
        inputRouter  <- InputRouter.create[IO, Event](new TextEntryTranslator(appConfig))
        systemClipboard = SystemClipboard.awt[IO]
        inputHandler    = makeInputHandler(inputRouter)
        _             <- inputRouter.setActiveTranslator(FocusedInputTranslator.forState(initialState))
        _             <- renderFull(initialState, true, None)
        _             <- logger.info("Initial render completed, starting main loop")
        fastMode      <- SignallingRef.of[IO, Boolean](false)
        _             <- IO(registerResizeCallback(resizeCallbackBridge(fastMode.set(true), resizeCallbackDispatcher)))
        cursorVisible <- Ref.of[IO, Boolean](true)
        breathIndex   <- Ref.of[IO, Int](0)
        checkResizeAndHandle = checkResize.flatMap(RenderController.handleResize(_, stateManager, fastMode.set(true)))
        inputFunnel = (s: Stream[IO, Event]) =>
          s.evalMap(event =>
            checkResizeAndHandle >>
              signalGestureRenderBeforeHandling(event, fastMode.set(true)) >>
              ClipboardEventSync.beforeEvent(event, stateManager, systemClipboard) >>
              stateManager.applyEvent(event) >>
              ClipboardEventSync.afterEvent(event, stateManager, systemClipboard) >>
              stateManager.getCurrentState
                .flatMap(state => inputRouter.setActiveTranslator(FocusedInputTranslator.forState(state))) >>
              fastMode.set(true)
          ).drain
        _ <-
          def withRuntimeDiagnostics[A](
            loopName: String,
            phase: String,
            stateForDiagnostics: IO[Option[AppState]] = stateManager.getCurrentState.map(Some(_))
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

          def computeCursorForIdle(state: AppState): IO[(Boolean, Option[Color])] =
            state.config.cursorMode match
              case CursorMode.Blink =>
                cursorVisible.updateAndGet(!_).map(vis => (vis, None))
              case CursorMode.Breathe =>
                for
                  i <- breathIndex.updateAndGet(i => (i + 1) % 48)
                  c     = state.config.cursorColors.activeOr(state.theme.cursor)
                  alpha = ((math.sin(i * math.Pi / 24) + 1.0) / 2.0 * 255).toInt
                yield (true, Some(new Color(c.getRed, c.getGreen, c.getBlue, alpha)))

          def recoverIdleCursorRenderFailure(error: Throwable): IO[Unit] =
            val (phase, diagnostics, cause) = error match
              case RuntimeFailure(_, failedPhase, failureDiagnostics, failureCause) =>
                (failedPhase, failureDiagnostics, failureCause)
              case other =>
                ("idle.cursor-render", "state=unavailable", other)
            logger.warn(cause)(
              s"[RUNTIME] idle cursor render failed phase=$phase; $diagnostics; requesting full render"
            ) >> fastMode.set(true)

          def idlePhase: Stream[IO, Unit] =
            Stream
              .fixedRate[IO](500.millis)
              .interruptWhen(fastMode.discrete)
              .evalMap { _ =>
                for
                  _     <- withRuntimeDiagnostics("render loop", "idle.resize")(checkResizeAndHandle)
                  state <- withRuntimeDiagnostics("render loop", "idle.state")(stateManager.getCurrentState)
                  (visible, cursor) <- withRuntimeDiagnostics(
                    "render loop",
                    "idle.cursor",
                    IO.pure(Some(state))
                  )(computeCursorForIdle(state))
                  _ <- withRuntimeDiagnostics(
                    "render loop",
                    "idle.cursor-render",
                    IO.pure(Some(state))
                  )(renderCursorOnly(state, visible, cursor)).handleErrorWith(recoverIdleCursorRenderFailure)
                yield ()
              }

          def fastPhase: Stream[IO, Unit] =
            Stream
              .fixedRate[IO](16.millis)
              .evalMap { _ =>
                for
                  _ <- withRuntimeDiagnostics("render loop", "fast.resize")(checkResizeAndHandle)
                  active <- withRuntimeDiagnostics(
                    "render loop",
                    "fast.animation-tick"
                  )(stateManager.advanceAnimationsOnTick())
                  state <- withRuntimeDiagnostics("render loop", "fast.state")(stateManager.getCurrentState)
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
                  val stillActive =
                    state.buffers.values.exists(_.animations.hasActiveAnimations) ||
                      state.themeTransition.isDefined ||
                      state.surfaceAnimations.nonEmpty
                  if stillActive then IO.unit else fastMode.set(false)
                }
              }

          val renderLoop: Stream[IO, Unit] =
            Stream.repeatEval(IO.unit).flatMap(_ => idlePhase ++ fastPhase)

          val quitSignal = stateManager.awaitQuit.attempt
          val shutdownInputHandler =
            stateManager.awaitQuit >> inputHandler.shutdown
          def supervised(name: String)(effect: IO[Unit]): IO[Unit] =
            effect.handleErrorWith { error =>
              val (phase, diagnostics, loggedError) = error match
                case RuntimeFailure(_, failedPhase, failureDiagnostics, cause) =>
                  (s" phase=$failedPhase", s"; $failureDiagnostics", cause)
                case other =>
                  ("", "", other)
              logger.error(loggedError)(s"[RUNTIME] $name failed$phase$diagnostics; forcing safe shutdown") >>
                stateManager.forceQuit().attempt.void
            }

          (
            supervised("input loop")(
              inputHandler.eventStream
                .evalTap(event => stateManager.getCurrentState.flatMap(s => logSelectiveEvents(event, s.focus, logger)))
                .through(inputFunnel)
                .interruptWhen(quitSignal)
                .compile
                .drain
            ),
            supervised("render loop")(renderLoop.interruptWhen(quitSignal).compile.drain),
            stateManager.awaitQuit,
            supervised("interval save loop")(stateManager.intervalSaveStream.compile.drain),
            supervised("external quit coordinator")(
              IO.race(
                awaitExternalQuit >> stateManager.forceQuit(),
                stateManager.awaitQuit
              ).void
            ),
            supervised("input shutdown")(shutdownInputHandler),
            supervised("LSP loop")(
              LspManager.run(stateManager.lspEffectStream, stateManager.applyEvent, logger, appConfig.lspUserConfig)
            )
          ).parMapN((_, _, _, _, _, _, _) => ())
        _ <- logger.info("Serenity editor shutdown complete")
      yield ()
    }

  private[serenity] def resizeCallbackBridge(
    signalResize: IO[Unit],
    dispatcher: Dispatcher[IO]
  )(using logger: Logger[IO]): () => Unit =
    () =>
      dispatcher.unsafeRunAndForget(
        signalResize.handleErrorWith(error => logger.error(error)("[RUNTIME] resize callback failed"))
      )

  private[serenity] def signalGestureRenderBeforeHandling(event: Event, signalFastRender: IO[Unit]): IO[Unit] =
    event match
      case _: MouseDrag | _: MouseMove | _: MousePress => signalFastRender
      case _                                           => IO.unit

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
