package com.serenity.app

import java.awt.Color

import scala.concurrent.duration.*

import cats.effect.*
import cats.syntax.parallel.*
import com.serenity.config.{AppConfig, CursorMode}
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

  def run(
    initialViewportSize: ViewportSize,
    makeInputHandler: InputRouter[IO, Event] => InputHandler[IO],
    checkResize: IO[Option[ViewportSize]],
    renderFull: (AppState, Boolean, Option[Color]) => IO[Unit],
    renderCursorOnly: (AppState, Boolean, Option[Color]) => IO[Unit],
    appConfig: AppConfig,
    makeStateManager: Option[Logger[IO] => IO[StateManager]] = None,
    awaitExternalQuit: IO[Unit] = IO.never,
    registerResizeCallback: IO[Unit] => Unit = _ => ()
  )(using logger: Logger[IO], loggerFactory: LoggerFactory[IO], balance: com.serenity.rope.Balance): IO[Unit] =
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
      _             <- IO(registerResizeCallback(fastMode.set(true)))
      cursorVisible <- Ref.of[IO, Boolean](true)
      breathIndex   <- Ref.of[IO, Int](0)
      checkResizeAndHandle = checkResize.flatMap(RenderController.handleResize(_, stateManager, fastMode.set(true)))
      inputFunnel = (s: Stream[IO, Event]) =>
        s.evalMap(event =>
          checkResizeAndHandle >>
            ClipboardEventSync.beforeEvent(event, stateManager, systemClipboard) >>
            stateManager.applyEvent(event) >>
            ClipboardEventSync.afterEvent(event, stateManager, systemClipboard) >>
            stateManager.getCurrentState
              .flatMap(state => inputRouter.setActiveTranslator(FocusedInputTranslator.forState(state))) >>
            fastMode.set(true)
        ).drain
      _ <-
        def computeCursorForIdle(state: AppState): IO[(Boolean, Option[Color])] =
          state.config.cursorMode match
            case CursorMode.Blink =>
              cursorVisible.updateAndGet(!_).map(vis => (vis, None))
            case CursorMode.Breathe =>
              for
                i <- breathIndex.updateAndGet(i => (i + 1) % 48)
                c     = state.theme.cursor
                alpha = ((math.sin(i * math.Pi / 24) + 1.0) / 2.0 * 255).toInt
              yield (true, Some(new Color(c.getRed, c.getGreen, c.getBlue, alpha)))

        def idlePhase: Stream[IO, Unit] =
          Stream
            .fixedRate[IO](500.millis)
            .interruptWhen(fastMode.discrete)
            .evalMap { _ =>
              for
                _                 <- checkResizeAndHandle
                state             <- stateManager.getCurrentState
                (visible, cursor) <- computeCursorForIdle(state)
                _                 <- renderCursorOnly(state, visible, cursor)
              yield ()
            }

        def fastPhase: Stream[IO, Unit] =
          Stream
            .fixedRate[IO](16.millis)
            .evalMap { _ =>
              for
                _      <- checkResizeAndHandle
                active <- stateManager.advanceAnimationsOnTick()
                state  <- stateManager.getCurrentState
                _      <- renderFull(state, true, None)
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

        def renderLoop: Stream[IO, Unit] = idlePhase ++ fastPhase ++ renderLoop

        val quitSignal = stateManager.awaitQuit.attempt
        val shutdownInputHandler =
          stateManager.awaitQuit >> inputHandler.shutdown
        (
          inputHandler.eventStream
            .evalTap(event => stateManager.getCurrentState.flatMap(s => logSelectiveEvents(event, s.focus, logger)))
            .through(inputFunnel)
            .interruptWhen(quitSignal)
            .compile
            .drain,
          renderLoop.interruptWhen(quitSignal).compile.drain,
          stateManager.awaitQuit,
          stateManager.intervalSaveStream.compile.drain,
          IO.race(
            awaitExternalQuit >> stateManager.forceQuit(),
            stateManager.awaitQuit
          ).void,
          shutdownInputHandler,
          LspManager.run(stateManager.lspEffectStream, stateManager.applyEvent, logger)
        ).parMapN((_, _, _, _, _, _, _) => ())
      _ <- logger.info("Serenity editor shutdown complete")
    yield ()

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
