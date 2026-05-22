import scala.concurrent.duration.*

import cats.effect.{IO, IOApp, Ref, Resource}
import cats.syntax.parallel.*
import com.googlecode.lanterna.screen.{Screen, TerminalScreen}
import com.googlecode.lanterna.terminal.{DefaultTerminalFactory, Terminal}
import com.serenity.input.{InputRouter, ScreenInputHandler}
import com.serenity.keystroke.events.{Event, TextEntryEvent, UnhandledEvent}
import com.serenity.keystroke.translators.TextEntryTranslator
import com.serenity.rope.Balance
import com.serenity.state.manager.StateManager
import com.serenity.state.models.Focus
import com.serenity.ui.layout.TerminalSize
import com.serenity.ui.renderer.{RenderController, Renderer}
import com.serenity.ui.theme.config.AppThemeManager
import fs2.Stream
import fs2.concurrent.SignallingRef
import org.typelevel.log4cats.slf4j.Slf4jFactory
import org.typelevel.log4cats.{LoggerFactory, LoggerName}

given Balance = Balance.default

object Main extends IOApp.Simple:

  given LoggerFactory[IO] = Slf4jFactory.create[IO]

  def run: IO[Unit] =
    val logger = LoggerFactory[IO].getLogger(using LoggerName("Main"))
    terminalResource.use { terminal =>
      screenResource(terminal).use { screen =>
        for
          _            <- logger.info("Starting Serenity text editor")
          themeManager  = AppThemeManager.create
          defaultTheme <- themeManager.initializeWithTheme()
          stateManager <- StateManager.apply(logger)
          bufferId     <- stateManager.createNewEmptyBuffer()
          paneId       <- stateManager.createPane(Some(bufferId))
          _            <- stateManager.updateState(_.copy(theme = defaultTheme))
          inputRouter  <- InputRouter.create[IO, TextEntryEvent](new TextEntryTranslator)
          inputHandler  = new ScreenInputHandler[IO, TextEntryEvent](screen, inputRouter)
          initialState <- stateManager.getCurrentState
          _            <- IO.blocking(Renderer.render(initialState, cursorVisible = true, screen))
          _            <- logger.info("Initial render completed, starting main loop")
          fastMode     <- SignallingRef.of[IO, Boolean](false)
          cursorVisible <- Ref.of[IO, Boolean](true)
          checkResize   = IO.blocking(Option(screen.doResizeIfNecessary()))
                            .map(_.map(s => TerminalSize(s.getColumns, s.getRows)))
                            .flatMap(RenderController.handleResize(_, stateManager, fastMode.set(true)))
          inputFunnel   = (s: Stream[IO, Event]) =>
                            s.evalMap { event =>
                              stateManager.applyEvent(event) >> fastMode.set(true)
                            }.drain
          _ <- {
            def idlePhase: Stream[IO, Unit] =
              Stream
                .fixedRate[IO](500.millis)
                .interruptWhen(fastMode.discrete)
                .evalMap { _ =>
                  for
                    _       <- checkResize
                    visible <- cursorVisible.updateAndGet(b => !b)
                    state   <- stateManager.getCurrentState
                    _       <- IO.blocking(Renderer.renderCursorOnly(state, visible, screen))
                  yield ()
                }

            def fastPhase: Stream[IO, Unit] =
              Stream
                .fixedRate[IO](16.millis)
                .evalMap { _ =>
                  for
                    _      <- checkResize
                    active <- stateManager.advanceAnimationsOnTick()
                    state  <- stateManager.getCurrentState
                    _      <- IO.blocking(Renderer.render(state, cursorVisible = true, screen))
                  yield active
                }
                .takeWhile(identity)
                .map(_ => ())
                .onFinalize {
                  stateManager.getCurrentState.flatMap { state =>
                    if state.screenAnimations.hasActiveAnimations then IO.unit
                    else fastMode.set(false)
                  }
                }

            def renderLoop: Stream[IO, Unit] = idlePhase ++ fastPhase ++ renderLoop

            (
              inputHandler.eventStream
                .evalTap { event =>
                  stateManager.getCurrentState.flatMap(s => logSelectiveEvents(event, s.focus, logger))
                }
                .through(inputFunnel)
                .compile.drain,
              renderLoop.compile.drain,
              stateManager.awaitQuit
            ).parMapN((_, _, _) => ())
          }
          _ <- logger.info("Serenity editor shutdown complete")
        yield ()
      }
    }

  private def terminalResource: Resource[IO, Terminal] =
    Resource.make(
      IO.blocking {
        val factory  = new DefaultTerminalFactory()
        val terminal = factory.createTerminal()
        terminal.enterPrivateMode()
        terminal
      }
    )(terminal =>
      IO.blocking {
        terminal.exitPrivateMode()
        terminal.close()
      }
    )

  private def screenResource(terminal: Terminal): Resource[IO, Screen] =
    Resource.make(
      IO.blocking {
        val screen = new TerminalScreen(terminal)
        screen.startScreen()
        screen
      }
    )(screen =>
      IO.blocking {
        screen.stopScreen()
      }
    )

  private def logSelectiveEvents(
    event: Event,
    currentFocus: Focus,
    logger: org.typelevel.log4cats.Logger[IO]
  ): IO[Unit] =
    event match
      case _: UnhandledEvent[?] =>
        logger.warn(s"[UNHANDLED] $event")
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
