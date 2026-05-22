import scala.concurrent.duration.*

import cats.effect.{IO, IOApp, Resource}
import cats.syntax.parallel.*
import com.googlecode.lanterna.screen.{Screen, TerminalScreen}
import com.googlecode.lanterna.terminal.{DefaultTerminalFactory, Terminal}
import com.serenity.input.{InputRouter, ScreenInputHandler}
import com.serenity.keystroke.events.{Event, TextEntryEvent, UnhandledEvent}
import com.serenity.keystroke.translators.TextEntryTranslator
import com.serenity.rope.Balance
import com.serenity.state.manager.StateManager
import com.serenity.state.models.Focus
import com.serenity.ui.renderer.Renderer
import com.serenity.ui.theme.config.AppThemeManager
import fs2.Stream
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
          _ <- logger.info("Starting Serenity text editor")
          // Initialize theme manager and load default theme
          themeManager = AppThemeManager.create
          defaultTheme <- themeManager.initializeWithTheme() // Uses "default-dark"
          stateManager <- StateManager.apply(logger)
          // Create initial empty buffer and pane for startup
          bufferId <- stateManager.createNewEmptyBuffer()
          paneId   <- stateManager.createPane(Some(bufferId))
          // Apply the loaded theme to the initial state
          _           <- stateManager.updateState(_.copy(theme = defaultTheme))
          inputRouter <- InputRouter.create[IO, TextEntryEvent](new TextEntryTranslator)
          inputHandler = new ScreenInputHandler[IO, TextEntryEvent](screen, inputRouter)
          // Render initial state before starting input loop
          initialState <- stateManager.getCurrentState
          _            <- IO.blocking(Renderer.render(initialState, screen))
          _            <- logger.info("Initial render completed, starting main loop")
          // Create animation tick stream - 16ms for smooth 60 FPS animation
          animationTickStream = Stream
            .fixedRate[IO](16.millis)
            .evalMap(_ => stateManager.advanceAnimationsOnTick())
          // Create rendering stream - 30 FPS for smooth UI updates
          renderingStream = Stream
            .fixedRate[IO]((1000.0 / 30).millis)
            .evalMap(_ =>
              for
                state <- stateManager.getCurrentState
                _     <- IO.blocking(Renderer.render(state, screen))
              yield ()
            )
          // Race the main event loop, animation ticks, rendering, and quit signal
          _ <- (
            // Input event processing
            inputHandler.eventStream
              .evalMap { event =>
                for
                  _            <- stateManager.applyEvent(event)
                  activeBuffer <- stateManager.getActiveBuffer
                  state        <- stateManager.getCurrentState
                  _            <- logSelectiveEvents(event, state.focus, logger)
                yield ()
              }
              .compile
              .drain,
            // 16ms animation tick stream
            animationTickStream.compile.drain,
            // 30 FPS rendering stream
            renderingStream.compile.drain,
            stateManager.awaitQuit
          ).parMapN((_, _, _, _) => ())
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

  /** Log only focus changes and unregistered events */
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
      case _                                            => true // Log other events that might change focus/handlers
