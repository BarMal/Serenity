import cats.effect.{IO, IOApp, Resource}
import cats.syntax.parallel.*
import com.googlecode.lanterna.screen.{Screen, TerminalScreen}
import com.googlecode.lanterna.terminal.{DefaultTerminalFactory, Terminal}
import com.serenity.input.{InputRouter, ScreenInputHandler}
import com.serenity.keystroke.events.TextEntryEvent
import com.serenity.keystroke.translators.TextEntryTranslator
import com.serenity.rope.Balance
import com.serenity.state.manager.StateManager
import com.serenity.ui.renderer.Renderer
import com.serenity.ui.theme.config.AppThemeManager
import com.serenity.keystroke.events.{Event, UnhandledEvent}
import com.serenity.state.models.Focus

given Balance = Balance.default

object Main extends IOApp.Simple:

  def run: IO[Unit] =
    terminalResource.use { terminal =>
      screenResource(terminal).use { screen =>
        for
          // Initialize theme manager and load default theme
          themeManager = AppThemeManager.create
          defaultTheme <- themeManager.initializeWithTheme() // Uses "default-dark"
          stateManager <- StateManager.apply
          // Create initial buffer and pane for startup  
          bufferId    <- stateManager.createBuffer("Welcome to Serenity!\nStart typing to edit text.")
          paneId      <- stateManager.createPane(Some(bufferId))
          // Apply the loaded theme to the initial state
          _           <- stateManager.updateState(_.copy(theme = defaultTheme))
          inputRouter <- InputRouter.create[IO, TextEntryEvent](new TextEntryTranslator)
          inputHandler = new ScreenInputHandler[IO, TextEntryEvent](screen, inputRouter)
          // Render initial state before starting input loop
          initialState <- stateManager.getCurrentState
          _            <- IO.blocking(Renderer.render(initialState, screen))
          // Race the main event loop with the quit signal
          _ <- (
            inputHandler.eventStream
              .evalMap { event =>
                for
                  _            <- stateManager.applyEvent(event)
                  state        <- stateManager.getCurrentState
                  _            <- IO.blocking(Renderer.render(state, screen))
                  activeBuffer <- stateManager.getActiveBuffer
                  _            <- logSelectiveEvents(event, state.focus)
                yield ()
              }
              .compile
              .drain,
            stateManager.awaitQuit
          ).parMapN((_, _) => ())
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
  private def logSelectiveEvents(event: Event, currentFocus: Focus): IO[Unit] =
    event match
      case _: UnhandledEvent[?] =>
        IO.println(s"[UNHANDLED] $event")
      case _ if shouldLogFocusChange(event) =>
        IO.println(s"[FOCUS] Event: $event, Focus: $currentFocus")
      case _ => IO.unit

  private def shouldLogFocusChange(event: Event): Boolean =
    event match
      case com.serenity.keystroke.events.MoveUp => false
      case com.serenity.keystroke.events.MoveDown => false  
      case com.serenity.keystroke.events.MoveLeft => false
      case com.serenity.keystroke.events.MoveRight => false
      case com.serenity.keystroke.events.InsertChar(_) => false
      case com.serenity.keystroke.events.DeleteBackward => false
      case com.serenity.keystroke.events.DeleteForward => false
      case _ => true // Log other events that might change focus/handlers
