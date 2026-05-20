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
                  _            <- IO.println(s"Event: $event, Focus: ${state.focus}, Buffer: ${activeBuffer.map(_.id)}")
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
