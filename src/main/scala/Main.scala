import cats.effect.*
import cats.syntax.parallel.*
import com.googlecode.lanterna.screen.{Screen, TerminalScreen}
import com.googlecode.lanterna.terminal.Terminal
import com.serenity.app.AppStartup
import com.serenity.config.AppConfig
import com.serenity.input.{FocusedInputTranslator, InputHandler, InputRouter, ScreenInputHandler, SwingInputHandler}
import com.serenity.keystroke.events.{Event, UnhandledEvent}
import com.serenity.keystroke.translators.TextEntryTranslator
import com.serenity.rope.Balance
import com.serenity.state.manager.StateManager
import com.serenity.state.models.{AppState, Focus}
import com.serenity.ui.fonts.FontLoader
import com.serenity.ui.layout.TerminalSize
import com.serenity.ui.renderer.{RenderController, Renderer}
import com.serenity.ui.terminal.{SwingTerminal, TerminalFactory}
import com.serenity.ui.theme.config.AppThemeManager
import fs2.Stream
import fs2.concurrent.SignallingRef
import org.typelevel.log4cats.slf4j.Slf4jFactory
import org.typelevel.log4cats.{LoggerFactory, LoggerName}

import scala.concurrent.duration.*

given Balance = Balance.default

object Main extends IOApp.Simple:

  given LoggerFactory[IO] = Slf4jFactory.create[IO]

  def run: IO[Unit] =
    given logger: org.typelevel.log4cats.Logger[IO] = LoggerFactory[IO].getLogger(using LoggerName("Main"))
    val appConfig = AppConfig.default
    val backend   = sys.env.getOrElse("SERENITY_BACKEND", "lanterna")

    backend match
      case "swing" =>
        SwingTerminal.resource().use { swingTerm =>
          for
            fonts <- com.serenity.ui.fonts.FontLoader.loadMonaspaceNeon(appConfig.fontConfig)
            font   = fonts.headOption.getOrElse(new java.awt.Font(java.awt.Font.MONOSPACED, java.awt.Font.PLAIN, 14))
            _ <- appRun(
              initialTerminalSize = swingTerm.terminalSize,
              makeInputHandler    = router => new SwingInputHandler[IO, Event](swingTerm.canvas, router),
              checkResize         = IO { swingTerm.doResizeIfNecessary() },
              renderFull          = (state, vis) => IO.blocking(Renderer.render(state, vis, swingTerm, font)),
              renderCursorOnly    = (state, vis) => IO.blocking(Renderer.render(state, vis, swingTerm, font)),
              appConfig           = appConfig
            )
          yield ()
        }
      case _ =>
        terminalResource(appConfig.fontConfig).use { terminal =>
          screenResource(terminal).use { screen =>
            for
              initialTerminalSize <- IO.blocking {
                val size = screen.getTerminalSize
                TerminalSize(size.getColumns, size.getRows)
              }
              _ <- appRun(
                initialTerminalSize = initialTerminalSize,
                makeInputHandler    = router => new ScreenInputHandler[IO, Event](screen, router),
                checkResize         = IO.blocking(Option(screen.doResizeIfNecessary()))
                                        .map(_.map(s => TerminalSize(s.getColumns, s.getRows))),
                renderFull          = (state, vis) => IO.blocking(Renderer.render(state, vis, screen)),
                renderCursorOnly    = (state, vis) => IO.blocking(Renderer.renderCursorOnly(state, vis, screen)),
                appConfig           = appConfig
              )
            yield ()
          }
        }

  private def appRun(
    initialTerminalSize: TerminalSize,
    makeInputHandler: InputRouter[IO, Event] => InputHandler[IO],
    checkResize: IO[Option[TerminalSize]],
    renderFull: (AppState, Boolean) => IO[Unit],
    renderCursorOnly: (AppState, Boolean) => IO[Unit],
    appConfig: AppConfig
  )(using logger: org.typelevel.log4cats.Logger[IO]): IO[Unit] =
    for
      _ <- logger.info("Starting Serenity text editor")
      themeManager           = AppThemeManager.create
      defaultTheme          <- themeManager.initializeWithTheme()
      stateManager          <- StateManager.apply(logger)
      initialState          <- AppStartup.initializeState(stateManager, defaultTheme, initialTerminalSize)
      inputRouter           <- InputRouter.create[IO, Event](new TextEntryTranslator)
      inputHandler           = makeInputHandler(inputRouter)
      _                     <- inputRouter.setActiveTranslator(FocusedInputTranslator.forState(initialState))
      _                     <- renderFull(initialState, true)
      _                     <- logger.info("Initial render completed, starting main loop")
      fastMode              <- SignallingRef.of[IO, Boolean](false)
      cursorVisible         <- Ref.of[IO, Boolean](true)
      checkResizeAndHandle   = checkResize.flatMap(RenderController.handleResize(_, stateManager, fastMode.set(true)))
      inputFunnel            = (s: Stream[IO, Event]) =>
        s.evalMap(event =>
          checkResizeAndHandle >>
            stateManager.applyEvent(event) >>
            stateManager.getCurrentState.flatMap(state =>
              inputRouter.setActiveTranslator(FocusedInputTranslator.forState(state))) >>
            fastMode.set(true)
        ).drain
      _ <-
        def idlePhase: Stream[IO, Unit] =
          Stream
            .fixedRate[IO](500.millis)
            .interruptWhen(fastMode.discrete)
            .evalMap { _ =>
              for
                _       <- checkResizeAndHandle
                visible <- cursorVisible.updateAndGet(b => !b)
                state   <- stateManager.getCurrentState
                _       <- renderCursorOnly(state, visible)
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
                _      <- renderFull(state, true)
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

        (
          inputHandler.eventStream
            .evalTap(event => stateManager.getCurrentState.flatMap(s => logSelectiveEvents(event, s.focus, logger)))
            .through(inputFunnel)
            .compile
            .drain,
          renderLoop.compile.drain,
          stateManager.awaitQuit,
          stateManager.intervalSaveStream.compile.drain
        ).parMapN((_, _, _, _) => ())
      _ <- logger.info("Serenity editor shutdown complete")
    yield ()

  private def terminalResource(
    fontConfig: FontLoader.FontConfig
  )(using logger: org.typelevel.log4cats.Logger[IO]): Resource[IO, Terminal] =
    Resource.make(
      for
        terminal <- TerminalFactory.createTerminal(fontConfig)
        _        <- IO.blocking(terminal.enterPrivateMode())
      yield terminal
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
    import com.googlecode.lanterna.input.KeyType
    event.keyStroke.getKeyType match
      case KeyType.EOF       => false
      case KeyType.Unknown   => true
      case KeyType.Character =>
        Option(event.keyStroke.getCharacter).exists { char =>
          char.toInt == 0 ||
          char.toInt == 4 ||
          char.toInt == 26
        }
      case _ => false
