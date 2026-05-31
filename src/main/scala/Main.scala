import cats.effect.*
import cats.effect.unsafe.implicits.global
import cats.syntax.parallel.*
import com.serenity.app.AppStartup
import com.serenity.config.{AppConfig, CursorMode}
import com.serenity.input.{FocusedInputTranslator, InputHandler, InputRouter, SwingInputHandler}
import com.serenity.lsp.LspManager
import com.serenity.keystroke.events.{Event, UnhandledEvent}
import com.serenity.keystroke.translators.TextEntryTranslator
import com.serenity.rope.Balance
import com.serenity.state.manager.StateManager
import com.serenity.state.models.{AppState, Focus}
import com.serenity.ui.fonts.FontLoader
import com.serenity.ui.layout.ViewportSize
import com.serenity.ui.renderer.{RenderController, Renderer}
import com.serenity.ui.terminal.SwingWindow
import com.serenity.ui.theme.config.AppThemeManager
import fs2.Stream
import fs2.concurrent.SignallingRef
import org.typelevel.log4cats.slf4j.Slf4jFactory
import org.typelevel.log4cats.{LoggerFactory, LoggerName}

import java.awt.Color
import scala.concurrent.duration.*

given Balance = Balance.default

object Main extends IOApp.Simple:

  given LoggerFactory[IO] = Slf4jFactory.create[IO]

  def run: IO[Unit] =
    given logger: org.typelevel.log4cats.Logger[IO] = LoggerFactory[IO].getLogger(using LoggerName("Main"))
    val appConfig = AppConfig.default

    for
      fonts <- FontLoader.loadMonaspaceNeon(appConfig.fontConfig)
      font   = fonts.headOption.getOrElse(new java.awt.Font(java.awt.Font.MONOSPACED, java.awt.Font.PLAIN, 14))
      metrics = com.serenity.ui.layout.CellMetrics.fromFont(font)
      _ <- SwingWindow.resource(metrics).use { swingWin =>
        appRun(
          initialViewportSize    = swingWin.viewportSize,
          makeInputHandler       = router => new SwingInputHandler[IO, Event](swingWin.canvas, router, metrics),
          checkResize            = IO { swingWin.doResizeIfNecessary() },
          renderFull             = (state, vis, cc) => IO.blocking(Renderer.render(state, vis, swingWin, font, cc)),
          renderCursorOnly       = (state, vis, cc) => IO.blocking(Renderer.render(state, vis, swingWin, font, cc)),
          appConfig              = appConfig,
          awaitExternalQuit      = swingWin.awaitClose,
          registerResizeCallback = cb => swingWin.setOnResize(() => cb.unsafeRunAndForget())
        )
      }
    yield ()

  private def appRun(
    initialViewportSize: ViewportSize,
    makeInputHandler: InputRouter[IO, Event] => InputHandler[IO],
    checkResize: IO[Option[ViewportSize]],
    renderFull: (AppState, Boolean, Option[Color]) => IO[Unit],
    renderCursorOnly: (AppState, Boolean, Option[Color]) => IO[Unit],
    appConfig: AppConfig,
    awaitExternalQuit: IO[Unit] = IO.never,
    registerResizeCallback: IO[Unit] => Unit = _ => ()
  )(using logger: org.typelevel.log4cats.Logger[IO]): IO[Unit] =
    for
      _ <- logger.info("Starting Serenity text editor")
      themeManager           = AppThemeManager.create
      defaultTheme          <- themeManager.initializeWithTheme()
      stateManager          <- StateManager.apply(logger)
      initialState          <- AppStartup.initializeState(stateManager, defaultTheme, initialViewportSize)
      inputRouter           <- InputRouter.create[IO, Event](new TextEntryTranslator)
      inputHandler           = makeInputHandler(inputRouter)
      _                     <- inputRouter.setActiveTranslator(FocusedInputTranslator.forState(initialState))
      _                     <- renderFull(initialState, true, None)
      _                     <- logger.info("Initial render completed, starting main loop")
      fastMode              <- SignallingRef.of[IO, Boolean](false)
      _                     <- IO(registerResizeCallback(fastMode.set(true)))
      cursorVisible         <- Ref.of[IO, Boolean](true)
      breathIndex           <- Ref.of[IO, Int](0)
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
        def computeCursorForIdle(state: AppState): IO[(Boolean, Option[Color])] =
          state.config.cursorMode match
            case CursorMode.Blink =>
              cursorVisible.updateAndGet(!_).map(vis => (vis, None))
            case CursorMode.Breathe =>
              for
                i    <- breathIndex.updateAndGet(i => (i + 1) % 48)
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
            awaitExternalQuit >> stateManager.applyEvent(com.serenity.keystroke.events.Quit),
            stateManager.awaitQuit
          ).void,
          LspManager.run(stateManager.lspEffectStream, logger)
        ).parMapN((_, _, _, _, _, _) => ())
      _ <- logger.info("Serenity editor shutdown complete")
    yield ()

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
    import com.serenity.keystroke.InputKey
    event.info.keyType match
      case InputKey.EOF       => false
      case InputKey.Unknown   => true
      case InputKey.Character =>
        event.info.character.exists { char =>
          char.toInt == 0 ||
          char.toInt == 4 ||
          char.toInt == 26
        }
      case _ => false
