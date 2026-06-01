import cats.effect.*
import cats.effect.unsafe.implicits.global
import com.serenity.app.{AppRuntime, RuntimeDisplayState}
import com.serenity.config.AppConfig
import com.serenity.input.SwingInputHandler
import com.serenity.rope.Balance
import com.serenity.ui.renderer.Renderer
import com.serenity.ui.terminal.SwingWindow
import org.typelevel.log4cats.slf4j.Slf4jFactory
import org.typelevel.log4cats.{LoggerFactory, LoggerName}

given Balance = Balance.default

object Main extends IOApp.Simple:

  given LoggerFactory[IO] = Slf4jFactory.create[IO]

  def run: IO[Unit] =
    given logger: org.typelevel.log4cats.Logger[IO] = LoggerFactory[IO].getLogger(using LoggerName("Main"))
    val appConfig                                   = AppConfig.default

    for
      displayState <- RuntimeDisplayState.create(appConfig.fontConfig)
      _ <- SwingWindow.resource(displayState.textMetrics, appConfig.windowChromeMode).use { swingWin =>
        def syncDisplayMetrics(state: com.serenity.state.models.AppState): IO[java.awt.Font] =
          IO.blocking {
            val metrics = displayState.metricsFor(state)
            if swingWin.metrics != metrics then swingWin.updateMetrics(metrics)
            displayState.fontFor(state)
          }

        AppRuntime.run(
          initialViewportSize = swingWin.viewportSize,
          makeInputHandler =
            router => new SwingInputHandler[IO, com.serenity.keystroke.events.Event](swingWin.canvas, router, () => swingWin.metrics),
          checkResize = IO(swingWin.doResizeIfNecessary()),
          renderFull =
            (state, vis, cc) =>
              syncDisplayMetrics(state).flatMap(font => IO.blocking(Renderer.render(state, vis, swingWin, font, cc))),
          renderCursorOnly =
            (state, vis, cc) =>
              syncDisplayMetrics(state).flatMap(font => IO.blocking(Renderer.render(state, vis, swingWin, font, cc))),
          appConfig = appConfig,
          makeStateManager = Some(logger =>
            com.serenity.state.manager.StateManager.apply(
              logger,
              onFontConfigChanged = config =>
                displayState.update(config) >>
                  IO.blocking(swingWin.updateMetrics(displayState.textMetrics))
            )
          ),
          awaitExternalQuit = swingWin.awaitClose,
          registerResizeCallback = cb => swingWin.setOnResize(() => cb.unsafeRunAndForget())
        )
      }
    yield ()
