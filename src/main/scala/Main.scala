import cats.effect.*
import cats.effect.unsafe.implicits.global
import com.serenity.app.{AppRuntime, CrashReporter, RuntimeDisplayState}
import com.serenity.config.{AppConfig, ConfigManager, ConfigMigrationWarning}
import com.serenity.input.SwingInputHandler
import com.serenity.io.SwingFileDialog
import com.serenity.rope.Balance
import com.serenity.ui.display.DisplayScale
import com.serenity.ui.renderer.Renderer
import com.serenity.ui.terminal.SwingWindow
import org.typelevel.log4cats.slf4j.Slf4jFactory
import org.typelevel.log4cats.{LoggerFactory, LoggerName}

given Balance = Balance.default

object Main extends IOApp:

  given LoggerFactory[IO] = Slf4jFactory.create[IO]

  def run(args: List[String]): IO[ExitCode] =
    given logger: org.typelevel.log4cats.Logger[IO] = LoggerFactory[IO].getLogger(using LoggerName("Main"))

    for
      _          <- IO(CrashReporter.install())
      configLoad <- ConfigManager.loadConfigResultIO()
      _ <- ConfigMigrationWarning
        .message(ConfigManager.defaultConfigPath, configLoad.report)
        .fold(IO.unit)(message => logger.warn(message))
      appConfig = resolveAutoTextScale(configLoad.config, DisplayScale.defaultDeviceScale.textScale)
      displayState <- RuntimeDisplayState.create(appConfig.fontConfig)
      _ <- SwingWindow
        .resource(
          displayState.primaryMetrics,
          displayState.uiMetrics,
          appConfig.windowChromeMode,
          appConfig.preferredWindowSize
        )
        .use { swingWin =>
          val actualAppConfig =
            resolveAutoTextScale(appConfig, swingWin.detectedDeviceTextScale)
          val initialScaleSync =
            if actualAppConfig.fontConfig != appConfig.fontConfig then
              displayState.update(actualAppConfig.fontConfig) >>
                IO.blocking(swingWin.updateMetrics(displayState.primaryMetrics, displayState.uiMetrics))
            else IO.unit

          def syncDisplayMetrics(): IO[Unit] =
            IO.blocking {
              val metrics = displayState.primaryMetrics
              if swingWin.metrics != metrics then swingWin.updateMetrics(metrics, displayState.uiMetrics)
            }

          initialScaleSync >> AppRuntime.run(
            initialViewportSize = swingWin.viewportSize,
            makeInputHandler = router =>
              new SwingInputHandler[IO, com.serenity.keystroke.events.Event](
                swingWin.canvas,
                router,
                () => swingWin.metrics
              ),
            checkResize = IO(swingWin.doResizeIfNecessary()),
            renderFull = (state, vis, cc) =>
              syncDisplayMetrics() >> IO.blocking(
                Renderer.render(
                  state,
                  vis,
                  swingWin,
                  displayState.codeFont,
                  displayState.textFont,
                  displayState.uiFont,
                  displayState.uiMetrics,
                  cc
                )
              ),
            renderCursorOnly = (state, vis, cc) =>
              syncDisplayMetrics() >> IO.blocking(
                Renderer.render(
                  state,
                  vis,
                  swingWin,
                  displayState.codeFont,
                  displayState.textFont,
                  displayState.uiFont,
                  displayState.uiMetrics,
                  cc
                )
              ),
            appConfig = actualAppConfig,
            makeStateManager = Some(logger =>
              com.serenity.state.manager.StateManager.apply(
                logger,
                onFontConfigChanged = config =>
                  displayState.update(config) >>
                    IO.blocking(swingWin.updateMetrics(displayState.primaryMetrics, displayState.uiMetrics)),
                configPersistencePath = Some(ConfigManager.defaultConfigPath),
                windowSizeProvider = IO.blocking(Some(swingWin.currentPreferredWindowSize)),
                onPreferredWindowSizeChanged = size => IO.blocking(swingWin.resizeToPreferred(size)),
                fileDialog = SwingFileDialog(swingWin.canvas)
              )
            ),
            awaitExternalQuit = swingWin.awaitClose,
            registerResizeCallback = cb => swingWin.setOnResize(() => cb.unsafeRunAndForget())
          )
        }
    yield ExitCode.Success

  private def resolveAutoTextScale(config: AppConfig, detectedTextScale: Double): AppConfig =
    config.withFontConfig(config.fontConfig.resolveAutoTextScale(detectedTextScale))
