import cats.effect.*
import cats.syntax.all.*
import com.serenity.animation.WindowSitter
import com.serenity.app.*
import com.serenity.config.{AppConfig, ConfigManager, ConfigMigrationWarning, MotionFamily}
import com.serenity.input.SwingInputHandler
import com.serenity.io.SwingFileDialog
import com.serenity.rope.Balance
import com.serenity.ui.accessibility.AccessibilitySnapshot
import com.serenity.ui.display.DisplayScale
import com.serenity.ui.renderer.{PaintExecutionContext, Renderer}
import com.serenity.ui.terminal.SwingWindow
import org.typelevel.log4cats.slf4j.Slf4jFactory
import org.typelevel.log4cats.{LoggerFactory, LoggerName}

given Balance = Balance.default

object Main extends IOApp:

  given LoggerFactory[IO] = Slf4jFactory.create[IO]

  def run(args: List[String]): IO[ExitCode] =
    given logger: org.typelevel.log4cats.Logger[IO] = LoggerFactory[IO].getLogger(using LoggerName("Main"))

    for
      _ <- Java2DPipeline.installSafeDefaults()
      _ <- IO(CrashReporter.install())
      launchOptions = LaunchOptions.parse(args)
      configResult <- ConfigManager.loadConfigResultIO()
      configLoad <- configResult.fold(
        error =>
          logger.error(error.cause.getOrElse(new RuntimeException(error.message)))(s"[CONFIG] ${error.message}") >>
            IO.pure(
              com.serenity.config.ConfigLoadResult(AppConfig.default, com.serenity.config.ConfigMigrationReport.empty)
            ),
        IO.pure
      )
      _ <- ConfigMigrationWarning
        .message(ConfigManager.defaultConfigPath, configLoad.report)
        .fold(IO.unit)(message => logger.warn(message))
      appConfig = resolveAutoTextScale(configLoad.config, DisplayScale.defaultDeviceScale.textScale)
      displayState <- RuntimeDisplayState.create(appConfig.fontConfig)
      _ <- (
        SwingWindow.resource(
          displayState.primaryMetrics,
          displayState.uiMetrics,
          appConfig.windowChromeMode,
          appConfig.preferredWindowSize,
          initialWindowSitter = WindowSitter.fromConfig(appConfig.windowSitterConfig),
          initialWindowSitterVisible = appConfig.windowSitterConfig.enabled &&
            appConfig.surfaceConfig.effectiveMotionConfiguration.family(MotionFamily.UiTransitions).enabled
        ),
        PaintExecutionContext.resource
      ).tupled
        .use { (swingWin, paintEc) =>
          val actualAppConfig =
            resolveAutoTextScale(appConfig, swingWin.detectedDeviceTextScale)
          val initialScaleSync =
            if actualAppConfig.fontConfig != appConfig.fontConfig then
              displayState.update(actualAppConfig.fontConfig) >>
                IO.blocking(swingWin.updateMetrics(displayState.primaryMetrics, displayState.uiMetrics))
            else IO.unit

          def syncDisplayMetrics(): IO[Unit] =
            IO {
              val metrics = displayState.primaryMetrics
              if swingWin.metrics != metrics then swingWin.updateMetrics(metrics, displayState.uiMetrics)
            }.evalOn(paintEc)

          def syncChromeTheme(state: com.serenity.state.models.AppState): IO[Unit] =
            IO {
              swingWin.updateChromeTheme(state.theme)
              val sitterVisible = state.config.windowSitterConfig.enabled &&
                state.config.surfaceConfig.effectiveMotionConfiguration.family(MotionFamily.UiTransitions).enabled
              swingWin.updateWindowSitter(state.windowSitter, sitterVisible)
            }.evalOn(paintEc)

          def syncAccessibility(state: com.serenity.state.models.AppState): IO[Unit] =
            IO(swingWin.updateAccessibility(AccessibilitySnapshot.from(state, swingWin.viewportSize)))
              .evalOn(paintEc)

          initialScaleSync >> AppRuntime.run(
            initialViewportSize = swingWin.viewportSize,
            makeInputHandler = router =>
              new SwingInputHandler[IO, com.serenity.keystroke.events.Event](
                swingWin.canvas,
                router,
                () => swingWin.metrics,
                () => displayState.uiMetrics
              ),
            checkResize = IO(swingWin.doResizeIfNecessary()),
            renderFull = (state, vis, cc) =>
              syncDisplayMetrics() >> syncChromeTheme(state) >> syncAccessibility(state) >> IO {
                if vis then
                  val _ = Renderer.renderWithCursorOverlay(
                    state,
                    swingWin,
                    displayState.codeFont,
                    displayState.textFont,
                    displayState.uiFont,
                    displayState.uiMetrics,
                    cc
                  )
                  ()
                else
                  Renderer.render(
                    state,
                    cursorVisible = false,
                    swingWin,
                    displayState.codeFont,
                    displayState.textFont,
                    displayState.uiFont,
                    displayState.uiMetrics,
                    None,
                    repaintOnFlush = SwingWindow.shouldRepaintBaseFrameBeforeCursorOverlay(vis)
                  )
              }.evalOn(paintEc),
            renderCursorOnly = (state, vis, cc) =>
              syncDisplayMetrics() >> syncChromeTheme(state) >> syncAccessibility(state) >> IO {
                val rendered = Renderer.renderCursorOnly(
                  state,
                  vis,
                  swingWin,
                  displayState.codeFont,
                  displayState.textFont,
                  displayState.uiFont,
                  displayState.uiMetrics,
                  cc
                )
                if !rendered then
                  Renderer.render(
                    state,
                    vis,
                    swingWin,
                    displayState.codeFont,
                    displayState.textFont,
                    displayState.uiFont,
                    displayState.uiMetrics,
                    cc,
                    repaintOnFlush = true
                  )
              }.evalOn(paintEc),
            appConfig = actualAppConfig,
            makeStateManager = Some(logger =>
              com.serenity.state.manager.StateManager.apply(
                logger,
                onFontConfigChanged = config =>
                  displayState.update(config) >>
                    IO.blocking(swingWin.updateMetrics(displayState.primaryMetrics, displayState.uiMetrics)),
                deviceTextScaleProvider = IO.blocking(swingWin.detectedDeviceTextScale),
                configPersistencePath = Some(ConfigManager.defaultConfigPath),
                windowSizeProvider = IO.blocking(Some(swingWin.currentPreferredWindowSize)),
                onPreferredWindowSizeChanged = size => IO.blocking(swingWin.resizeToPreferred(size)),
                fileDialog = SwingFileDialog(swingWin.canvas)
              )
            ),
            awaitExternalQuit = swingWin.awaitClose,
            registerResizeCallback = cb => swingWin.setOnResize(cb),
            openPath = launchOptions.openPath
          )
        }
    yield ExitCode.Success

  private def resolveAutoTextScale(config: AppConfig, detectedTextScale: Double): AppConfig =
    config.withFontConfig(config.fontConfig.resolveAutoTextScale(detectedTextScale))
