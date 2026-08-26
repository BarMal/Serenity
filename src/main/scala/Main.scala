import cats.effect.*
import cats.syntax.all.*
import com.serenity.animation.WindowSitter
import com.serenity.app.*
import com.serenity.config.{AppConfig, ConfigManager, ConfigMigrationWarning, MotionFamily}
import com.serenity.diagnostics.Trace
import com.serenity.input.SwingInputHandler
import com.serenity.io.SwingFileDialog
import com.serenity.rope.Balance
import com.serenity.ui.accessibility.{AccessibilitySnapshot, AccessibilitySync}
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
      initialDisplay = displayState.snapshot
      _ <- (
        SwingWindow.resource(
          initialDisplay.codeMetrics,
          initialDisplay.uiMetrics,
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
                IO.blocking {
                  val display = displayState.snapshot
                  swingWin.updateMetrics(display.codeMetrics, display.uiMetrics)
                }
            else IO.unit

          def syncDisplayMetrics(): IO[Unit] =
            Trace.timed("render.syncDisplayMetrics") {
              IO {
                val display = displayState.snapshot
                if swingWin.metrics != display.codeMetrics then
                  swingWin.updateMetrics(display.codeMetrics, display.uiMetrics)
              }.evalOn(paintEc)
            }

          def syncChromeTheme(state: com.serenity.state.models.AppState): IO[Unit] =
            Trace.timed("render.syncChromeTheme") {
              IO {
                swingWin.updateChromeTheme(state.theme)
                val sitterVisible = state.config.windowSitterConfig.enabled &&
                  state.config.surfaceConfig.effectiveMotionConfiguration.family(MotionFamily.UiTransitions).enabled
                swingWin.updateWindowSitter(state.windowSitter, sitterVisible)
              }.evalOn(paintEc)
            }

          AccessibilitySync.empty.flatMap { accessibilitySync =>
            def syncAccessibility(state: com.serenity.state.models.AppState): IO[Unit] =
              Trace.timed("render.syncAccessibility") {
                accessibilitySync
                  .sync(state)(previous => IO(AccessibilitySnapshot.from(state, swingWin.viewportSize, previous)))
                  .flatMap(snapshot => IO(swingWin.updateAccessibility(snapshot)))
                  .evalOn(paintEc)
              }

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
              renderFull = (state, vis, cc, damage) =>
                syncDisplayMetrics() >> syncChromeTheme(state) >> syncAccessibility(state) >>
                  IO(paintFullFrame(state, vis, cc, swingWin, displayState.snapshot, damage)).evalOn(paintEc),
              renderCursorOnly = (state, vis, cc, damage) =>
                syncDisplayMetrics() >> syncChromeTheme(state) >> syncAccessibility(state) >>
                  IO(paintCursorFrame(state, vis, cc, swingWin, displayState.snapshot, damage)).evalOn(paintEc),
              appConfig = actualAppConfig,
              makeStateManager = Some(logger =>
                com.serenity.state.manager.StateManager.apply(
                  logger,
                  onFontConfigChanged = config =>
                    displayState.update(config) >>
                      IO.blocking {
                        val display = displayState.snapshot
                        swingWin.updateMetrics(display.codeMetrics, display.uiMetrics)
                      },
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
        }
    yield ExitCode.Success

  private def resolveAutoTextScale(config: AppConfig, detectedTextScale: Double): AppConfig =
    config.withFontConfig(config.fontConfig.resolveAutoTextScale(detectedTextScale))

  /** Paint a whole frame.
    *
    * `display` is taken once by the caller and threaded through: reading each font and metric from the runtime
    * separately would let a concurrent font-config change land mid-frame and paint glyphs at one generation's advance
    * with another's metrics.
    */
  private def paintFullFrame(
    state: com.serenity.state.models.AppState,
    cursorVisible: Boolean,
    cursorColor: Option[java.awt.Color],
    window: SwingWindow,
    display: RuntimeDisplayState.Snapshot,
    damage: com.serenity.state.models.Damage
  ): Unit =
    if cursorVisible then
      val _ = Renderer.renderWithCursorOverlay(
        state,
        window,
        display.codeFont,
        display.textFont,
        display.uiFont,
        display.uiMetrics,
        cursorColor,
        damage
      )
      ()
    else
      Renderer.render(
        state,
        cursorVisible = false,
        window,
        display.codeFont,
        display.textFont,
        display.uiFont,
        display.uiMetrics,
        None,
        repaintOnFlush = SwingWindow.shouldRepaintBaseFrameBeforeCursorOverlay(cursorVisible),
        damage = damage
      )

  /** Repaint only the cursor overlay, falling back to a full frame when the overlay path declines. */
  private def paintCursorFrame(
    state: com.serenity.state.models.AppState,
    cursorVisible: Boolean,
    cursorColor: Option[java.awt.Color],
    window: SwingWindow,
    display: RuntimeDisplayState.Snapshot,
    damage: com.serenity.state.models.Damage
  ): Unit =
    val rendered = Renderer.renderCursorOnly(
      state,
      cursorVisible,
      window,
      display.codeFont,
      display.textFont,
      display.uiFont,
      display.uiMetrics,
      cursorColor
    )
    if !rendered then
      Renderer.render(
        state,
        cursorVisible,
        window,
        display.codeFont,
        display.textFont,
        display.uiFont,
        display.uiMetrics,
        cursorColor,
        repaintOnFlush = true,
        damage = damage
      )
