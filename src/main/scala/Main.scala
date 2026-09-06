import scala.concurrent.duration.Duration

import cats.effect.*
import cats.effect.unsafe.IORuntimeConfig
import cats.syntax.all.*
import com.serenity.animation.WindowSitter
import com.serenity.BuildInfo
import com.serenity.app.*
import com.serenity.config.{AppConfig, ConfigManager, ConfigMigrationWarning, MotionFamily}
import com.serenity.diagnostics.{Trace, TuiConsoleLogFilter}
import com.serenity.input.SwingInputHandler
import com.serenity.io.SwingFileDialog
import com.serenity.rope.Balance
import com.serenity.ui.accessibility.{AccessibilitySnapshot, AccessibilitySync}
import com.serenity.ui.display.DisplayScale
import com.serenity.ui.renderer.{PaintExecutionContext, Renderer}
import com.serenity.ui.terminal.SwingWindow
import com.serenity.ui.tui.{TerminalShell, TuiRuntime}
import org.typelevel.log4cats.slf4j.Slf4jFactory
import org.typelevel.log4cats.{Logger, LoggerFactory, LoggerName}

given Balance = Balance.default

object Main extends IOApp:

  // Hibernate/restore causes a wall-clock jump that makes every fiber appear stalled, flooding stderr
  // with starvation warnings and corrupting the TUI display. An interactive editor has no latency SLA
  // that the checker could meaningfully enforce, so disable it.
  override def runtimeConfig: IORuntimeConfig =
    super.runtimeConfig.copy(cpuStarvationCheckInitialDelay = Duration.Inf)

  given LoggerFactory[IO] = Slf4jFactory.create[IO]

  def run(args: List[String]): IO[ExitCode] =
    // An unparseable command line is reported and nothing is started. `Help.errors` is empty for a `--help` request
    // and non-empty for a rejected argument, which is the difference between exiting zero and exiting non-zero.
    LaunchOptions.parse(args) match
      case Left(help) =>
        IO(System.err.println(help)).as(if help.errors.isEmpty then ExitCode.Success else ExitCode.Error)
      case Right(options) if options.showVersion =>
        IO(println(s"Serenity ${BuildInfo.version} (${BuildInfo.commit})")).as(ExitCode.Success)
      case Right(options) => launch(options)

  private def launch(launchOptionsForLogging: LaunchOptions): IO[ExitCode] =
    // #1215: must run before the `given logger` below, which triggers logback's one-time console-appender setup on
    // its first call -- `TuiConsoleLogFilter` reads this property per log event, but it still has to be set before
    // the very first event a TUI launch could otherwise leak onto the terminal surface it is about to take over.
    System.setProperty(
      TuiConsoleLogFilter.EnabledProperty,
      LaunchOptions.resolveTuiMode(launchOptionsForLogging).toString
    )

    given logger: org.typelevel.log4cats.Logger[IO] = LoggerFactory[IO].getLogger(using LoggerName("Main"))

    for
      _ <- Java2DPipeline.installSafeDefaults()
      _ <- IO(CrashReporter.install())
      launchOptions = launchOptionsForLogging
      configResult <- ConfigManager.loadConfigResultIO()
      // A config that cannot be read means this session runs on defaults -- every setting the user had, gone until
      // they restart with a readable file. Keep their file aside before anything can overwrite it, and carry a notice
      // the start page can show: the log line below is invisible in TUI mode, where stderr goes to nowhere.
      configNotice <- configResult.fold(
        error =>
          logger.error(error.cause.getOrElse(new RuntimeException(error.message)))(s"[CONFIG] ${error.message}") >>
            IO.blocking(ConfigManager.preserveUnreadableConfig(ConfigManager.defaultConfigPath)).map { preserved =>
              val kept = preserved.fold("")(path => s" A copy was kept at $path.")
              Some(s"Configuration could not be read, so this session is using defaults.$kept")
            },
        _ => IO.pure(None)
      )
      configLoad = configResult.getOrElse(
        com.serenity.config.ConfigLoadResult(AppConfig.default, com.serenity.config.ConfigMigrationReport.empty)
      )
      _ <- ConfigMigrationWarning
        .message(ConfigManager.defaultConfigPath, configLoad.report)
        .fold(IO.unit)(message => logger.warn(message))
      appConfig = resolveAppConfig(configLoad.config, launchOptions)
      _ <-
        if LaunchOptions.resolveTuiMode(launchOptions) then runTui(appConfig, launchOptions, configNotice)
        else runGui(appConfig, launchOptions, configNotice)
    yield ExitCode.Success

  /** The TUI launch path (issue #1112): a real system terminal via [[TerminalShell.resource]], restored on every exit
    * path by that `Resource`'s release. This branch never references `SwingWindow` -- the terminal capability bundle
    * lives entirely in [[TuiRuntime]], which owns no such reference either.
    */
  private def runTui(appConfig: AppConfig, launchOptions: LaunchOptions, configNotice: Option[String])(using
    logger: Logger[IO],
    loggerFactory: LoggerFactory[IO]
  ): IO[Unit] =
    // Silence raw System.err writes so they never corrupt the alternate-screen TUI surface. logback's console
    // appender is already suppressed by TuiConsoleLogFilter; this catches any direct System.err traffic that
    // bypasses the logging framework (CE3 stall checker, JVM internals). Crash info is preserved by CrashReporter.
    IO(System.setErr(new java.io.PrintStream(java.io.OutputStream.nullOutputStream()))) >>
      TuiRuntime.run(
        shell = TerminalShell.resource,
        appConfig = appConfig,
        openPath = launchOptions.openPath,
        configPersistencePath = Some(ConfigManager.defaultConfigPath),
        hasDisplay = LaunchOptions.isDisplayReachable(sys.env),
        configNotice = configNotice
      )

  /** The GUI launch path: unchanged from before #1112 beyond being extracted into its own method. Constructs a
    * [[SwingWindow]] and closes `AppRuntime.run`'s capabilities over it; never touches [[TerminalShell]].
    */
  private def runGui(appConfig: AppConfig, launchOptions: LaunchOptions, configNotice: Option[String])(using
    logger: Logger[IO],
    loggerFactory: LoggerFactory[IO]
  ): IO[Unit] =
    for
      displayState <- RuntimeDisplayState.create(appConfig.editorConfig.fontConfig)
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
            if actualAppConfig.editorConfig.fontConfig != appConfig.editorConfig.fontConfig then
              displayState.update(actualAppConfig.editorConfig.fontConfig) >>
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
                swingWin.updateChromeTheme(state.persisted.theme)
                val sitterVisible = state.persisted.config.windowSitterConfig.enabled &&
                  state.persisted.config.surfaceConfig.effectiveMotionConfiguration
                    .family(MotionFamily.UiTransitions)
                    .enabled
                swingWin.updateWindowSitter(state.runtime.windowSitter, sitterVisible)
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
                IO.pure(
                  new SwingInputHandler[IO, com.serenity.keystroke.events.Event](
                    swingWin.canvas,
                    router,
                    () => swingWin.metrics,
                    () => displayState.uiMetrics,
                    actualAppConfig.inputConfig.wheelScrollLines
                  )
                ),
              checkResize = IO(swingWin.doResizeIfNecessary()),
              renderFull = (state, vis, cc, damage, bufferAnimations) =>
                syncDisplayMetrics() >> syncChromeTheme(state) >> syncAccessibility(state) >>
                  IO(
                    paintFullFrame(state, vis, cc, swingWin, displayState.snapshot, damage, bufferAnimations)
                  ).evalOn(paintEc),
              renderCursorOnly = (state, vis, cc, damage, bufferAnimations) =>
                syncDisplayMetrics() >> syncChromeTheme(state) >> syncAccessibility(state) >>
                  IO(
                    paintCursorFrame(state, vis, cc, swingWin, displayState.snapshot, damage, bufferAnimations)
                  ).evalOn(paintEc),
              appConfig = actualAppConfig,
              configNotice = configNotice,
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
                  fileDialog = Some(SwingFileDialog(swingWin.canvas))
                )
              ),
              awaitExternalQuit = swingWin.awaitClose,
              registerResizeCallback = cb => swingWin.setOnResize(cb),
              registerFocusCallback = cb => swingWin.setOnFocusChange(cb),
              openPath = launchOptions.openPath
            )
          }
        }
    yield ()

  private def resolveAutoTextScale(config: AppConfig, detectedTextScale: Double): AppConfig =
    config.withFontConfig(config.editorConfig.fontConfig.resolveAutoTextScale(detectedTextScale))

  /** Applies the eco overlay (if requested via `--eco` or `SERENITY_ECO=1`) and the alpha overlay (if requested via
    * `--alpha`) before the auto text-scale resolution that follows every config load, so their changes are visible to
    * that step just like any other loaded setting. Eco touches only the render fps target and motion accessibility;
    * alpha touches only the currently-gated experimental prototype flags (command-runner cursor-peek today) -- the two
    * overlays don't share any field, so application order between them doesn't matter.
    */
  private def resolveAppConfig(loadedConfig: AppConfig, launchOptions: LaunchOptions): AppConfig =
    resolveAutoTextScale(
      AlphaMode.applyIfRequested(EcoMode.applyIfRequested(loadedConfig, launchOptions), launchOptions),
      DisplayScale.defaultDeviceScale.textScale
    )

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
    damage: com.serenity.state.models.Damage,
    bufferAnimations: Map[com.serenity.state.models.BufferId, com.serenity.animation.AnimationState]
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
        damage,
        bufferAnimations
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
        damage = damage,
        bufferAnimations = bufferAnimations
      )

  /** Repaint only the cursor overlay, falling back to a full frame when the overlay path declines. */
  private def paintCursorFrame(
    state: com.serenity.state.models.AppState,
    cursorVisible: Boolean,
    cursorColor: Option[java.awt.Color],
    window: SwingWindow,
    display: RuntimeDisplayState.Snapshot,
    damage: com.serenity.state.models.Damage,
    bufferAnimations: Map[com.serenity.state.models.BufferId, com.serenity.animation.AnimationState]
  ): Unit =
    val rendered = Renderer.renderCursorOnly(
      state,
      cursorVisible,
      window,
      display.codeFont,
      display.textFont,
      display.uiFont,
      display.uiMetrics,
      cursorColor,
      bufferAnimations
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
        damage = damage,
        bufferAnimations = bufferAnimations
      )
