import java.nio.file.Path
import java.time.Instant

import scala.concurrent.ExecutionContext
import scala.concurrent.duration.Duration

import cats.effect.*
import cats.effect.unsafe.IORuntimeConfig
import cats.syntax.all.*
import com.serenity.BuildInfo
import com.serenity.app.*
import com.serenity.app.LaunchReset.Moved
import com.serenity.app.instance.{LaunchRole, SingleInstance}
import com.serenity.config.{
  AppConfig,
  ConfigDiagnostic,
  ConfigLoadResult,
  ConfigManager,
  ConfigMigrationReport,
  ConfigMigrationWarning,
  ConfigNotice
}
import com.serenity.diagnostics.{FrameKind, FramePhase, FrameTimings, Trace, TuiConsoleLogFilter}
import com.serenity.frontend.{Frontend, FrontendRuntime, GuiFrontend}
import com.serenity.input.SwingInputHandler
import com.serenity.io.SwingFileDialog
import com.serenity.rope.Balance
import com.serenity.session.SessionManager
import com.serenity.state.models.RestartMode
import com.serenity.ui.accessibility.{AccessibilitySnapshot, AccessibilitySync}
import com.serenity.ui.color.RenderColor
import com.serenity.ui.display.DisplayScale
import com.serenity.ui.renderer.{FontSpec, PaintExecutionContext, RendererCursorOverlay, RendererEntryPoints}
import com.serenity.ui.terminal.{SwingMenuBar, SwingWindow}
import com.serenity.ui.tui.{TerminalShell, TuiRuntime}
import fs2.Stream
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
    // Before anything else: the toolkit is fixed the moment the first java.awt class initialises.
    ToolkitSelection.install.flatMap { toolkit =>
      // An unparseable command line is reported and nothing is started. `Help.errors` is empty for a `--help` request
      // and non-empty for a rejected argument, which is the difference between exiting zero and exiting non-zero.
      LaunchOptions.parse(args) match
        case Left(help) =>
          IO(System.err.println(help)).as(if help.errors.isEmpty then ExitCode.Success else ExitCode.Error)
        case Right(options) if options.showVersion =>
          IO(println(s"Serenity ${BuildInfo.version} (${BuildInfo.commit})")).as(ExitCode.Success)
        case Right(options) => launchUntilSettled(options, toolkit)
    }

  /** A restart ends the running editor and starts the next one in this same JVM, so the terminal and the toolkit choice
    * carry straight over; the single-instance lock is released and taken again by the next launch. It drops the
    * one-shot resets.
    */
  private def launchUntilSettled(options: LaunchOptions, toolkit: ToolkitSelection.Decision): IO[ExitCode] =
    Ref.of[IO, Option[RestartMode]](None).flatMap { restartRequested =>
      launch(options, toolkit, mode => restartRequested.set(Some(mode))) >> restartRequested.get.flatMap {
        case Some(mode) =>
          launchUntilSettled(
            options.copy(safeMode = mode == RestartMode.InSafeMode, resetConfig = false, resetSession = false),
            toolkit
          )
        case None => IO.pure(ExitCode.Success)
      }
    }

  private def launch(
    launchOptionsForLogging: LaunchOptions,
    toolkit: ToolkitSelection.Decision,
    requestRestart: RestartMode => IO[Unit]
  ): IO[ExitCode] =
    // #1215/#1669: must run before the `given logger` below, which triggers logback's one-time console-appender setup
    // on its first call -- `TuiConsoleLogFilter` checks this per log event, but it still has to be configured before
    // the very first event a TUI launch could otherwise leak onto the terminal surface it is about to take over. The
    // real `GuiFrontend`/`TuiFrontend` instance doesn't exist yet this early (constructing one means acquiring the
    // Swing window or terminal resource, below) -- `Frontend.guiLogRouting`/`tuiLogRouting` are exactly what those
    // instances' own `logRouting` would answer, without needing one built.
    TuiConsoleLogFilter.configure(
      if LaunchOptions.resolveTuiMode(launchOptionsForLogging) then Frontend.tuiLogRouting else Frontend.guiLogRouting
    )

    given logger: org.typelevel.log4cats.Logger[IO] = LoggerFactory[IO].getLogger(using LoggerName("Main"))

    for
      _         <- logger.info(s"[TOOLKIT] ${toolkit.choice} (${toolkit.reason})")
      _         <- Java2DPipeline.installSafeDefaults()
      _         <- IO(CrashReporter.install())
      requested <- IO(launchOptionsForLogging.openPath.map(_.toAbsolutePath.normalize).toList)
      // #2023: settled before anything reads or writes the session, which only one process may own.
      instances = SingleInstance.forConfigDirectory(SessionManager.defaultSessionRoot(), logger)
      _ <- SingleInstance.claim(instances, requested, logger).use {
        case LaunchRole.Forwarded => reportForwarded(requested)
        case role                 => runAs(role, launchOptionsForLogging, requestRestart)
      }
    yield ExitCode.Success

  private def reportForwarded(paths: List[Path])(using logger: Logger[IO]): IO[Unit] =
    val message =
      if paths.isEmpty then "Serenity is already running; its window was brought forward."
      else s"Serenity is already running; opened ${paths.mkString(", ")} there."
    logger.info(s"[INSTANCE] $message") >> IO.println(message)

  /** Everything here follows the instance claim: a launch that handed its files to the running one has already
    * returned, so it records no start attempt and moves no file.
    */
  private def runAs(role: LaunchRole, launchOptions: LaunchOptions, requestRestart: RestartMode => IO[Unit])(using
    logger: Logger[IO]
  ): IO[Unit] =
    for
      startedAt                   <- IO.realTimeInstant
      (configMoved, sessionMoved) <- applyResets(role, launchOptions, startedAt)
      unfinishedStarts            <- countUnfinishedStarts(launchOptions)
      plan = StartupRecovery.plan(
        launchOptions,
        StartupCrashGuard.decide(unfinishedStarts, launchOptions.safeMode),
        configMoved,
        sessionMoved
      )
      // Safe mode never reads the user's config, so a file it cannot parse is not even set aside.
      configResult <-
        if plan.safeMode then IO.pure(Right(ConfigLoadResult(SafeMode.config, ConfigMigrationReport.empty)))
        else ConfigManager.loadConfigResultIO()
      // Only a file that cannot be parsed at all means defaults for the session. It is left exactly as it is, and
      // saving is refused while it stays that way. Anything less costs only the settings that were invalid, and the
      // start page names them: the log is invisible in TUI mode, where stderr goes to nowhere.
      (loaded, configNotice) = ConfigNotice.forOutcome(ConfigManager.defaultConfigPath, configResult)
      _ <- configResult.left.toOption.traverse_(error =>
        logger.error(error.cause.getOrElse(new RuntimeException(error.message)))(s"[CONFIG] ${error.message}")
      )
      _ <- ConfigMigrationWarning
        .message(ConfigManager.defaultConfigPath, loaded.report)
        .fold(IO.unit)(message => logger.warn(message))
      // The warning above already lists deprecated, unknown, removed and invalid entries.
      _ <- loaded.report.diagnostics.collect {
        case diagnostic @ (_: ConfigDiagnostic.ConflictingHotkey | _: ConfigDiagnostic.NewerFileVersion |
            _: ConfigDiagnostic.Migrated) =>
          logger.warn(s"[CONFIG] ${diagnostic.message}")
      }.sequence_
      appConfig = resolveAppConfig(loaded.config, launchOptions)
      startup   = Startup(plan, requestRestart, StartupCrashGuard.markStarted(StartupCrashGuard.defaultMarker))
      // The scratch session of safe mode replaces an isolated one, so only the notice that is still true is shown.
      instanceNotice = Option.unless(plan.safeMode)(role.notice).flatten
      notice         = plan.noticeWith(Option((configNotice.toList ++ instanceNotice).mkString(" ")).filter(_.nonEmpty))
      _ <- safeModeSessionRoot(plan).use { scratchRoot =>
        val session = SessionChoice(scratchRoot.orElse(role.sessionRootOverride), forwardedOpensOf(role))
        if LaunchOptions.resolveTuiMode(launchOptions) then runTui(appConfig, launchOptions, notice, startup, session)
        else runGui(appConfig, launchOptions, notice, startup, session)
      }
      _ <- startup.markStarted
    yield ()

  /** What the recovery plan adds to a launch beyond the config: the plan itself, how to ask for a restart, and what to
    * do once the first frame is painted.
    */
  final private case class Startup(
      plan: StartupRecovery.Plan,
      requestRestart: RestartMode => IO[Unit],
      markStarted: IO[Unit]
  )

  /** A launch already in safe mode only reads the marker: a safe start that dies must not push the next one further. */
  private def countUnfinishedStarts(options: LaunchOptions): IO[Int] =
    if options.safeMode then StartupCrashGuard.peekUnfinishedStarts(StartupCrashGuard.defaultMarker)
    else StartupCrashGuard.recordStartAttempt(StartupCrashGuard.defaultMarker)

  /** Safe mode writes its session into a scratch folder, so the real one is neither read nor replaced. */
  private def safeModeSessionRoot(plan: StartupRecovery.Plan): Resource[IO, Option[Path]] =
    if plan.safeMode then SafeMode.scratchSessionRoot.map(Some(_)) else Resource.pure(None)

  /** An isolated launch does not own the session folder, so a reset must not move what the running instance writes. */
  private def applyResets(role: LaunchRole, options: LaunchOptions, at: Instant): IO[(List[Moved], List[Moved])] =
    for
      config <-
        if options.resetConfig then LaunchReset.backUpConfig(ConfigManager.defaultConfigPath, at) else IO.pure(Nil)
      session <-
        if options.resetSession && role.sessionRootOverride.isEmpty then
          LaunchReset.backUpSession(SessionManager.defaultSessionRoot(), at)
        else IO.pure(Nil)
      _ <- (config ++ session).traverse_(moved => IO(System.err.println(s"Moved ${moved.from} to ${moved.to}")))
    yield (config, session)

  /** Which session this process may write, and the files later launches hand it. */
  final private case class SessionChoice(rootOverride: Option[Path], forwardedOpens: Stream[IO, List[Path]])

  private def forwardedOpensOf(role: LaunchRole): Stream[IO, List[Path]] =
    role match
      case LaunchRole.Primary(forwardedOpens) => forwardedOpens
      case _                                  => Stream.empty

  /** The TUI launch path (issue #1112): a real system terminal via [[TerminalShell.resource]], restored on every exit
    * path by that `Resource`'s release. This branch never references `SwingWindow` -- the terminal capability bundle
    * lives entirely in [[TuiRuntime]], which owns no such reference either.
    */
  private def runTui(
    appConfig: AppConfig,
    launchOptions: LaunchOptions,
    configNotice: Option[String],
    startup: Startup,
    session: SessionChoice
  )(using
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
        configPersistencePath = startup.plan.configPersistencePath(ConfigManager.defaultConfigPath),
        hasDisplay = LaunchOptions.isDisplayReachable(sys.env),
        sessionRootOverride = session.rootOverride,
        configNotice = configNotice,
        recovery = startup.plan,
        restarter = Some(startup.requestRestart),
        onFirstFrame = startup.markStarted,
        forwardedOpens = session.forwardedOpens
      )

  /** The GUI launch path: unchanged from before #1112 beyond being extracted into its own method. Constructs a
    * [[SwingWindow]] and closes `AppRuntime.run`'s capabilities over it; never touches [[TerminalShell]].
    */
  private def runGui(
    appConfig: AppConfig,
    launchOptions: LaunchOptions,
    configNotice: Option[String],
    startup: Startup,
    session: SessionChoice
  )(using
    logger: Logger[IO],
    loggerFactory: LoggerFactory[IO]
  ): IO[Unit] =
    for
      displayState <- RuntimeDisplayState.create(appConfig.editorConfig.fontConfig)
      initialDisplay = displayState.snapshot
      frameTimings   = FrameTimings()
      _ <- (
        SwingWindow.resource(
          initialDisplay.codeMetrics,
          initialDisplay.uiMetrics,
          appConfig.windowChromeMode,
          appConfig.preferredWindowSize,
          frameTimings,
          startup.plan.windowTitle
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
              }
            }

          def syncChromeTheme(state: com.serenity.state.models.AppState): IO[Unit] =
            Trace.timed("render.syncChromeTheme") {
              IO {
                swingWin.updateChromeTheme(state.persisted.theme)
              }
            }

          (AccessibilitySync.empty, Deferred[IO, SwingInputHandler[IO, com.serenity.keystroke.events.Event]]).tupled
            .flatMap { (accessibilitySync, menuInput) =>
              def syncAccessibility(state: com.serenity.state.models.AppState): IO[Unit] =
                Trace.timed("render.syncAccessibility") {
                  accessibilitySync
                    .sync(state)(previous => IO(AccessibilitySnapshot.from(state, swingWin.viewportSize, previous)))
                    .flatMap(snapshot => IO(swingWin.updateAccessibility(snapshot)))
                }

              val frontendRuntime = FrontendRuntime(
                inputHandler = router =>
                  IO.pure(
                    new SwingInputHandler[IO, com.serenity.keystroke.events.Event](
                      swingWin.canvas,
                      router,
                      () => swingWin.metrics,
                      () => displayState.uiMetrics,
                      actualAppConfig.inputConfig.wheelScrollLines,
                      frameTimings
                    )
                  ).flatTap(menuInput.complete(_).void),
                renderFull = (state, vis, cc, damage, caches) =>
                  timedFrame(frameTimings, FrameKind.Full, paintEc)(
                    syncDisplayMetrics() >> syncChromeTheme(state) >> syncAccessibility(state),
                    IO(
                      paintFullFrame(
                        state,
                        vis,
                        cc,
                        swingWin,
                        displayState.snapshot,
                        damage,
                        caches
                      )
                    )
                  ),
                renderCursorOnly = (state, vis, cc, damage, caches) =>
                  timedFrame(frameTimings, FrameKind.CursorOnly, paintEc)(
                    syncDisplayMetrics() >> syncChromeTheme(state) >> syncAccessibility(state),
                    IO(
                      paintCursorFrame(
                        state,
                        vis,
                        cc,
                        swingWin,
                        displayState.snapshot,
                        damage,
                        caches
                      )
                    )
                  ),
                frameTimings = frameTimings,
                menus = Option.when(SwingMenuBar.enabledFor(System.getProperty("os.name", "")))(
                  SwingMenuBar.resource(swingWin, menuInput.get)
                ),
                offscreenFrames = Some(
                  OffscreenWarmUpFrames.forCanvas(
                    swingWin.canvas,
                    () => swingWin.viewportSize,
                    () => displayState.snapshot
                  )
                )
              )

              initialScaleSync >> AppRuntime.run(
                initialViewportSize = swingWin.viewportSize,
                checkResize = IO(swingWin.doResizeIfNecessary()),
                runtime = frontendRuntime,
                appConfig = actualAppConfig,
                configNotice = configNotice,
                recovery = startup.plan,
                onFirstFrame = startup.markStarted,
                makeStateManager = Some(logger =>
                  com.serenity.state.manager.StateManager.apply(
                    logger,
                    policy = SessionManager.SessionPolicy.interactive,
                    sessionRootOverride = session.rootOverride,
                    onFontConfigChanged = config =>
                      displayState.update(config) >>
                        IO.blocking {
                          val display = displayState.snapshot
                          swingWin.updateMetrics(display.codeMetrics, display.uiMetrics)
                        },
                    deviceTextScaleProvider = IO.blocking(swingWin.detectedDeviceTextScale),
                    configPersistencePath = startup.plan.configPersistencePath(ConfigManager.defaultConfigPath),
                    projectTasksEnabled = !startup.plan.safeMode,
                    restarter = Some(startup.requestRestart),
                    uiPresetStore = startup.plan.uiPresetStore(session.rootOverride),
                    windowSizeProvider = IO.blocking(Some(swingWin.currentPreferredWindowSize)),
                    onPreferredWindowSizeChanged = size => IO.blocking(swingWin.resizeToPreferred(size)),
                    fileDialog = Some(SwingFileDialog(swingWin.canvas))
                  )
                ),
                awaitExternalQuit = swingWin.awaitClose,
                registerResizeCallback = cb => swingWin.setOnResize(cb),
                registerFocusCallback = cb => swingWin.setOnFocusChange(cb),
                openPath = launchOptions.openPath,
                frontend = GuiFrontend,
                forwardedOpens = session.forwardedOpens.evalTap(_ => IO(swingWin.bringToFront()))
              )
            }
        }
    yield ()

  /** Syncs window state and draws in a single hop onto the paint thread: each hop is a wait the frame pays for. */
  private def timedFrame(timings: FrameTimings, kind: FrameKind, paintEc: ExecutionContext)(
    sync: IO[Unit],
    draw: IO[Unit]
  ): IO[Unit] =
    IO(timings.renderStarted()) >>
      (timings.timedIO(FramePhase.Sync)(sync) >> draw).evalOn(paintEc).guarantee(IO(timings.renderFinished(kind)))

  private def resolveAutoTextScale(config: AppConfig, detectedTextScale: Double): AppConfig =
    config.withFontConfig(config.editorConfig.fontConfig.resolveAutoTextScale(detectedTextScale))

  /** Applies the eco overlay (if requested via `--eco` or `SERENITY_ECO=1`) and the alpha overlay (if requested via
    * `--alpha`) before the auto text-scale resolution that follows every config load, so their changes are visible to
    * that step just like any other loaded setting. Eco touches only the render fps target; alpha touches only the
    * currently-gated experimental prototype flags (command-runner cursor-peek today) -- the two overlays don't share
    * any field, so application order between them doesn't matter.
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
    cursorColor: Option[RenderColor],
    window: SwingWindow,
    display: RuntimeDisplayState.Snapshot,
    damage: com.serenity.state.models.Damage,
    caches: com.serenity.state.manager.RenderCaches
  ): Unit =
    if cursorVisible then
      val _ = RendererCursorOverlay.renderWithCursorOverlay(
        state,
        window,
        FontSpec.fromAwt(display.codeFont),
        FontSpec.fromAwt(display.textFont),
        FontSpec.fromAwt(display.uiFont),
        display.uiMetrics,
        cursorColor,
        damage,
        caches
      )
      ()
    else
      RendererEntryPoints.render(
        state,
        cursorVisible = false,
        window,
        FontSpec.fromAwt(display.codeFont),
        FontSpec.fromAwt(display.textFont),
        FontSpec.fromAwt(display.uiFont),
        display.uiMetrics,
        None,
        repaintOnFlush = SwingWindow.shouldRepaintBaseFrameBeforeCursorOverlay(cursorVisible),
        damage = damage,
        caches = caches
      )

  /** Repaint only the cursor overlay, falling back to a full frame when the overlay path declines. */
  private def paintCursorFrame(
    state: com.serenity.state.models.AppState,
    cursorVisible: Boolean,
    cursorColor: Option[RenderColor],
    window: SwingWindow,
    display: RuntimeDisplayState.Snapshot,
    damage: com.serenity.state.models.Damage,
    caches: com.serenity.state.manager.RenderCaches
  ): Unit =
    val rendered = RendererCursorOverlay.renderCursorOnly(
      state,
      cursorVisible,
      window,
      FontSpec.fromAwt(display.codeFont),
      FontSpec.fromAwt(display.textFont),
      FontSpec.fromAwt(display.uiFont),
      display.uiMetrics,
      cursorColor,
      caches
    )
    if !rendered then
      RendererEntryPoints.render(
        state,
        cursorVisible,
        window,
        FontSpec.fromAwt(display.codeFont),
        FontSpec.fromAwt(display.textFont),
        FontSpec.fromAwt(display.uiFont),
        display.uiMetrics,
        cursorColor,
        repaintOnFlush = true,
        damage = damage,
        caches = caches
      )
