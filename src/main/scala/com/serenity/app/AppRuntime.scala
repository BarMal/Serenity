package com.serenity.app

import java.nio.file.Path

import scala.concurrent.duration.*

import cats.effect.*
import cats.effect.std.{Dispatcher, Supervisor}
import cats.syntax.apply.*
import cats.syntax.foldable.*
import cats.syntax.parallel.*
import cats.syntax.semigroup.*
import com.serenity.config.{AppConfig, RenderFpsTarget}
import com.serenity.diagnostics.{FrameTimingReport, FrameTimings, KeyLatencyReport}
import com.serenity.frontend.{Frontend, FrontendRuntime}
import com.serenity.input.*
import com.serenity.keystroke.events.Event
import com.serenity.keystroke.translators.TextEntryTranslator
import com.serenity.lsp.LspManager
import com.serenity.state.manager.*
import com.serenity.state.models.{AppState, BufferId, BufferMapChanges, Damage}
import com.serenity.ui.layout.ViewportSize
import com.serenity.ui.renderer.RenderController
import fs2.Stream
import fs2.concurrent.{Signal, SignallingRef}
import org.typelevel.log4cats.{Logger, LoggerFactory}

object AppRuntime:

  /** Issue #1669's remaining scope moved this onto [[FrontendRuntime]], the frontend-owned render/input bundle; kept as
    * an alias so callers outside this file (`AppRuntimeRenderLoops`, `TuiRuntime`) don't need to know it moved.
    * `FrontendRuntime.RenderFn` itself still carries the `RenderCaches` instance as its final argument (#1677's
    * remaining scope), supplied by callers from `stateManager.renderCaches` rather than closed over when the
    * `FrontendRuntime` bundle is built.
    */
  private[serenity] type RenderFn = FrontendRuntime.RenderFn

  private val NanosPerSecond: Long = 1_000_000_000L

  private[serenity] def fastFrameInterval(target: RenderFpsTarget): FiniteDuration =
    FiniteDuration(NanosPerSecond / target.framesPerSecond.toLong, NANOSECONDS)

  /** How long a fast frame waits so it starts no earlier than one interval after the previous frame's start: render
    * time already spent counts towards the interval instead of adding to it, and a frame whose deadline has passed --
    * or the first frame ever -- starts at once.
    */
  private[serenity] def fastFrameDelay(
    frameInterval: FiniteDuration,
    previousFrameStart: Option[FiniteDuration],
    now: FiniteDuration
  ): FiniteDuration =
    previousFrameStart.fold(Duration.Zero)(previous => (previous + frameInterval - now).max(Duration.Zero))

  private[serenity] def resetCursorActivity(cursorVisible: Ref[IO, Boolean]): IO[Unit] =
    cursorVisible.set(true)

  /** React to a Swing window focus transition. Losing focus parks the cursor visible-and-steady (reset to the start of
    * its blink cycle) and forces one fast render so the steady caret paints immediately, regardless of where the idle
    * loop was in its own cadence. Regaining focus flips the signal the idle loop is waiting on --
    * `awaitFocusedIdleTick` picks that up and resumes the normal cadence on its own -- and runs `onFocusGained` (#1623:
    * re-checking the focused buffer's file for external changes), defaulted to a no-op for callers that don't need it
    * (most existing tests).
    */
  private[serenity] def onWindowFocusChanged(
    focused: Boolean,
    windowFocused: SignallingRef[IO, Boolean],
    cursorVisible: Ref[IO, Boolean],
    requestFastRender: IO[Unit],
    onFocusGained: IO[Unit] = IO.unit
  ): IO[Unit] =
    if focused then windowFocused.set(true) >> onFocusGained
    else windowFocused.set(false) >> resetCursorActivity(cursorVisible) >> requestFastRender

  /** The idle loop's per-tick wait: the normal cursor idle cadence while the window is focused, or an indefinite,
    * wakeup-free wait otherwise -- the mechanism that actually stops idle wakeups, rather than merely skipping the
    * render they'd otherwise trigger. Two things can make focused waiting indefinite instead of cadenced:
    * `cursorIdleInterval` returning `None` (#1170's TUI-blink caret delegation --
    * [[com.serenity.frontend.Frontend.cursorIdleInterval]]), racing here against [[Stream.interruptWhen]]'s
    * `fastModeSignal` in [[idleRenderPhase]] so a real input event still wakes it immediately -- and losing focus
    * entirely, which waits on `windowFocused` turning true again instead.
    */
  private[serenity] def awaitFocusedIdleTick(
    loadState: IO[AppState],
    windowFocused: SignallingRef[IO, Boolean],
    cursorIdleInterval: AppConfig => Option[FiniteDuration]
  ): IO[Unit] =
    windowFocused.get.flatMap {
      case true =>
        loadState.flatMap { state =>
          cursorIdleInterval(state.persisted.config) match
            case Some(interval) => IO.sleep(interval)
            case None           => IO.never
        }
      case false =>
        windowFocused.discrete.find(identity).compile.drain
    }

  /** Whether the caret blinks on after `blinks` idle ticks without input, or has blinked through the configured cursor
    * blink timeout and should hold solid (#1883).
    */
  private[serenity] def keepsBlinking(config: AppConfig, blinkInterval: FiniteDuration, blinks: Int): Boolean =
    config.cursorBlinkTimeout.forall(timeout => blinkInterval * blinks.toLong < timeout)

  /** The fast phase stands down once no fresh damage arrived while it was running -- `pendingDamage` is drained to
    * `Damage.Nothing` when the phase starts, so any non-`Nothing` value here means `emitDamage` was called again since,
    * and the loop should carry straight on to another frame rather than idle.
    */
  private[serenity] def shouldClearFastMode(pendingDamage: Damage): Boolean =
    pendingDamage == Damage.Nothing

  /** Only input, resize and focus changes call `emitDamage` themselves; this covers every other commit -- language
    * server diagnostics, an async open, task output, find results -- so it shows without waiting for the next key. A
    * commit that changes nothing on screen stays silent, so an idle editor stays idle.
    */
  private[serenity] def wakeRenderLoopOnCommit(
    emitDamage: Damage => IO[Unit]
  )(using com.serenity.rope.Balance): (AppState, AppState) => IO[Unit] =
    (before, after) =>
      val damage = DamageProducer.forTransition(before, after)
      IO.whenA(damage != Damage.Nothing)(emitDamage(damage))

  final private[serenity] case class RuntimeFailure(
      loopName: String,
      phase: String,
      diagnostics: String,
      cause: Throwable
  ) extends RuntimeException(s"$loopName failed in phase=$phase; $diagnostics", cause)

  def run(
    initialViewportSize: ViewportSize,
    checkResize: IO[Option[ViewportSize]],
    runtime: FrontendRuntime,
    appConfig: AppConfig,
    makeStateManager: Option[Logger[IO] => IO[StateManager]] = None,
    awaitExternalQuit: IO[Unit] = IO.never,
    registerResizeCallback: (() => Unit) => Unit = _ => (),
    registerFocusCallback: (Boolean => Unit) => Unit = _ => (),
    registerMarkdownPreviewCloseCallback: (() => Unit) => Unit = _ => (),
    openPath: Option[Path] = None,
    systemClipboard: SystemClipboard[IO] = SystemClipboard.awt[IO],
    frontend: Frontend = com.serenity.frontend.GuiFrontend,
    configNotice: Option[String] = None,
    recovery: StartupRecovery.Plan = StartupRecovery.Plan.normal,
    onFirstFrame: IO[Unit] = IO.unit,
    forwardedOpens: Stream[IO, List[Path]] = Stream.empty
  )(using logger: Logger[IO], loggerFactory: LoggerFactory[IO], balance: com.serenity.rope.Balance): IO[Unit] =
    (Dispatcher.parallel[IO], Supervisor[IO](await = false)).tupled.use { (resizeCallbackDispatcher, timerSupervisor) =>
      for
        _ <- logger.info("Starting Serenity text editor")
        themeManager = com.serenity.ui.theme.config.AppThemeManager.create
        stateManager <- makeStateManager.getOrElse(logger => StateManager.apply(logger, initialConfig = appConfig))(
          logger
        )
        startupTheme <- AppStartup.startupTheme(stateManager.sessionStartupInfo, themeManager)
        initialState <- AppStartup.initializeState(
          stateManager,
          stateManager.sessionStartupInfo,
          startupTheme,
          initialViewportSize,
          appConfig,
          openPath,
          frontend.capabilities,
          configNotice,
          recovery
        )
        inputRouter  <- InputRouter.create[IO, Event](new TextEntryTranslator(appConfig))
        inputHandler <- runtime.inputHandler(inputRouter)
        _            <- inputRouter.setActiveTranslator(FocusedInputTranslator.forState(initialState))
        _ <- inputRouter.setCursorPeekEnabled(
          initialState.persisted.config.surfaceConfig.commandRunnerCursorPeekEnabled
        )
        fastModeSignal <- SignallingRef.of[IO, Boolean](false)
        pendingDamage  <- Ref.of[IO, Damage](Damage.Nothing)
        // Separate from pendingDamage: that ref answers "did more damage arrive while the fast phase ran" (see
        // shouldClearFastMode). This one answers "what has changed since the last frame was actually drawn," and is
        // drained by every render call.
        pendingPaintDamage <- Ref.of[IO, Damage](Damage.Nothing)
        emitDamage = (damage: Damage) =>
          pendingDamage.update(_ |+| damage) >> pendingPaintDamage.update(_ |+| damage) >> fastModeSignal.set(true)
        // The resize/idle-recovery paths don't have a before/after AppState to diff, so they report the coarsest
        // damage rather than none -- inputEventPhase is the one caller that reports real per-event damage.
        requestFastRender = emitDamage(Damage.Everything)
        typingQuietTimer <- TypingQuietTimer.create(timerSupervisor, TypingQuietTimer.expireIn(stateManager))
        frameTimingEnabled <- SignallingRef.of[IO, Boolean](
          initialState.persisted.config.surfaceConfig.frameTimingEnabled
        )
        latencyTraceEnabled <- SignallingRef.of[IO, Boolean](
          initialState.persisted.config.surfaceConfig.latencyTraceEnabled
        )
        watchInputs <- SignallingRef.of[IO, Long](0L)
        wakeOnCommit = wakeRenderLoopOnCommit(emitDamage)
        observeTransition = (before: AppState, after: AppState) =>
          typingQuietTimer.onCommit(before, after) >> followFrameTimingSetting(frameTimingEnabled)(before, after) >>
            followLatencyTraceSetting(latencyTraceEnabled)(before, after) >>
            IO.whenA(watchInputsChanged(before, after))(watchInputs.update(_ + 1))
        _ <- stateManager.runtimeLifecycle.observeCommits((before, after) =>
          wakeOnCommit(before, after) >> observeTransition(before, after)
        )
        _             <- IO(registerResizeCallback(resizeCallbackBridge(requestFastRender, resizeCallbackDispatcher)))
        cursorVisible <- Ref.of[IO, Boolean](true)
        windowFocused <- SignallingRef.of[IO, Boolean](true)
        _ <- IO(
          registerFocusCallback(
            focusCallbackBridge(
              windowFocused,
              cursorVisible,
              requestFastRender,
              resizeCallbackDispatcher,
              stateManager.fileService.checkExternalChangesOnFocus
            )
          )
        )
        _ <- IO(
          registerMarkdownPreviewCloseCallback(
            markdownPreviewCloseCallbackBridge(stateManager, resizeCallbackDispatcher)
          )
        )
        lastFastFrameStart <- Ref.of[IO, Option[FiniteDuration]](None)
        translatorCache    <- Ref.of[IO, Option[AppRuntimeRenderLoops.FocusedTranslatorCacheEntry]](None)
        currentStateForDiagnostics = stateManager.getCurrentState.map(Some(_))
        checkResizeAndHandle = checkResize.flatMap(RenderController.handleResize(_, stateManager, requestFastRender))
        firstInput <- Deferred[IO, Unit]
        inputFunnel = AppRuntimeRenderLoops.inputBatchPhase(
          AppRuntimeRenderLoops.InputBatchContext(
            stateManager,
            inputRouter,
            systemClipboard,
            checkResizeAndHandle,
            cursorVisible,
            emitDamage,
            translatorCache,
            runtime.frameTimings,
            onUserInput = firstInput.complete(()).void,
            observeBatch = observeTransition,
            logEvent = (event, focus) => AppRuntimeLogging.logSelectiveEvents(event, focus, logger)
          )
        )
        inputLoop = runInputLoop(stateManager, inputHandler.inputBatches.through(inputFunnel))
        _ <-
          Resource.make(inputLoop.start)(_.cancel).use { inputFiber =>
            runtime.renderFull(initialState, true, None, Damage.Everything, stateManager.renderCaches) >>
              onFirstFrame >>
              logger.info("Initial render completed, starting main loop") >>
              startupWarmUp(runtime, initialState, initialViewportSize, firstInput).surround {
                val idlePhase = AppRuntimeRenderLoops.idleRenderPhase(
                  loadModel = stateManager.getModel,
                  fastModeSignal = fastModeSignal,
                  windowFocused = windowFocused,
                  pendingPaintDamage = pendingPaintDamage,
                  currentStateForDiagnostics = currentStateForDiagnostics,
                  checkResizeAndHandle = checkResizeAndHandle,
                  cursorVisible = cursorVisible,
                  renderCursorOnly = runtime.renderCursorOnly,
                  requestFastRender = requestFastRender,
                  cursorIdleInterval = frontend.cursorIdleInterval,
                  renderCaches = stateManager.renderCaches
                )

                val fastPhase = AppRuntimeRenderLoops.fastRenderPhase(
                  stateManager,
                  fastModeSignal,
                  pendingDamage,
                  pendingPaintDamage,
                  currentStateForDiagnostics,
                  checkResizeAndHandle,
                  runtime.renderFull,
                  stateManager.renderCaches,
                  lastFrameStart = lastFastFrameStart,
                  keyLatency = runtime.frameTimings.keyLatency
                )

                val renderLoop = AppRuntimeRenderLoops.renderLoop(idlePhase, fastPhase)

                com.serenity.io.FileChangeWatcher.create.use(watcher =>
                  runRuntimeLoops(
                    stateManager,
                    inputHandler,
                    inputFiber.joinWithNever,
                    renderLoop,
                    watcher,
                    awaitExternalQuit,
                    appConfig,
                    runtime.frameTimings,
                    frameTimingEnabled,
                    latencyTraceEnabled,
                    watchInputs.discrete.as(()),
                    windowFocused,
                    forwardedOpens,
                    recovery.crashRecorder
                  )
                )
              }
          }
        _ <- logger.info("Serenity editor shutdown complete")
      yield ()
    }

  private def runRuntimeLoops(
    stateManager: StateManager,
    inputHandler: InputHandler[IO],
    awaitInputLoop: IO[Unit],
    renderLoop: Stream[IO, Unit],
    fileChangeWatcher: com.serenity.io.FileChangeWatcher,
    awaitExternalQuit: IO[Unit],
    appConfig: AppConfig,
    frameTimings: FrameTimings,
    frameTimingEnabled: SignallingRef[IO, Boolean],
    latencyTraceEnabled: SignallingRef[IO, Boolean],
    watchInputsChanges: Stream[IO, Unit],
    windowFocused: Signal[IO, Boolean],
    forwardedOpens: Stream[IO, List[Path]],
    recordCrash: StartupRecovery.CrashRecorder
  )(using logger: Logger[IO]): IO[Unit] =
    val (lifecycle, quitSignal) = (stateManager.runtimeLifecycle, stateManager.runtimeLifecycle.awaitQuit.attempt)
    (
      awaitInputLoop,
      AppRuntimeRenderLoops.superviseLoop("render loop", lifecycle.forceQuit, recordCrash)(
        renderLoop.interruptWhen(quitSignal).compile.drain
      ),
      lifecycle.awaitQuit,
      AppRuntimeRenderLoops.superviseLoop("interval save loop", lifecycle.forceQuit, recordCrash)(
        lifecycle.intervalSaveStream.compile.drain
      ),
      AppRuntimeRenderLoops.superviseLoop("external quit coordinator", lifecycle.forceQuit, recordCrash)(
        coordinateExternalQuit(awaitExternalQuit, lifecycle.forceQuit, lifecycle.awaitQuit)
      ),
      AppRuntimeRenderLoops.superviseLoop("input shutdown", lifecycle.forceQuit, recordCrash)(
        shutdownInputAfterQuit(lifecycle.awaitQuit, inputHandler.shutdown)
      ),
      AppRuntimeRenderLoops.superviseLoop("LSP loop", lifecycle.forceQuit, recordCrash)(
        LspManager.run(
          stateManager.lspEffectSource.lspEffectStream,
          stateManager.applyEvent,
          logger,
          appConfig.languageToolsConfig.lspUserConfig
        )
      ),
      AppRuntimeRenderLoops.superviseLoop("external change watch loop", lifecycle.forceQuit, recordCrash)(
        externalChangeWatchLoop(
          fileChangeWatcher,
          stateManager.fileService.openBufferPaths,
          stateManager.fileService.checkBufferForExternalChanges,
          stateManager.fileService.dictionaryWatchDirectories,
          stateManager.fileService.refreshDictionaryFingerprints,
          stateManager.fileService.explorerWatchDirectories,
          stateManager.fileService.markExplorerDirectoriesStale,
          watchInputsChanges,
          windowFocused = windowFocused
        ).interruptWhen(quitSignal).compile.drain
      ),
      AppRuntimeRenderLoops.superviseLoop("frame timing report", lifecycle.forceQuit, recordCrash)(
        FrameTimingReport
          .stream(frameTimings, frameTimingEnabled.discrete, line => logger.info(line))
          .interruptWhen(quitSignal)
          .compile
          .drain
      ),
      AppRuntimeRenderLoops.superviseLoop("latency trace report", lifecycle.forceQuit, recordCrash)(
        KeyLatencyReport
          .stream(frameTimings.keyLatency, latencyTraceEnabled.discrete, line => logger.info(line))
          .interruptWhen(quitSignal)
          .compile
          .drain
      ),
      AppRuntimeRenderLoops.superviseLoop("forwarded open loop", lifecycle.forceQuit, recordCrash)(
        forwardedOpens.evalMap(openForwarded(stateManager.fileOpener)).interruptWhen(quitSignal).compile.drain
      )
    ).parMapN((_, _, _, _, _, _, _, _, _, _, _) => ())

  /** Opens what a later launch handed over (#2023). A file that fails to open must not take the editor down with it. */
  private[serenity] def openForwarded(fileOpener: FileOpener)(paths: List[Path])(using logger: Logger[IO]): IO[Unit] =
    paths.traverse_ { path =>
      fileOpener
        .openFile(path)
        .handleErrorWith(error => logger.error(error)(s"[INSTANCE] Could not open forwarded file $path"))
    }

  /** Publishes `ui.render.frame_timing` only when a commit changes it, so the report stream sleeps through ordinary
    * edits.
    */
  private[serenity] def followFrameTimingSetting(
    frameTimingEnabled: SignallingRef[IO, Boolean]
  )(before: AppState, after: AppState): IO[Unit] =
    val enabled = after.persisted.config.surfaceConfig.frameTimingEnabled
    IO.whenA(enabled != before.persisted.config.surfaceConfig.frameTimingEnabled)(frameTimingEnabled.set(enabled))

  /** Publishes `ui.render.latency_trace` only when a commit changes it. */
  private[serenity] def followLatencyTraceSetting(
    latencyTraceEnabled: SignallingRef[IO, Boolean]
  )(before: AppState, after: AppState): IO[Unit] =
    val enabled = after.persisted.config.surfaceConfig.latencyTraceEnabled
    IO.whenA(enabled != before.persisted.config.surfaceConfig.latencyTraceEnabled)(latencyTraceEnabled.set(enabled))

  /** Whether a commit may have changed what [[externalChangeWatchLoop]] watches: the open files, the spell-check
    * dictionaries, or the docked explorers. Errs towards yes; the loop re-derives the set and a no-op resync is free.
    */
  private[serenity] def watchInputsChanged(before: AppState, after: AppState): Boolean =
    (before.runtime.uiSurfaces ne after.runtime.uiSurfaces) ||
      before.persisted.config.languageToolsConfig.spellCheck != after.persisted.config.languageToolsConfig.spellCheck ||
      before.persisted.buffers.size != after.persisted.buffers.size ||
      BufferMapChanges.anyChanged(before.persisted.buffers, after.persisted.buffers)(
        added = _ => true,
        changed = _.document.filePath != _.document.filePath
      )

  private enum WatchResync:
    case SetChanged, FocusLost, FocusRegained

  /** Background half of external-change detection (#1623), complementing the focus-in re-check. Watches the parent
    * directories of open local buffers, `dictionaryWatchDirectories` (#1691) and `explorerWatchDirectories`, re-derived
    * on each `watchedSetChanges` element. A changed buffer file gets the same reload-or-prompt check the focus-in path
    * runs; a change under a dictionary directory refreshes the dictionary fingerprints; a changed explorer directory is
    * marked stale.
    *
    * Event-driven (#1938): with something watched the loop blocks in the watcher until a change arrives, and with
    * nothing watched it never calls the watcher at all, waiting for the set to change. Either way it does not wake on a
    * timer. Never calling the genuinely blocking watcher while nothing is watched also keeps a buffer-less startup
    * compatible with virtual-time tests (`VirtualTime.runVirtual`'s `TestControl` treats `IO.blocking` as
    * non-terminating). Changes are gathered for `settle` after the first, so a burst checks each file once (#1885).
    *
    * While `windowFocused` is false nothing is watched, so a polling backend stops listing directories and a window in
    * the background costs nothing. Regaining focus re-registers the watched set and checks every open buffer, the
    * dictionaries and the explorers once, since changes made meanwhile were never observed.
    */
  private[serenity] def externalChangeWatchLoop(
    watcher: com.serenity.io.FileChangeWatcher,
    openBufferPaths: IO[Map[Path, BufferId]],
    checkBufferForExternalChanges: BufferId => IO[Unit],
    dictionaryWatchDirectories: IO[Set[Path]] = IO.pure(Set.empty),
    refreshDictionaryFingerprints: IO[Unit] = IO.unit,
    explorerWatchDirectories: IO[Set[Path]] = IO.pure(Set.empty),
    markExplorerDirectoriesStale: Set[Path] => IO[Unit] = _ => IO.unit,
    watchedSetChanges: Stream[IO, Unit] = Stream.emit(()),
    settle: FiniteDuration = 200.millis,
    windowFocused: Signal[IO, Boolean] = Signal.constant[IO, Boolean](true)
  ): Stream[IO, Unit] =
    val watched = (openBufferPaths, dictionaryWatchDirectories, explorerWatchDirectories).mapN {
      (paths, dictionaryDirectories, explorerDirectories) =>
        paths.keySet.flatMap(path => Option(path.getParent)) ++ dictionaryDirectories ++ explorerDirectories
    }
    val react = (changed: Set[Path]) =>
      (openBufferPaths, dictionaryWatchDirectories, explorerWatchDirectories).mapN {
        (paths, dictionaryDirectories, explorerDirectories) =>
          val changedDirectories = changed.flatMap(path => Option(path.getParent))
          val staleExplorers     = changedDirectories.intersect(explorerDirectories)
          changed.flatMap(paths.get).toList.traverse_(checkBufferForExternalChanges) >>
            IO.whenA(changedDirectories.exists(dictionaryDirectories.contains))(refreshDictionaryFingerprints) >>
            IO.whenA(staleExplorers.nonEmpty)(markExplorerDirectoriesStale(staleExplorers))
      }.flatten
    def syncWatched(watching: SignallingRef[IO, Boolean]) =
      watched.flatMap(directories => watcher.sync(directories) >> watching.set(directories.nonEmpty))
    val recheckEverything = (openBufferPaths, dictionaryWatchDirectories, explorerWatchDirectories).mapN {
      (paths, dictionaryDirectories, explorerDirectories) =>
        paths.values.toList.traverse_(checkBufferForExternalChanges) >>
          IO.whenA(dictionaryDirectories.nonEmpty)(refreshDictionaryFingerprints) >>
          IO.whenA(explorerDirectories.nonEmpty)(markExplorerDirectoriesStale(explorerDirectories))
    }.flatten
    val focusChanges = windowFocused.discrete.changes.zipWithPrevious.collect {
      case (_, false)          => WatchResync.FocusLost
      case (Some(false), true) => WatchResync.FocusRegained
    }
    Stream.eval(SignallingRef.of[IO, Boolean](false)).flatMap { watching =>
      val resync = watchedSetChanges.as(WatchResync.SetChanged).merge(focusChanges).evalMap {
        case WatchResync.SetChanged =>
          windowFocused.get.flatMap(focused => IO.whenA(focused)(syncWatched(watching)))
        case WatchResync.FocusLost     => watcher.sync(Set.empty) >> watching.set(false)
        case WatchResync.FocusRegained => syncWatched(watching) >> recheckEverything
      }
      watching.discrete.changes
        .switchMap(active =>
          if active then Stream.repeatEval(watcher.awaitChangedFiles(settle)).evalMap(react) else Stream.empty
        )
        .concurrently(resync)
    }

  /** Runs [[StartupWarmUp]] in the background for as long as the main loop does, if the frontend can draw off-screen
    * and `startup.warm_up` is on.
    */
  private def startupWarmUp(
    runtime: FrontendRuntime,
    initialState: AppState,
    viewport: ViewportSize,
    firstInput: Deferred[IO, Unit]
  )(using logger: Logger[IO], balance: com.serenity.rope.Balance): Resource[IO, Unit] =
    runtime.offscreenFrames.filter(_ => initialState.persisted.config.surfaceConfig.startupWarmUpEnabled) match
      case None => Resource.unit
      case Some(frames) =>
        StartupWarmUp
          .run(initialState.persisted.config, initialState.persisted.theme, viewport, frames, firstInput)
          .timed
          .flatMap((elapsed, outcome) => logger.info(s"[WARMUP] $outcome after ${elapsed.toMillis}ms"))
          .handleErrorWith(error => logger.warn(error)("[WARMUP] Startup warm-up failed"))
          .background
          .map(_ => ())

  private def runInputLoop(stateManager: StateManager, inputLoop: Stream[IO, Unit])(using
    logger: Logger[IO]
  ): IO[Unit] =
    val quitSignal = stateManager.runtimeLifecycle.awaitQuit.attempt
    AppRuntimeRenderLoops.superviseLoop("input loop", stateManager.runtimeLifecycle.forceQuit)(
      inputLoop.interruptWhen(quitSignal).compile.drain
    )

  private[serenity] def coordinateExternalQuit(
    awaitExternalQuit: IO[Unit],
    forceQuit: IO[Unit],
    awaitQuit: IO[Unit]
  ): IO[Unit] =
    IO.race(awaitExternalQuit >> forceQuit, awaitQuit).void

  private[serenity] def shutdownInputAfterQuit(awaitQuit: IO[Unit], shutdownInput: IO[Unit]): IO[Unit] =
    awaitQuit >> shutdownInput

  /** Dispatches an AWT/JLine callback's effect onto the runtime, tolerating the dispatcher having already shut down.
    *
    * On quit the runtime's `Dispatcher` closes while native callbacks (WINCH resize, window focus-lost) can still fire.
    * `unsafeRunAndForget` then throws `IllegalStateException: Dispatcher already closed` synchronously -- before the
    * effect's own error handler can run -- onto the AWT/pump thread, where `CrashReporter` logs it as a crash. A
    * callback arriving after shutdown has nothing left to do, so swallow only that specific closed-dispatcher state and
    * let every other failure propagate.
    */
  private def dispatchIfRunning(dispatcher: Dispatcher[IO])(effect: IO[Unit]): Unit =
    try dispatcher.unsafeRunAndForget(effect)
    catch case _: IllegalStateException => ()

  private[serenity] def resizeCallbackBridge(
    signalResize: IO[Unit],
    dispatcher: Dispatcher[IO]
  )(using logger: Logger[IO]): () => Unit =
    () =>
      dispatchIfRunning(dispatcher)(
        signalResize.handleErrorWith(error => logger.error(error)("[RUNTIME] resize callback failed"))
      )

  private[serenity] def focusCallbackBridge(
    windowFocused: SignallingRef[IO, Boolean],
    cursorVisible: Ref[IO, Boolean],
    requestFastRender: IO[Unit],
    dispatcher: Dispatcher[IO],
    onFocusGained: IO[Unit] = IO.unit
  )(using logger: Logger[IO]): Boolean => Unit =
    focused =>
      dispatchIfRunning(dispatcher)(
        onWindowFocusChanged(focused, windowFocused, cursorVisible, requestFastRender, onFocusGained)
          .handleErrorWith(error => logger.error(error)("[RUNTIME] focus callback failed"))
      )

  /** Bridges the TUI's spawned Markdown preview window (issue #1113) closing via its own native close control back into
    * application state: the window only hides itself (see `MarkdownPreviewWindow.resource`), so this callback's sole
    * job is toggling `markdownPreviewWindowBuffer` back off rather than orphaning a dead window reference.
    */
  private[serenity] def markdownPreviewCloseCallbackBridge(
    stateManager: StateUpdater,
    dispatcher: Dispatcher[IO]
  )(using logger: Logger[IO]): () => Unit =
    () =>
      dispatcher.unsafeRunAndForget(
        stateManager
          .updateStateValidated(closeMarkdownPreviewWindowInState)
          .handleErrorWith(error => logger.error(error)("[RUNTIME] markdown preview close callback failed"))
      )

  private[serenity] def closeMarkdownPreviewWindowInState(state: AppState): AppState =
    state.copy(runtime = state.runtime.copy(markdownPreviewWindowBuffer = None))

  private[serenity] def describeStateForDiagnostics(state: AppState): String =
    val viewport   = state.runtime.viewportSize.map(size => s"${size.width}x${size.height}").getOrElse("unknown")
    val activePane = state.persisted.layout.activeEditorPaneId
    val activeBuffer =
      activePane.flatMap(paneId =>
        state.persisted.layout.editorPanes.get(paneId).flatMap(_.bufferId).flatMap(state.persisted.buffers.get)
      )
    val activeBufferSummary = activeBuffer match
      case Some(buffer) =>
        val language = buffer.document.language.map(_.id).getOrElse("plaintext")
        val cursor   = buffer.editing.cursorPositions.headOption.map(c => s"${c.line}:${c.column}").getOrElse("none")
        List(
          s"activeBuffer=${buffer.id}",
          s"chars=${buffer.document.content.weight}",
          s"lines=${buffer.document.content.lineCount}",
          s"dirty=${buffer.document.isDirty}",
          s"language=$language",
          s"cursor=$cursor"
        ).mkString(" ")
      case None =>
        "activeBuffer=none"
    List(
      s"focus=${state.persisted.focus}",
      s"viewport=$viewport",
      s"buffers=${state.persisted.buffers.size}",
      s"panes=${state.persisted.layout.editorPanes.size}",
      s"surfaces=${state.runtime.uiSurfaces.size}",
      s"activePane=${activePane.map(_.toString).getOrElse("none")}",
      activeBufferSummary
    ).mkString(" ")
