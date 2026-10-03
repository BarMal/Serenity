package com.serenity.state.manager

import java.nio.file.Path

import scala.concurrent.duration.*

import cats.effect.*
import cats.syntax.all.*
import com.serenity.command.{CommandRegistry, CommandRunner}
import com.serenity.config.{SpellCheckConfig, SpellCheckDictionaryFingerprint}
import com.serenity.diagnostics.Trace
import com.serenity.document.CommentRendering
import com.serenity.io.{FileBrowser, FileEntry}
import com.serenity.lsp.client.DocumentUri
import com.serenity.spellcheck.{DictionaryCache, DictionaryLoader, SpellChecker}
import com.serenity.state.core.NotesPaneSync
import com.serenity.state.effects.{EffectLanes, Lane, LaneKey, LanePolicy}
import com.serenity.state.models.*
import com.serenity.ui.layout.{DirEntry, WrappedLineCache}
import org.typelevel.log4cats.Logger

/** Operations emitted by capabilities for ordered interpretation at the event boundary. */
private[manager] enum StateManagerOperation:
  case Event(event: com.serenity.keystroke.events.Event)

/** One-directional hand-off for operations emitted while interpreting effects. */
final private[manager] class StateManagerOperationBoundary private (
    pendingOperations: Ref[IO, List[StateManagerOperation]],
    modelRef: Ref[IO, Model],
    documentAnalysisInputsRef: Ref[IO, Option[Map[DocumentUri, SpellCheckFingerprint]]],
    dictionaryFingerprintsRef: Ref[IO, Option[(SpellCheckConfig, List[SpellCheckDictionaryFingerprint])]],
    logger: Logger[IO],
    val effectLanes: EffectLanes,
    releaseEffectLanes: IO[Unit],
    effectsShutdownRef: Ref[IO, Boolean],
    effectsShutDown: Deferred[IO, Unit],
    submittedEffects: Ref[IO, Long],
    beforeDocumentAnalysisStart: IO[Unit],
    beforeEffectsShutdown: IO[Unit],
    dispatcher: StateManagerDispatcher,
    fileWriteLedger: FileWriteLedger,
    discoverDictionaryFingerprints: SpellCheckConfig => IO[List[SpellCheckDictionaryFingerprint]],
    dictionaryCache: DictionaryCache,
    listDirectory: Path => IO[List[DirEntry]],
    commitObserver: Ref[IO, (AppState, AppState) => IO[Unit]],
    wrapCache: WrappedLineCache,
    commitsUnobserved: Ref[IO, Boolean]
):
  private val DocumentAnalysisDebounce         = 150.millis
  private val FindSearchDebounce               = 50.millis
  private val MarkdownPreviewCommitDebounce    = 150.millis
  private val OutlineRefreshDebounce           = 150.millis
  private val FindSearchLane: Lane.Keyed       = Lane.Keyed(LaneKey.Search, LanePolicy.SwitchLatest)
  private val OutlineRefreshLane: Lane.Keyed   = Lane.Keyed(LaneKey.OutlineRefresh, LanePolicy.SwitchLatest)
  private val DocumentAnalysisLane: Lane.Keyed = Lane.Keyed(LaneKey.Analysis, LanePolicy.SwitchLatest)
  private val ShutdownGracePeriod              = 5.seconds

  // Built here, over the dispatcher's own model ref, because every commit it makes runs this boundary's follow-up work.
  val modelCommit: ModelCommit = new ModelCommit(modelRef, this, wrapCache)

  def enqueueEvent(event: com.serenity.keystroke.events.Event): IO[Unit] =
    pendingOperations.update(_ :+ StateManagerOperation.Event(event))

  def takeOperations: IO[List[StateManagerOperation]] = pendingOperations.getAndSet(Nil)

  /** Runs `request` on the single state dispatcher and waits for it (#1570, #1697): a dispatch commits a state built
    * from a snapshot read at its own start, so it must never overlap another writer. Never call this from code already
    * running on the dispatcher -- see `StateManagerDispatcher.submit`.
    */
  def dispatch[A](request: IO[A]): IO[A] = dispatcher.submit(request)

  def ensureCommandRunnerSurface(state: AppState): AppState =
    val registry = CommandRegistry.default
    val activatedRunner =
      CommandRunner.empty.activate(
        registry,
        state.persisted.config,
        state.runtime.capabilities,
        state.commandRunnerContext
      )
    val runner = activatedRunner
    val (stateWithId, surfaceId) =
      state.commandRunnerSurface.map(surface => (state, surface.id)).getOrElse(state.allocateSurfaceId)
    val surface = UiSurface(
      id = surfaceId,
      content = SurfaceContent.CommandPalette(runner),
      presentation = SurfacePresentation.Floating(state.activeCursorPosition, SurfacePlacement.BelowCursor)
    )
    stateWithId
      .copy(runtime =
        stateWithId.runtime
          .copy(uiSurfaces = stateWithId.runtime.uiSurfaces.movedToEndWhere(_.id == surfaceId)(surface))
      )
      .pushFocus(Focus.Surface(surfaceId))

  private def currentModalId(state: AppState): Option[SurfaceId] =
    state.topModal.map(_.id).orElse(state.modalSurface.map(_.id))

  /** The follow-up work of every `ModelCommit` app-state commit. */
  private[manager] def afterCommit(fallbackState: AppState, committedState: AppState): IO[Unit] =
    logModalTransition(fallbackState, committedState) >> scheduleDocumentAnalysis() >>
      PanelContentSync.outlineRefreshDue(committedState, fallbackState).traverse_(scheduleOutlineRefresh) >>
      PanelContentSync.explorerListingsDue(committedState, fallbackState).traverse_(listExplorerDirectory) >>
      commitsUnobserved.get.ifM(IO.unit, commitObserver.get.flatMap(_(fallbackState, committedState)))

  /** Replaces the observer told of every commit `afterCommit` follows up, with the states before and after. */
  def observeCommits(observer: (AppState, AppState) => IO[Unit]): IO[Unit] =
    commitObserver.set(observer)

  /** Runs `work` with its commits kept from the commit observer, for a caller that reports their damage itself. Only
    * sound on the dispatcher, where no other writer's commit can land while `work` runs.
    */
  def unobserved[A](work: IO[A]): IO[A] =
    commitsUnobserved.set(true).bracket(_ => work)(_ => commitsUnobserved.set(false))

  private[manager] def logRejectedCommit(errors: List[String]): IO[Unit] =
    logger.error(s"State validation failed: ${errors.mkString(", ")}")

  private def logModalTransition(before: AppState, after: AppState): IO[Unit] =
    (currentModalId(before), currentModalId(after)) match
      case (beforeId, afterId) if beforeId != afterId =>
        logger.info(
          s"[STATE MODAL] before=${beforeId.getOrElse("none")} " +
            s"after=${afterId.getOrElse("none")} focus=${after.persisted.focus}"
        )
      case _ => IO.unit

  /** #1691: `discoverDictionaryFingerprints` is a real filesystem stat per candidate dictionary path, so this only
    * calls it when there is anything to check (`requiresDocumentAnalysis`) and reuses `dictionaryFingerprintsRef`'s
    * cached value for as long as the spell-check config it was discovered from stays the same -- a state commit (this
    * method's caller, via `afterCommit`) fires on every keystroke, but the dictionaries themselves essentially never
    * change mid-session. `refreshDictionaryFingerprints` is the intentional cache-busting signal for when they do.
    */
  def scheduleDocumentAnalysis(): IO[Unit] =
    modelCommit.currentState.flatMap { state =>
      if !requiresDocumentAnalysis(state) then IO.unit
      else
        val spellCheckConfig = state.persisted.config.languageToolsConfig.spellCheck
        cachedDictionaryFingerprints(spellCheckConfig).flatMap { dictionaryFingerprints =>
          val inputs = SpellChecker.analysisFingerprints(state, dictionaryFingerprints)
          documentAnalysisInputsRef.modify(previous => Some(inputs) -> previous.forall(_ != inputs)).flatMap {
            inputsChanged =>
              if !inputsChanged then IO.unit
              else
                effectsShutdownRef.get.ifM(
                  IO.unit,
                  beforeDocumentAnalysisStart >> submit(DocumentAnalysisLane, documentAnalysisJob)
                )
          }
        }
    }

  /** The dictionary fingerprints for `config`, from `dictionaryFingerprintsRef` when they were last discovered from
    * this same (normalized) config, otherwise freshly discovered and cached. Keying the cache on the config itself
    * means a spell-check config change (new `dictionaryPaths`, languages, or enabling it at all) always gets a fresh
    * discovery -- only an on-disk dictionary edit under an unchanged config can leave this stale, which is exactly what
    * `refreshDictionaryFingerprints` exists to correct.
    */
  private def cachedDictionaryFingerprints(config: SpellCheckConfig): IO[List[SpellCheckDictionaryFingerprint]] =
    val normalized = config.normalized
    dictionaryFingerprintsRef.get.flatMap {
      case Some((cachedConfig, fingerprints)) if cachedConfig == normalized => IO.pure(fingerprints)
      case _ => discoverAndCacheDictionaryFingerprints(normalized)
    }

  private def discoverAndCacheDictionaryFingerprints(
    normalized: SpellCheckConfig
  ): IO[List[SpellCheckDictionaryFingerprint]] =
    discoverDictionaryFingerprints(normalized).flatTap(fingerprints =>
      dictionaryFingerprintsRef.set(Some(normalized -> fingerprints))
    )

  /** Forces a fresh dictionary-fingerprint discovery (#1691). The primary trigger is the `FileChangeWatcher`-backed
    * watch loop (`AppRuntime.externalChangeWatchLoop`, via `FileService.dictionaryWatchDirectories`/
    * `refreshDictionaryFingerprints`) noticing an on-disk change under a watched dictionary directory in real time;
    * window focus-gain (`StateManagerFileCapability`'s `checkExternalChangesOnFocus`) calls this too, as a cheap
    * backstop for a change made while the watcher wasn't running or its poll window missed it. Either way, this is the
    * signal `scheduleDocumentAnalysis`'s cache otherwise has no way to receive, since an on-disk dictionary edit
    * changes nothing about the spell-check config itself. The next `scheduleDocumentAnalysis` call picks up the
    * refreshed fingerprints and, if they actually differ, schedules re-analysis exactly as a buffer edit would.
    */
  def refreshDictionaryFingerprints(): IO[Unit] =
    modelCommit.currentState.flatMap { state =>
      discoverAndCacheDictionaryFingerprints(state.persisted.config.languageToolsConfig.spellCheck.normalized).void
    }

  /** The directories `AppRuntime.externalChangeWatchLoop` should register a real-time watch on for the *current*
    * spell-check config (#1691), re-derived from the current model on every call so it tracks a config change the same
    * cycle the buffer-directory set already does. See `SpellCheckConfig.dictionaryWatchDirectories` for why this is
    * safe to call every watch-loop cycle without itself touching the filesystem.
    */
  def dictionaryWatchDirectories: IO[Set[java.nio.file.Path]] =
    modelCommit.currentState.map(state =>
      SpellCheckConfig.dictionaryWatchDirectories(state.persisted.config.languageToolsConfig.spellCheck)
    )

  def explorerWatchDirectories: IO[Set[Path]] =
    modelCommit.currentState.map(PanelContentSync.explorerWatchDirectories)

  def markExplorerDirectoriesStale(directories: Set[Path]): IO[Unit] =
    dispatch(
      modelCommit.updateValidated(model =>
        Some(model.copy(app = PanelContentSync.withStaleDirectories(model.app, directories)))
      )
    )

  /** The one quit step, for both a normal and a forced quit: the `Lane.Exclusive` barrier of
    * docs/state-architecture-target.md. Queued and running Sequential work -- file saves, config and preset writes --
    * finishes first, for at most [[ShutdownGracePeriod]] so a write that hangs cannot wedge quitting; switch-latest and
    * drop-if-busy work is cancelled. The lanes are then released and later requests become no-ops. Runs once; every
    * caller returns when it has finished.
    */
  def shutdownEffects(): IO[Unit] =
    beforeEffectsShutdown >> effectsShutdownRef
      .getAndSet(true)
      .flatMap(alreadyShut =>
        if alreadyShut then IO.unit
        // Started on its own fiber so a caller cancelled while waiting (a lost race on quit) cannot abandon it.
        else (drainPersistence >> releaseEffectLanes).guarantee(effectsShutDown.complete(()).void).start.void
      ) >> effectsShutDown.get

  private def drainPersistence: IO[Unit] =
    IO.deferred[Unit].flatMap { drained =>
      effectLanes.submit(Lane.Exclusive, drained.complete(()).void) >>
        drained.get.timeoutTo(
          ShutdownGracePeriod,
          logger.warn(s"[EFFECTS] Pending persistence still running after $ShutdownGracePeriod; abandoning it on quit")
        )
    }

  /** Returns once every lane job accepted so far has settled and every result it handed the dispatcher has been
    * applied, including follow-up work those results queued. Never call from code on the dispatcher.
    */
  def awaitEffects: IO[Unit] =
    submittedEffects.get.flatMap { before =>
      effectLanes.drain >> dispatcher.submit(IO.unit) >> submittedEffects.get.flatMap(after =>
        if after == before then IO.unit else awaitEffects
      )
    }

  /** What file persistence needs from this boundary (#1697 Wave 3). */
  val fileLanes: FileEffectLanes = new FileEffectLanes:
    val fileWrites: FileWriteLedger = fileWriteLedger
    def submitToLane(lane: Lane.Scheduled, job: IO[Unit]): IO[Unit] =
      submittedEffects.update(_ + 1) >> effectLanes.submit(lane, job)
    def post(update: IO[Unit]): IO[Unit]           = dispatcher.post(update)
    def dispatchUpdate(update: IO[Unit]): IO[Unit] = dispatcher.submit(update)

  def scheduleFindSearch(request: FindSearchRequest): IO[Unit] =
    submit(
      FindSearchLane,
      IO.sleep(FindSearchDebounce) >>
        IO.delay(FindSearch.results(request.content, request.query))
          .flatMap(results => postResult(EffectResult.FindSearchCompleted(request, results)))
    )

  /** Supersedes any pending markdown-preview commit for `bufferId` with one that, after
    * `MarkdownPreviewCommitDebounce`, records `generation` as this buffer's committed markdown-preview generation -- if
    * no edit has moved past it by then. The renderer compares this against `Buffer.markdownPreviewEditGeneration` to
    * decide whether an edit burst is still in flight -- see `MarkdownDocumentPreview.renderOrReuseCommitted`.
    */
  def scheduleMarkdownPreviewCommit(bufferId: BufferId, generation: Long): IO[Unit] =
    submit(
      markdownPreviewCommitLane(bufferId),
      IO.sleep(MarkdownPreviewCommitDebounce) >> postResult(EffectResult.MarkdownPreviewSettled(bufferId, generation))
    )

  /** Re-parses the outline of `bufferId` once edits to it pause: large documents take several milliseconds to parse,
    * too long to repeat on every keystroke. The result is dropped if the buffer changed again before it landed.
    */
  private def scheduleOutlineRefresh(bufferId: BufferId): IO[Unit] =
    submit(
      OutlineRefreshLane,
      IO.sleep(OutlineRefreshDebounce) >> modelCommit.currentState.flatMap { snapshot =>
        snapshot.persisted.buffers.get(bufferId).traverse_ { buffer =>
          IO.delay(PanelSymbolLookup.outlineSymbolsForBuffer(buffer))
            .flatMap(symbols =>
              postResult(EffectResult.OutlineRefreshed(bufferId, buffer.document.contentVersion, symbols))
            )
        }
      }
    )

  private def listExplorerDirectory(surfaceId: SurfaceId, path: Path): IO[Unit] =
    submit(
      Lane.Keyed(LaneKey.ExplorerListing(surfaceId, path.toAbsolutePath.normalize), LanePolicy.SwitchLatest),
      listDirectory(path).attempt.flatMap { listing =>
        val result = listing.leftMap(error => Option(error.getMessage).getOrElse(error.getClass.getSimpleName))
        listing.left.toOption.traverse_(error => logger.error(error)(s"[FILE] Failed to load directory $path")) >>
          postResult(EffectResult.ExplorerListed(surfaceId, path, result))
      }
    )

  private def markdownPreviewCommitLane(bufferId: BufferId): Lane.Keyed =
    Lane.Keyed(LaneKey.MarkdownPreview(bufferId), LanePolicy.SwitchLatest)

  private[manager] def submitEffect(lane: Lane.Keyed, job: IO[Unit]): IO[Unit] = submit(lane, job)

  // A request arriving after shutdown has nothing left to run on, and quitting does not want it anyway.
  private def submit(lane: Lane.Scheduled, job: IO[Unit]): IO[Unit] =
    submittedEffects.update(_ + 1) >> effectLanes.submit(lane, job).recover { case _: EffectLanes.Released => () }

  private def postResult(result: EffectResult): IO[Unit] =
    dispatcher.post(modelCommit.applyResult(result, _ => IO.unit))

  private def documentAnalysisJob: IO[Unit] =
    given Logger[IO] = logger
    (IO.sleep(DocumentAnalysisDebounce) >>
      Trace.timed("analysis.documentAnalysisJob") {
        modelCommit.currentState.flatMap { snapshot =>
          val spellCheckConfig = snapshot.persisted.config.languageToolsConfig.spellCheck
          IO.blocking(DictionaryLoader.loadSnapshot(spellCheckConfig, dictionaryCache)).flatMap { dictionary =>
            val expected = SpellChecker.analysisFingerprints(snapshot, dictionary.fingerprints)
            val analyzed = SpellChecker.refreshDiagnostics(snapshot, dictionary)
            postResult(EffectResult.DocumentAnalysisCompleted(analyzed, expected, dictionary.fingerprints))
          }
        }
      }).handleErrorWith(error =>
      documentAnalysisInputsRef.set(None) >> logger.error(error)("[ANALYSIS] Document analysis refresh failed")
    )

  private def requiresDocumentAnalysis(state: AppState): Boolean =
    state.persisted.config.languageToolsConfig.spellCheck.enabled ||
      state.runtime.languageService.diagnosticsState.spellCheckCache.nonEmpty

private[manager] object StateManagerOperationBoundary:

  /** What a commit of `newState` over `fallbackState` would write, or why it is rejected: every commit path runs this
    * so none of them can skip validation or the fix-ups below.
    */
  def prepareCommit(newState: AppState, fallbackState: AppState): Either[List[String], AppState] =
    // #1550: every state transition passes through here, so this is the one place that can keep the floating comment
    // lens in sync with the cursor regardless of what moved it -- a keyboard cursor move opens/closes it exactly as a
    // mouse click already did, without each event source having to remember to call it itself.
    AppStateValidation
      .validated(EventPipelineTransitions.commandRunnerFocusNormalized(newState))
      .map(CommentRendering.syncFloatingLensWithCursor(_, fallbackState))
      .map(PanelContentSync.synced(_, fallbackState))
      .map(NotesPaneSync.synced(_, fallbackState))
      .map(PanelArrangement.resyncedIn)

  /** A directory listing for an explorer. `FileBrowser` lists a missing directory as empty, which an explorer would
    * show as an empty folder, so an empty listing is checked for the directory still being there.
    */
  def explorerListing(list: Path => IO[List[FileEntry]])(directory: Path): IO[List[DirEntry]] =
    list(directory).flatMap { entries =>
      if entries.nonEmpty then IO.pure(entries.map(entry => DirEntry(entry.path, entry.name, entry.isDirectory)))
      else
        IO.blocking(java.nio.file.Files.isDirectory(directory))
          .ifM(IO.pure(Nil), IO.raiseError(new java.io.FileNotFoundException("folder not found")))
    }

  /** `StateManager` is built as a plain `IO` (by the app and by many specs), so no `Resource` owns these lanes: they
    * are allocated here and released by [[StateManagerOperationBoundary.shutdownEffects]] on the quit path.
    */
  def create(
    modelRef: Ref[IO, Model],
    logger: Logger[IO],
    beforeDocumentAnalysisStart: IO[Unit] = IO.unit,
    beforeEffectsShutdown: IO[Unit] = IO.unit,
    // Injectable seam for tests (mirrors `SpellCheckConfig.discoverDictionarySourcePaths`'s own
    // `osDictionaryDirectories` parameter) so a spec can count or fake filesystem stats without touching a real
    // dictionary directory -- see `StateManagerDictionaryFingerprintCacheSpec`.
    discoverDictionaryFingerprints: SpellCheckConfig => IO[List[SpellCheckDictionaryFingerprint]] = config =>
      IO.blocking(SpellCheckConfig.discoverDictionaryFingerprints(config)),
    listDirectory: Path => IO[List[DirEntry]] = explorerListing(FileBrowser.listDirectory),
    wrapCache: WrappedLineCache = WrappedLineCache.Uncached
  ): IO[StateManagerOperationBoundary] =
    for
      pendingOperations         <- Ref.of[IO, List[StateManagerOperation]](Nil)
      documentAnalysisInputsRef <- Ref.of[IO, Option[Map[DocumentUri, SpellCheckFingerprint]]](None)
      dictionaryFingerprintsRef <-
        Ref.of[IO, Option[(SpellCheckConfig, List[SpellCheckDictionaryFingerprint])]](None)
      effectsShutdownRef <- Ref.of[IO, Boolean](false)
      effectsShutDown    <- Deferred[IO, Unit]
      submittedEffects   <- Ref.of[IO, Long](0L)
      (effectLanes, releaseEffectLanes) <- EffectLanes
        .resource((lane, error) => logger.error(error)(s"[EFFECTS] Job on $lane failed"))
        .allocated
      dispatcher        <- StateManagerDispatcher.create(logger)
      fileWriteLedger   <- FileWriteLedger.create
      commitObserver    <- Ref.of[IO, (AppState, AppState) => IO[Unit]]((_, _) => IO.unit)
      commitsUnobserved <- Ref.of[IO, Boolean](false)
    yield new StateManagerOperationBoundary(
      pendingOperations,
      modelRef,
      documentAnalysisInputsRef,
      dictionaryFingerprintsRef,
      logger,
      effectLanes,
      releaseEffectLanes,
      effectsShutdownRef,
      effectsShutDown,
      submittedEffects,
      beforeDocumentAnalysisStart,
      beforeEffectsShutdown,
      dispatcher,
      fileWriteLedger,
      discoverDictionaryFingerprints,
      DictionaryCache(),
      listDirectory,
      commitObserver,
      wrapCache,
      commitsUnobserved
    )
