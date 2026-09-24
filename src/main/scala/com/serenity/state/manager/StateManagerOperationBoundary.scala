package com.serenity.state.manager

import scala.concurrent.duration.*

import cats.effect.*
import com.serenity.command.{CommandRegistry, CommandRunner}
import com.serenity.config.SpellCheckConfig
import com.serenity.diagnostics.Trace
import com.serenity.document.CommentRendering
import com.serenity.spellcheck.{DictionaryLoader, SpellChecker}
import com.serenity.state.effects.{EffectLanes, Lane, LaneKey, LanePolicy}
import com.serenity.state.models.*
import com.serenity.state.reducers.CommandRunnerPanelSelections
import org.typelevel.log4cats.Logger

/** Operations emitted by capabilities for ordered interpretation at the event boundary. */
private[manager] enum StateManagerOperation:
  case Event(event: com.serenity.keystroke.events.Event)
  case ApplyAnimationHooks(previousState: AppState)

/** One-directional hand-off for operations emitted while interpreting effects. */
final private[manager] class StateManagerOperationBoundary private (
    pendingOperations: Ref[IO, List[StateManagerOperation]],
    stateRef: Ref[IO, AppState],
    documentAnalysisInputsRef: Ref[IO, Option[Map[String, SpellCheckFingerprint]]],
    logger: Logger[IO],
    val effectLanes: EffectLanes,
    releaseEffectLanes: IO[Unit],
    effectsShutdownRef: Ref[IO, Boolean],
    effectsShutDown: Deferred[IO, Unit],
    submittedEffects: Ref[IO, Long],
    beforeDocumentAnalysisStart: IO[Unit],
    beforeEffectsShutdown: IO[Unit],
    dispatcher: StateManagerDispatcher,
    fileWriteLedger: FileWriteLedger
):
  private val DocumentAnalysisDebounce         = 150.millis
  private val FindSearchDebounce               = 50.millis
  private val MarkdownPreviewCommitDebounce    = 150.millis
  private val FindSearchLane: Lane.Keyed       = Lane.Keyed(LaneKey.Search, LanePolicy.SwitchLatest)
  private val DocumentAnalysisLane: Lane.Keyed = Lane.Keyed(LaneKey.Analysis, LanePolicy.SwitchLatest)
  private val ShutdownGracePeriod              = 5.seconds

  def enqueueEvent(event: com.serenity.keystroke.events.Event): IO[Unit] =
    pendingOperations.update(_ :+ StateManagerOperation.Event(event))

  def enqueueAnimationHooks(previousState: AppState): IO[Unit] =
    pendingOperations.update(_ :+ StateManagerOperation.ApplyAnimationHooks(previousState))

  def takeOperations: IO[List[StateManagerOperation]] = pendingOperations.getAndSet(Nil)

  /** Runs `request` on the single state dispatcher and waits for it (#1570, #1697): a dispatch commits a state built
    * from a snapshot read at its own start, so it must never overlap another writer. Never call this from code already
    * running on the dispatcher -- see `StateManagerDispatcher.submit`.
    */
  def dispatch[A](request: IO[A]): IO[A] = dispatcher.submit(request)

  /** Runs `request` only if no dispatch is in flight -- the render tick's way to stay off a slow dispatch. */
  def runIfDispatcherIdle[A](request: IO[A]): IO[Option[A]] = dispatcher.runIfIdle(request)

  def ensureCommandRunnerSurface(state: AppState): AppState =
    val registry = CommandRegistry.default
    val activatedRunner =
      CommandRunner.empty.activate(
        registry,
        state.persisted.config,
        state.runtime.isTuiMode,
        state.runtime.keyboardFidelityTier,
        state.commandRunnerContext
      )
    val runner = activatedRunner.copy(
      optionSelections = activatedRunner.optionSelections ++ CommandRunnerPanelSelections.fromState(state)
    )
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

  def validateAndUpdateState(newState: AppState, fallbackState: AppState): IO[Unit] =
    StateManagerOperationBoundary.prepareCommit(newState, fallbackState) match
      case Right(committedState) =>
        logModalTransition(fallbackState, committedState) >> stateRef.set(committedState) >> scheduleDocumentAnalysis()
      case Left(errors) =>
        logRejectedCommit(errors) >> stateRef.set(fallbackState)

  /** The follow-up work of a commit made outside `validateAndUpdateState` from a `prepareCommit` result. */
  private[manager] def afterCommit(fallbackState: AppState, committedState: AppState): IO[Unit] =
    logModalTransition(fallbackState, committedState) >> scheduleDocumentAnalysis()

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

  def scheduleDocumentAnalysis(): IO[Unit] =
    stateRef.get.flatMap { state =>
      val spellCheckConfig = state.persisted.config.languageToolsConfig.spellCheck
      IO.blocking(
        SpellChecker.analysisFingerprints(state, SpellCheckConfig.discoverDictionaryFingerprints(spellCheckConfig))
      ).flatMap { inputs =>
        documentAnalysisInputsRef.modify(previous => Some(inputs) -> previous.forall(_ != inputs)).flatMap {
          inputsChanged =>
            if !inputsChanged || !requiresDocumentAnalysis(state) then IO.unit
            else
              effectsShutdownRef.get.ifM(
                IO.unit,
                beforeDocumentAnalysisStart >> submit(DocumentAnalysisLane, documentAnalysisJob)
              )
        }
      }
    }

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
    val fileWrites: FileWriteLedger                                 = fileWriteLedger
    def submitToLane(lane: Lane.Scheduled, job: IO[Unit]): IO[Unit] =
      submittedEffects.update(_ + 1) >> effectLanes.submit(lane, job)
    def post(update: IO[Unit]): IO[Unit]                            = dispatcher.post(update)
    def dispatchUpdate(update: IO[Unit]): IO[Unit]                  = dispatcher.submit(update)
    def validateAndUpdateState(newState: AppState, fallbackState: AppState): IO[Unit] =
      StateManagerOperationBoundary.this.validateAndUpdateState(newState, fallbackState)

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

  private def markdownPreviewCommitLane(bufferId: BufferId): Lane.Keyed =
    Lane.Keyed(LaneKey.MarkdownPreview(bufferId), LanePolicy.SwitchLatest)

  private[manager] def submitEffect(lane: Lane.Keyed, job: IO[Unit]): IO[Unit] = submit(lane, job)

  // A request arriving after shutdown has nothing left to run on, and quitting does not want it anyway.
  private def submit(lane: Lane.Scheduled, job: IO[Unit]): IO[Unit] =
    submittedEffects.update(_ + 1) >> effectLanes.submit(lane, job).recover { case _: EffectLanes.Released => () }

  /** Applies `result` through the validated commit path if it is still current, then runs `onApplied` with the
    * committed state. Runs on the dispatcher: a lane job reaches it through `dispatch`.
    */
  private[manager] def applyResult(result: EffectResult, onApplied: AppState => IO[Unit]): IO[Unit] =
    stateRef.flatModify { current =>
      val next = EffectResult.applyIfCurrent(current, result)
      if next eq current then (current, IO.unit)
      else
        StateManagerOperationBoundary.prepareCommit(next, current) match
          case Right(committed) => (committed, afterCommit(current, committed) >> onApplied(committed))
          case Left(errors)     => (current, logRejectedCommit(errors))
    }

  private def postResult(result: EffectResult): IO[Unit] =
    dispatcher.post(stateRef.update(EffectResult.applyIfCurrent(_, result)))

  private def documentAnalysisJob: IO[Unit] =
    given Logger[IO] = logger
    (IO.sleep(DocumentAnalysisDebounce) >>
      Trace.timed("analysis.documentAnalysisJob") {
        stateRef.get.flatMap { snapshot =>
          val spellCheckConfig = snapshot.persisted.config.languageToolsConfig.spellCheck
          IO.blocking(DictionaryLoader.loadSnapshot(spellCheckConfig)).flatMap { dictionary =>
            val expected = SpellChecker.analysisFingerprints(snapshot, dictionary.fingerprints)
            val analyzed = SpellChecker.refreshDiagnostics(snapshot, dictionary)
            postResult(EffectResult.DocumentAnalysisCompleted(analyzed, expected, dictionary.fingerprints))
          }
        }
      }).handleErrorWith(error =>
      documentAnalysisInputsRef.set(None) >> logger.error(error)("[ANALYSIS] Document analysis refresh failed")
    )

  private def requiresDocumentAnalysis(state: AppState): Boolean =
    state.persisted.config.languageToolsConfig.spellCheck.enabled || state.runtime.diagnosticsState.spellCheckCache.nonEmpty

private[manager] object StateManagerOperationBoundary:

  /** What a commit of `newState` over `fallbackState` would write, or why it is rejected: every commit path runs this
    * so none of them can skip validation or the fix-ups below.
    */
  def prepareCommit(newState: AppState, fallbackState: AppState): Either[List[String], AppState] =
    // #1550: every state transition passes through here, so this is the one place that can keep the floating comment
    // lens in sync with the cursor regardless of what moved it -- a keyboard cursor move opens/closes it exactly as a
    // mouse click already did, without each event source having to remember to call it itself.
    AppStateValidation
      .validated(normalizeCommandRunnerFocus(newState))
      .map(CommentRendering.syncFloatingLensWithCursor(_, fallbackState))

  private def normalizeCommandRunnerFocus(state: AppState): AppState =
    if state.hasCommandRunnerDomain && !state.isCommandRunnerDomainFocus() then
      state.preferredCommandRunnerFocus.fold(state)(focus =>
        state.copy(persisted = state.persisted.copy(focus = focus))
      )
    else state

  /** `StateManager` is built as a plain `IO` (by the app and by many specs), so no `Resource` owns these lanes: they
    * are allocated here and released by [[StateManagerOperationBoundary.shutdownEffects]] on the quit path.
    */
  def create(
    stateRef: Ref[IO, AppState],
    logger: Logger[IO],
    beforeDocumentAnalysisStart: IO[Unit] = IO.unit,
    beforeEffectsShutdown: IO[Unit] = IO.unit
  ): IO[StateManagerOperationBoundary] =
    for
      pendingOperations         <- Ref.of[IO, List[StateManagerOperation]](Nil)
      documentAnalysisInputsRef <- Ref.of[IO, Option[Map[String, SpellCheckFingerprint]]](None)
      effectsShutdownRef        <- Ref.of[IO, Boolean](false)
      effectsShutDown           <- Deferred[IO, Unit]
      submittedEffects          <- Ref.of[IO, Long](0L)
      (effectLanes, releaseEffectLanes) <- EffectLanes
        .resource((lane, error) => logger.error(error)(s"[EFFECTS] Job on $lane failed"))
        .allocated
      dispatcher      <- StateManagerDispatcher.create(logger)
      fileWriteLedger <- FileWriteLedger.create
    yield new StateManagerOperationBoundary(
      pendingOperations,
      stateRef,
      documentAnalysisInputsRef,
      logger,
      effectLanes,
      releaseEffectLanes,
      effectsShutdownRef,
      effectsShutDown,
      submittedEffects,
      beforeDocumentAnalysisStart,
      beforeEffectsShutdown,
      dispatcher,
      fileWriteLedger
    )
