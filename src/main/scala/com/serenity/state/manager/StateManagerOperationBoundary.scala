package com.serenity.state.manager

import scala.concurrent.duration.*

import cats.effect.*
import cats.effect.std.Semaphore
import cats.syntax.foldable.*
import com.serenity.command.{CommandRegistry, CommandRunner}
import com.serenity.config.SpellCheckConfig
import com.serenity.diagnostics.Trace
import com.serenity.spellcheck.SpellChecker
import com.serenity.state.models.*
import com.serenity.state.reducers.{CommandRunnerPanelSelections, ModalEventReducer}
import org.typelevel.log4cats.Logger

/** Operations emitted by capabilities for ordered interpretation at the event boundary. */
private[manager] enum StateManagerOperation:
  case Event(event: com.serenity.keystroke.events.Event)
  case ApplyAnimationHooks(previousState: AppState)

/** One-directional hand-off for operations emitted while interpreting effects. */
final private[manager] class StateManagerOperationBoundary private (
    pendingOperations: Ref[IO, List[StateManagerOperation]],
    stateRef: Ref[IO, AppState],
    documentAnalysisFiberRef: Ref[IO, Option[Fiber[IO, Throwable, Unit]]],
    documentAnalysisInputsRef: Ref[IO, Option[Map[String, SpellCheckFingerprint]]],
    findSearchFiberRef: Ref[IO, Option[Fiber[IO, Throwable, Unit]]],
    markdownPreviewCommitFibersRef: Ref[IO, Map[BufferId, Fiber[IO, Throwable, Unit]]],
    logger: Logger[IO],
    analysisLifecycleLock: Semaphore[IO],
    documentAnalysisShutdownRef: Ref[IO, Boolean],
    beforeDocumentAnalysisStart: IO[Unit],
    beforeDocumentAnalysisShutdown: IO[Unit]
):
  private val DocumentAnalysisDebounce      = 150.millis
  private val FindSearchDebounce            = 50.millis
  private val MarkdownPreviewCommitDebounce = 150.millis

  def enqueueEvent(event: com.serenity.keystroke.events.Event): IO[Unit] =
    pendingOperations.update(_ :+ StateManagerOperation.Event(event))

  def enqueueAnimationHooks(previousState: AppState): IO[Unit] =
    pendingOperations.update(_ :+ StateManagerOperation.ApplyAnimationHooks(previousState))

  def takeOperations: IO[List[StateManagerOperation]] = pendingOperations.getAndSet(Nil)

  def ensureCommandRunnerSurface(state: AppState): AppState =
    val registry = CommandRegistry.default
    val activatedRunner =
      CommandRunner.empty.activate(
        registry,
        state.persisted.config,
        state.runtime.isTuiMode,
        state.runtime.keyboardFidelityTier
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
        stateWithId.runtime.copy(uiSurfaces = stateWithId.runtime.uiSurfaces.filterNot(_.id == surfaceId) :+ surface)
      )
      .pushFocus(Focus.Surface(surfaceId))

  def validateAndUpdateState(newState: AppState, fallbackState: AppState): IO[Unit] =
    normalizeCommandRunnerFocus(newState).validated match
      case Right(validState) =>
        val modalTransitionLog =
          (fallbackState.modalSurface, validState.modalSurface) match
            case (before, after) if before != after =>
              logger.info(
                s"[STATE MODAL] before=${before.map(_.id).getOrElse("none")} " +
                  s"after=${after.map(_.id).getOrElse("none")} focus=${validState.persisted.focus}"
              )
            case _ => IO.unit
        modalTransitionLog >> stateRef.set(validState) >> scheduleDocumentAnalysis()
      case Left(errors) =>
        logger.error(s"State validation failed: ${errors.mkString(", ")}") >>
          stateRef.set(fallbackState)

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
              analysisLifecycleLock.permit.use { _ =>
                documentAnalysisShutdownRef.get.ifM(
                  IO.unit,
                  for
                    previous <- documentAnalysisFiberRef.getAndSet(None)
                    _        <- previous.traverse_(_.cancel)
                    _        <- beforeDocumentAnalysisStart
                    fiber    <- documentAnalysisJob.start
                    _        <- documentAnalysisFiberRef.set(Some(fiber))
                  yield ()
                )
              }
        }
      }
    }

  def cancelDocumentAnalysis(): IO[Unit] =
    beforeDocumentAnalysisShutdown >> analysisLifecycleLock.permit.use { _ =>
      documentAnalysisShutdownRef.set(true) >>
        documentAnalysisFiberRef.getAndSet(None).flatMap(_.traverse_(_.cancel))
    }

  def scheduleFindSearch(request: FindSearchRequest): IO[Unit] =
    findSearchFiberRef.getAndSet(None).flatMap(_.traverse_(_.cancel)) >>
      (IO.sleep(FindSearchDebounce) >>
        IO.blocking(FindSearch.results(request.content, request.query)).flatMap { results =>
          stateRef.update { before =>
            val after = ModalEventReducer.applyFindSearchResults(before, request, results)
            CursorViewport.ensureVisibleCursors(before, after)
          }
        }).start.flatMap(fiber => findSearchFiberRef.set(Some(fiber)))

  /** Cancels any pending markdown-preview commit for `bufferId` and schedules a new one that, after
    * `MarkdownPreviewCommitDebounce` of no further supersession, records `generation` as this buffer's committed
    * markdown-preview generation. The renderer compares this against `Buffer.markdownPreviewEditGeneration` to decide
    * whether an edit burst is still in flight -- see `MarkdownDocumentPreview.renderOrReuseCommitted`.
    */
  def scheduleMarkdownPreviewCommit(bufferId: BufferId, generation: Long): IO[Unit] =
    markdownPreviewCommitFibersRef.modify(fibers => (fibers - bufferId, fibers.get(bufferId))).flatMap { prior =>
      prior.traverse_(_.cancel) >>
        (IO.sleep(MarkdownPreviewCommitDebounce) >>
          stateRef.update { state =>
            state.persisted.buffers.get(bufferId).fold(state) { buffer =>
              state.copy(persisted =
                state.persisted.copy(buffers =
                  state.persisted.buffers
                    .updated(bufferId, buffer.copy(markdownPreviewCommittedGeneration = generation))
                )
              )
            }
          }).start.flatMap(fiber => markdownPreviewCommitFibersRef.update(_ + (bufferId -> fiber)))
    }

  private def documentAnalysisJob: IO[Unit] =
    given Logger[IO] = logger
    (IO.sleep(DocumentAnalysisDebounce) >>
      Trace.timed("analysis.documentAnalysisJob") {
        stateRef.get.flatMap { snapshot =>
          val spellCheckConfig = snapshot.persisted.config.languageToolsConfig.spellCheck
          IO.blocking(SpellChecker.loadDictionarySnapshot(spellCheckConfig)).flatMap { dictionary =>
            val expected = SpellChecker.analysisFingerprints(snapshot, dictionary.fingerprints)
            val analyzed = SpellChecker.refreshDiagnostics(snapshot, dictionary)
            stateRef
              .update(current => SpellChecker.applyIfCurrent(current, analyzed, expected, dictionary.fingerprints))
          }
        }
      }).handleErrorWith(error =>
      documentAnalysisInputsRef.set(None) >> logger.error(error)("[ANALYSIS] Document analysis refresh failed")
    )

  private def requiresDocumentAnalysis(state: AppState): Boolean =
    state.persisted.config.languageToolsConfig.spellCheck.enabled || state.runtime.diagnosticsState.spellCheckCache.nonEmpty

  private def normalizeCommandRunnerFocus(state: AppState): AppState =
    if state.hasCommandRunnerDomain && !state.isCommandRunnerDomainFocus() then
      state.preferredCommandRunnerFocus.fold(state)(focus =>
        state.copy(persisted = state.persisted.copy(focus = focus))
      )
    else state

private[manager] object StateManagerOperationBoundary:

  def create(
    stateRef: Ref[IO, AppState],
    documentAnalysisFiberRef: Ref[IO, Option[Fiber[IO, Throwable, Unit]]],
    logger: Logger[IO],
    beforeDocumentAnalysisStart: IO[Unit] = IO.unit,
    beforeDocumentAnalysisShutdown: IO[Unit] = IO.unit
  ): IO[StateManagerOperationBoundary] =
    for
      pendingOperations              <- Ref.of[IO, List[StateManagerOperation]](Nil)
      analysisLifecycleLock          <- Semaphore[IO](1)
      documentAnalysisShutdownRef    <- Ref.of[IO, Boolean](false)
      documentAnalysisInputsRef      <- Ref.of[IO, Option[Map[String, SpellCheckFingerprint]]](None)
      findSearchFiberRef             <- Ref.of[IO, Option[Fiber[IO, Throwable, Unit]]](None)
      markdownPreviewCommitFibersRef <- Ref.of[IO, Map[BufferId, Fiber[IO, Throwable, Unit]]](Map.empty)
    yield new StateManagerOperationBoundary(
      pendingOperations,
      stateRef,
      documentAnalysisFiberRef,
      documentAnalysisInputsRef,
      findSearchFiberRef,
      markdownPreviewCommitFibersRef,
      logger,
      analysisLifecycleLock,
      documentAnalysisShutdownRef,
      beforeDocumentAnalysisStart,
      beforeDocumentAnalysisShutdown
    )
