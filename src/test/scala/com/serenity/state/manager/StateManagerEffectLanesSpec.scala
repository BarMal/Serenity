package com.serenity.state.manager

import scala.concurrent.duration.*

import cats.effect.{Deferred, IO, Ref}
import com.serenity.config.AppConfig
import com.serenity.rope.{Balance, Rope}
import com.serenity.state.effects.{Lane, LaneKey, LanePolicy}
import com.serenity.state.models.*
import com.serenity.testkit.VirtualTime.runVirtual
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.noop.NoOpLogger

/** Find search, markdown-preview commits and document analysis run on `EffectLanes` (#1697 Wave 3): a newer request
  * supersedes an older one on the same lane, and a result that is no longer current when it reaches the dispatcher is
  * dropped rather than applied.
  */
class StateManagerEffectLanesSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private val quietLogger: Logger[IO]  = NoOpLogger.impl[IO]
  private val findSurfaceId            = SurfaceId("find")
  private val editorBufferId           = BufferId(0)
  private val analysisLane: Lane.Keyed = Lane.Keyed(LaneKey.Analysis, LanePolicy.SwitchLatest)
  private val searchLane: Lane.Keyed   = Lane.Keyed(LaneKey.Search, LanePolicy.SwitchLatest)

  private def boundaryOver(
    state: AppState,
    beforeDocumentAnalysisStart: IO[Unit] = IO.unit,
    logger: Logger[IO] = quietLogger
  ): IO[(Ref[IO, AppState], StateManagerOperationBoundary)] =
    for
      stateRef   <- Ref.of[IO, AppState](state)
      operations <- StateManagerOperationBoundary.create(stateRef, logger, beforeDocumentAnalysisStart)
    yield (stateRef, operations)

  private def withEditorContent(state: AppState, content: String): AppState =
    val buffer = state.persisted.buffers(editorBufferId)
    state.copy(persisted =
      state.persisted.copy(buffers =
        state.persisted.buffers
          .updated(editorBufferId, buffer.copy(document = buffer.document.copy(content = Rope(content))))
      )
    )

  private def withFindQuery(state: AppState, query: String): AppState =
    state.copy(runtime = state.runtime.copy(uiSurfaces = state.runtime.uiSurfaces.map {
      case surface if surface.id == findSurfaceId =>
        surface.copy(content = SurfaceContent.ModalWorkflow(Modal.Find(query, Nil, 0)))
      case other => other
    }))

  private def findModalState(content: String): AppState =
    val withContent = withEditorContent(AppState.initial, content)
    withContent.copy(
      persisted = withContent.persisted.copy(focus = Focus.Surface(findSurfaceId)),
      runtime = withContent.runtime.copy(uiSurfaces =
        List(
          UiSurface(
            findSurfaceId,
            SurfaceContent.ModalWorkflow(Modal.Find("", Nil, 0)),
            SurfacePresentation.Floating(None, SurfacePlacement.BelowCursor)
          )
        )
      )
    )

  private def findModal(state: AppState): Option[Modal] =
    state.surfaceById(findSurfaceId).map(_.content).collect { case SurfaceContent.ModalWorkflow(modal) => modal }

  /** What the find reducer does on a keystroke: update the live query, then request a search for it. */
  private def typeFindQuery(
    stateRef: Ref[IO, AppState],
    operations: StateManagerOperationBoundary,
    query: String
  ): IO[Unit] =
    stateRef.updateAndGet(withFindQuery(_, query)).flatMap { state =>
      operations.scheduleFindSearch(
        FindSearchRequest(
          findSurfaceId,
          editorBufferId,
          query,
          state.persisted.buffers(editorBufferId).document.content
        )
      )
    }

  "Find search" should "apply only the last of a burst of requests, one debounce after the last" in {
    val program =
      for
        (stateRef, operations) <- boundaryOver(findModalState("needle noodle needle"))
        _                      <- typeFindQuery(stateRef, operations, "n")
        _                      <- IO.sleep(30.millis)
        _                      <- typeFindQuery(stateRef, operations, "ne")
        _                      <- IO.sleep(30.millis)
        _                      <- typeFindQuery(stateRef, operations, "needle")
        _                      <- IO.sleep(40.millis)
        midBurst               <- stateRef.get.map(findModal)
        _                      <- IO.sleep(1.second)
        settled                <- stateRef.get.map(findModal)
      yield (midBurst, settled)

    runVirtual(program) shouldBe (
      Some(Modal.Find("needle", Nil, 0)),
      Some(Modal.Find("needle", List(FindResult(0, 0), FindResult(0, 14)), 0))
    )
  }

  it should "drop a result computed for a query that has since changed" in {
    val program =
      for
        (stateRef, operations) <- boundaryOver(findModalState("needle noodle"))
        _                      <- typeFindQuery(stateRef, operations, "needle")
        _                      <- IO.sleep(20.millis)
        _                      <- stateRef.update(withFindQuery(_, "noodle"))
        _                      <- IO.sleep(1.second)
        after                  <- stateRef.get
      yield (findModal(after), after.persisted.buffers(editorBufferId).findState)

    runVirtual(program) shouldBe (Some(Modal.Find("noodle", Nil, 0)), None)
  }

  private val previewA = BufferId(1)
  private val previewB = BufferId(2)

  private def previewState(editGeneration: Long): AppState =
    def editedBuffer(id: BufferId) =
      Buffer.fromString(id, "# notes").copy(markdownPreviewEditGeneration = editGeneration)
    AppState.initial.copy(persisted =
      AppState.initial.persisted
        .copy(buffers = Map(previewA -> editedBuffer(previewA), previewB -> editedBuffer(previewB)))
    )

  private def committedGenerations(state: AppState): (Long, Long) =
    (
      state.persisted.buffers(previewA).markdownPreviewCommittedGeneration,
      state.persisted.buffers(previewB).markdownPreviewCommittedGeneration
    )

  "Markdown preview commits" should "supersede per buffer while different buffers proceed independently" in {
    val program =
      for
        (stateRef, operations) <- boundaryOver(previewState(editGeneration = 1L))
        _                      <- operations.scheduleMarkdownPreviewCommit(previewA, 1L)
        _                      <- IO.sleep(50.millis)
        _                      <- operations.scheduleMarkdownPreviewCommit(previewB, 1L)
        _                      <- IO.sleep(50.millis)
        _                      <- operations.scheduleMarkdownPreviewCommit(previewA, 1L)
        _                      <- IO.sleep(60.millis)
        afterFirstWouldFire    <- stateRef.get.map(committedGenerations)
        _                      <- IO.sleep(50.millis)
        afterOtherBufferFires  <- stateRef.get.map(committedGenerations)
        _                      <- IO.sleep(50.millis)
        afterLatestFires       <- stateRef.get.map(committedGenerations)
      yield (afterFirstWouldFire, afterOtherBufferFires, afterLatestFires)

    runVirtual(program) shouldBe ((0L, 0L), (0L, 1L), (1L, 1L))
  }

  it should "not be cancelled by other switch-latest work on the same buffer" in {
    val program =
      for
        (stateRef, operations) <- boundaryOver(previewState(editGeneration = 1L))
        _                      <- operations.scheduleMarkdownPreviewCommit(previewA, 1L)
        _                      <- IO.sleep(50.millis)
        _     <- operations.effectLanes.submit(Lane.Keyed(LaneKey.Buffer(previewA), LanePolicy.SwitchLatest), IO.never)
        _     <- IO.sleep(1.second)
        after <- stateRef.get
      yield committedGenerations(after)

    runVirtual(program) shouldBe (1L, 0L)
  }

  it should "drop a commit whose generation an edit has since moved past" in {
    val program =
      for
        (stateRef, operations) <- boundaryOver(previewState(editGeneration = 1L))
        _                      <- operations.scheduleMarkdownPreviewCommit(previewA, 1L)
        _                      <- IO.sleep(50.millis)
        _ <- stateRef.update(state =>
          state.copy(persisted =
            state.persisted.copy(buffers =
              state.persisted.buffers.updatedWith(previewA)(
                _.map(_.copy(markdownPreviewEditGeneration = 2L))
              )
            )
          )
        )
        _     <- IO.sleep(1.second)
        after <- stateRef.get
      yield committedGenerations(after)

    runVirtual(program) shouldBe (0L, 0L)
  }

  "A settled markdown preview result" should "commit only the generation the buffer is still at" in {
    val state = previewState(editGeneration = 3L)

    committedGenerations(EffectResult.applyIfCurrent(state, EffectResult.MarkdownPreviewSettled(previewA, 3L))) shouldBe
      (3L, 0L)
    EffectResult.applyIfCurrent(state, EffectResult.MarkdownPreviewSettled(previewA, 2L)) shouldBe state
    EffectResult.applyIfCurrent(state, EffectResult.MarkdownPreviewSettled(BufferId(9), 3L)) shouldBe state
  }

  private val misspelling = "qzxvbnw"

  private def spellCheckedState(content: String): AppState =
    val enabled = AppState.initial.copy(persisted =
      AppState.initial.persisted.copy(config =
        AppConfig.default.withSpellCheck(AppConfig.default.languageToolsConfig.spellCheck.copy(enabled = true))
      )
    )
    withEditorContent(enabled, content)

  private def spellingDiagnosticStarts(state: AppState): List[(Int, Int)] =
    state.runtime.diagnosticsState.diagnostics.values.flatten.toList
      .map(diagnostic => (diagnostic.range.start.line, diagnostic.range.start.character))
      .sorted

  "Document analysis" should "apply only the analysis of the latest edit when an edit supersedes a running one" in {
    val firstEdit  = spellCheckedState(misspelling)
    val secondEdit = spellCheckedState(s"hello $misspelling")
    val program =
      for
        (stateRef, operations) <- boundaryOver(firstEdit)
        _                      <- operations.validateAndUpdateState(firstEdit, firstEdit)
        _                      <- IO.sleep(50.millis)
        _                      <- operations.validateAndUpdateState(secondEdit, firstEdit)
        _                      <- IO.sleep(110.millis)
        whenFirstWouldApply    <- stateRef.get.map(spellingDiagnosticStarts)
        _                      <- IO.sleep(1.second)
        settled                <- stateRef.get.map(spellingDiagnosticStarts)
      yield (whenFirstWouldApply, settled)

    runVirtual(program) shouldBe (Nil, List((0, 6)))
  }

  it should "start no analysis once effects have shut down" in {
    val edited = spellCheckedState(misspelling)
    val program =
      for
        starts                 <- Ref.of[IO, Int](0)
        (stateRef, operations) <- boundaryOver(edited, beforeDocumentAnalysisStart = starts.update(_ + 1))
        _                      <- operations.shutdownEffects()
        _                      <- operations.validateAndUpdateState(edited, edited)
        _                      <- IO.sleep(1.second)
        started                <- starts.get
        diagnostics            <- stateRef.get.map(spellingDiagnosticStarts)
      yield (started, diagnostics)

    runVirtual(program) shouldBe (0, Nil)
  }

  "Shutting down effects" should "cancel running lane work and turn later requests into no-ops" in {
    val program =
      for
        (_, operations) <- boundaryOver(findModalState("needle"))
        started         <- Deferred[IO, Unit]
        cancelled       <- Deferred[IO, Unit]
        _ <- operations.effectLanes.submit(
          analysisLane,
          (started.complete(()) >> IO.never[Unit]).onCancel(cancelled.complete(()).void)
        )
        _           <- started.get
        _           <- operations.shutdownEffects()
        _           <- operations.shutdownEffects()
        wasCanceled <- cancelled.tryGet
        search <- operations
          .scheduleFindSearch(FindSearchRequest(findSurfaceId, editorBufferId, "needle", Rope("needle")))
          .attempt
        commit <- operations.scheduleMarkdownPreviewCommit(previewA, 1L).attempt
      yield (wasCanceled, search, commit)

    runVirtual(program) shouldBe (Some(()), Right(()), Right(()))
  }

  "A failed lane job" should "be reported to the manager's logger" in {
    val program =
      for
        logged          <- Ref.of[IO, List[String]](Nil)
        (_, operations) <- boundaryOver(AppState.initial, logger = ErrorRecordingLogger(logged))
        _ <- operations.effectLanes.submit(
          searchLane,
          IO.raiseError(new IllegalStateException("search exploded"))
        )
        _        <- operations.effectLanes.drain
        messages <- logged.get
      yield messages

    runVirtual(program).exists(_.contains("search exploded")) shouldBe true
  }

  /** Records `error` lines, with the throwable's message appended; every other level is a no-op. */
  private class ErrorRecordingLogger(ref: Ref[IO, List[String]]) extends Logger[IO]:
    def error(message: => String): IO[Unit]               = ref.update(_ :+ message)
    def error(t: Throwable)(message: => String): IO[Unit] = ref.update(_ :+ s"$message: ${t.getMessage}")
    def warn(message: => String): IO[Unit]                = IO.unit
    def warn(t: Throwable)(message: => String): IO[Unit]  = IO.unit
    def info(message: => String): IO[Unit]                = IO.unit
    def info(t: Throwable)(message: => String): IO[Unit]  = IO.unit
    def debug(message: => String): IO[Unit]               = IO.unit
    def debug(t: Throwable)(message: => String): IO[Unit] = IO.unit
    def trace(message: => String): IO[Unit]               = IO.unit
    def trace(t: Throwable)(message: => String): IO[Unit] = IO.unit
