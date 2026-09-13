package com.serenity.state.manager

import cats.effect.unsafe.implicits.global
import cats.effect.{IO, Ref}
import com.serenity.rope.Balance
import com.serenity.state.models.*
import com.serenity.state.undo.{HistoryEntry, UndoState}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Exercises [[StateManagerReplaceWorkflow]] on its own (#1442), without a composed `StateManager`: the active editor
  * buffer lookup and the "tell the surface what to show" callback are plain closures, so each branch (no active buffer,
  * empty find text, no matches, a scope error, an actual replacement) can be checked directly rather than only
  * end-to-end via `ReplaceWorkflowStateManagerSpec`.
  */
class StateManagerReplaceWorkflowSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private val surfaceId = SurfaceId("replace-workflow")
  private val bufferId  = BufferId(0)
  private val paneId    = PaneId(0)

  private def stateWith(workflow: ReplaceWorkflowState, content: String, hasActiveBuffer: Boolean = true): AppState =
    val buffer = Buffer.fromString(bufferId, content)
    val base   = AppState.initial.copy(persisted = AppState.initial.persisted.copy(buffers = Map(bufferId -> buffer)))
    val surface = UiSurface(
      surfaceId,
      SurfaceContent.ModalWorkflow(Modal.ReplaceWorkflow(workflow)),
      SurfacePresentation.Floating(None, SurfacePlacement.BelowCursor)
    )
    base.copy(
      persisted = base.persisted.copy(
        layout =
          if hasActiveBuffer then base.persisted.layout
          else base.persisted.layout.copy(activeEditorPaneId = None)
      ),
      runtime = base.runtime.copy(uiSurfaces = List(surface))
    )

  final private class Harness(
      val stateRef: Ref[IO, AppState],
      val undoRef: Ref[IO, UndoState],
      val surfaceUpdates: Ref[IO, List[ReplaceWorkflowState]],
      val workflow: StateManagerReplaceWorkflow
  ):
    def currentState: AppState = stateRef.get.unsafeRunSync()
    def lastSurfaceUpdate: ReplaceWorkflowState =
      surfaceUpdates.get.unsafeRunSync().lastOption.getOrElse(fail("no surface update recorded"))

  private def harness(initialState: AppState): Harness =
    val stateRefVar = Ref.of[IO, AppState](initialState).unsafeRunSync()
    val undoRefVar  = Ref.of[IO, UndoState](UndoState()).unsafeRunSync()
    val updatesVar  = Ref.of[IO, List[ReplaceWorkflowState]](Nil).unsafeRunSync()
    def activeEditorBufferId(state: AppState): Option[BufferId] =
      state.persisted.layout.activeEditorPaneId.flatMap(state.persisted.layout.editorPanes.get).flatMap(_.bufferId)
    def updateSurface(id: SurfaceId, updated: ReplaceWorkflowState): IO[Unit] =
      updatesVar.update(_ :+ updated) >> stateRefVar.update { state =>
        state.copy(runtime = state.runtime.copy(uiSurfaces = state.runtime.uiSurfaces.map {
          case s if s.id == id => s.copy(content = SurfaceContent.ModalWorkflow(Modal.ReplaceWorkflow(updated)))
          case other           => other
        }))
      }

    new Harness(
      stateRefVar,
      undoRefVar,
      updatesVar,
      new StateManagerReplaceWorkflow(
        stateRefVar,
        undoRefVar,
        activeEditorBufferId,
        updateSurface,
        (newState, fallbackState) => stateRefVar.set(AppStateValidation.validated(newState).getOrElse(fallbackState))
      )
    )

  "submitReplaceWorkflowEffect with no active buffer" should "report a status message and change nothing else" in {
    val before = stateWith(ReplaceWorkflowState(findText = "a"), "irrelevant", hasActiveBuffer = false)
    val h      = harness(before)

    h.workflow.submitReplaceWorkflowEffect(surfaceId).unsafeRunSync()

    h.lastSurfaceUpdate.statusMessage shouldBe Some("No active buffer")
    h.currentState.persisted.buffers(bufferId).document.content.collect() shouldBe "irrelevant"
  }

  it should "report a status message when find text is empty" in {
    val before = stateWith(ReplaceWorkflowState(findText = ""), "some text")
    val h      = harness(before)

    h.workflow.submitReplaceWorkflowEffect(surfaceId).unsafeRunSync()

    h.lastSurfaceUpdate.statusMessage shouldBe Some("Enter text to find")
  }

  it should "report a status message when there are no matches" in {
    val before = stateWith(ReplaceWorkflowState(findText = "missing"), "some text")
    val h      = harness(before)

    h.workflow.submitReplaceWorkflowEffect(surfaceId).unsafeRunSync()

    h.lastSurfaceUpdate.statusMessage shouldBe Some("No matches found")
  }

  it should "report a status message when Selection scope is chosen without an active selection" in {
    val before = stateWith(
      ReplaceWorkflowState(findText = "some", selectedScope = ReplaceWorkflowScope.Selection),
      "some text"
    )
    val h = harness(before)

    h.workflow.submitReplaceWorkflowEffect(surfaceId).unsafeRunSync()

    h.lastSurfaceUpdate.statusMessage should not be None
    h.currentState.persisted.buffers(bufferId).document.content.collect() shouldBe "some text"
  }

  "ReplaceAll" should "replace every match, dismiss the surface, and record an undo entry" in {
    val before = stateWith(
      ReplaceWorkflowState(
        findText = "cat",
        replacementText = "dog",
        selectedAction = ReplaceWorkflowAction.ReplaceAll
      ),
      "cat sat, cat ran"
    )
    val h = harness(before)

    h.workflow.submitReplaceWorkflowEffect(surfaceId).unsafeRunSync()

    h.currentState.persisted.buffers(bufferId).document.content.collect() shouldBe "dog sat, dog ran"
    h.currentState.runtime.uiSurfaces.exists(_.id == surfaceId) shouldBe false
    h.undoRef.get.unsafeRunSync().undoStack should have size 1
    h.undoRef.get.unsafeRunSync().undoStack.head shouldBe a[HistoryEntry.BufferEdit]
  }

  it should "move focus to the active editor pane after replacing" in {
    val before = stateWith(
      ReplaceWorkflowState(
        findText = "cat",
        replacementText = "dog",
        selectedAction = ReplaceWorkflowAction.ReplaceAll
      ),
      "cat"
    )
    val h = harness(before)

    h.workflow.submitReplaceWorkflowEffect(surfaceId).unsafeRunSync()

    h.currentState.persisted.focus shouldBe Focus.EditorPane(paneId)
  }

  "ReplaceNext" should "replace only the first match at or after the cursor and keep the surface open" in {
    val before = stateWith(
      ReplaceWorkflowState(
        findText = "cat",
        replacementText = "dog",
        selectedAction = ReplaceWorkflowAction.ReplaceNext
      ),
      "cat sat, cat ran"
    )
    val h = harness(before)

    h.workflow.submitReplaceWorkflowEffect(surfaceId).unsafeRunSync()

    h.currentState.persisted.buffers(bufferId).document.content.collect() shouldBe "dog sat, cat ran"
    h.currentState.runtime.uiSurfaces.exists(_.id == surfaceId) shouldBe true
    h.lastSurfaceUpdate.statusMessage shouldBe Some("Replaced next match")
  }

  it should "record an undo entry for the single replacement" in {
    val before = stateWith(
      ReplaceWorkflowState(
        findText = "cat",
        replacementText = "dog",
        selectedAction = ReplaceWorkflowAction.ReplaceNext
      ),
      "cat sat"
    )
    val h = harness(before)

    h.workflow.submitReplaceWorkflowEffect(surfaceId).unsafeRunSync()

    h.undoRef.get.unsafeRunSync().undoStack should have size 1
  }

  "submitReplaceWorkflowEffect when the surface isn't a replace-workflow surface" should "do nothing" in {
    val before = AppState.initial
    val h      = harness(before)

    h.workflow.submitReplaceWorkflowEffect(SurfaceId("no-such-surface")).unsafeRunSync()

    h.currentState shouldBe before
  }

  "replaceWorkflowSurface" should "look up the workflow state for a live replace-workflow surface" in {
    val workflow = ReplaceWorkflowState(findText = "x")
    val state    = stateWith(workflow, "x")
    val h        = harness(state)

    h.workflow.replaceWorkflowSurface(state, surfaceId).map(_._2) shouldBe Some(workflow)
  }

  it should "report None for a surface id that isn't a replace-workflow surface" in {
    val state = AppState.initial
    val h     = harness(state)

    h.workflow.replaceWorkflowSurface(state, SurfaceId("missing")) shouldBe None
  }
