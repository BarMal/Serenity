package com.serenity.state.reducers

import java.nio.file.Paths

import com.serenity.rope.Balance
import com.serenity.state.models.*
import com.serenity.ui.layout.{PanelContent, PanelPosition, PanelTarget, PeekContent, ViewportSize}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** One pure call per public `StateManagerSurfaceCapability` operation, asserting what it changes and that the result
  * passes `AppStateValidation` -- the capability now commits every one of them through the validated path.
  */
class SurfaceOperationReducersSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private def valid(state: AppState): Boolean = AppStateValidation.validated(state).isRight

  private val sized: AppState =
    AppState.initial.copy(runtime = AppState.initial.runtime.copy(viewportSize = Some(ViewportSize(120, 40))))

  private val withOutline: AppState =
    PanelStateReducer.pin(PanelContent.Outline(Nil), PanelPosition.Right, 20, sized).state

  private val outlineId: SurfaceId =
    withOutline.pinnedSurfaces.map(_.id) match
      case List(id) => id
      case other    => fail(s"Expected one pinned surface, got $other")

  private val withPeek: AppState =
    PeekStateReducer.show(PeekContent.QuickInfo("info"), CursorPosition(0, 0), AppState.initial).state

  "PeekStateReducer.show" should "focus a new floating peek surface" in {
    withPeek.runtime.uiSurfaces should have size 1
    withPeek.persisted.focus shouldBe Focus.Surface(withPeek.runtime.uiSurfaces.head.id)
    valid(withPeek) shouldBe true
  }

  "PeekStateReducer.dismiss" should "remove the peek surface and return focus to the editor" in {
    val dismissed = PeekStateReducer.dismiss(withPeek).state

    dismissed.runtime.uiSurfaces shouldBe empty
    dismissed.persisted.focus shouldBe Focus.EditorPane(PaneId(0))
    valid(dismissed) shouldBe true
  }

  "PanelStateReducer.pinPeekOverlay" should "dock the active directory-listing peek" in {
    val listingPeek =
      PeekStateReducer.show(PeekContent.DirectoryListing(Paths.get("/repo"), Nil), CursorPosition(0, 0), sized).state
    val pinned = PanelStateReducer.pinPeekOverlay(PanelPosition.Right, listingPeek).state

    pinned.pinnedSurfaces should have size 1
    valid(pinned) shouldBe true
  }

  "PanelStateReducer.pin" should "dock a new panel" in {
    withOutline.pinnedSurfaces should have size 1
    valid(withOutline) shouldBe true
  }

  "PanelStateReducer.unpin(PanelTarget)" should "unpin by id and by position alike" in {
    val byId       = PanelStateReducer.unpin(PanelTarget.ById(outlineId), withOutline)
    val byPosition = PanelStateReducer.unpin(PanelTarget.ByPosition(PanelPosition.Right), withOutline)

    byId.state.pinnedSurfaces shouldBe empty
    byId.effects should matchPattern { case List(AppEffect.Undo(_)) => }
    byPosition.state shouldBe byId.state
    valid(byId.state) shouldBe true
  }

  it should "leave the state unchanged for a target that resolves to no panel" in {
    PanelStateReducer.unpin(PanelTarget.ByPosition(PanelPosition.Left), withOutline) shouldBe
      ReducerResult.noEffects(withOutline)
  }

  "PanelStateReducer.move" should "move a pinned panel to another edge" in {
    val moved = PanelStateReducer.move(outlineId, PanelPosition.Left, withOutline).state

    moved.persisted.layout.workspaceTree.flatMap(_.positionForSurface(outlineId)) shouldBe Some(PanelPosition.Left)
    valid(moved) shouldBe true
  }

  "PanelStateReducer.expand(PanelTarget)" should "maximise and focus the target by id and by position alike" in {
    val byId       = PanelStateReducer.expand(PanelTarget.ById(outlineId), withOutline).state
    val byPosition = PanelStateReducer.expand(PanelTarget.ByPosition(PanelPosition.Right), withOutline).state

    byId.persisted.layout.maximizedWorkspaceNodeId shouldBe defined
    byId.persisted.focus shouldBe Focus.Surface(outlineId)
    byPosition shouldBe byId
    valid(byId) shouldBe true
  }

  "PanelStateReducer.collapseExpandedPanel" should "clear the maximised node" in {
    val expanded  = PanelStateReducer.expand(PanelTarget.ById(outlineId), withOutline).state
    val collapsed = PanelStateReducer.collapseExpandedPanel(expanded).state

    collapsed.persisted.layout.maximizedWorkspaceNodeId shouldBe None
    valid(collapsed) shouldBe true
  }

  "PanelStateReducer.focus(PanelTarget)" should "focus the target by id and by position alike" in {
    val byId       = PanelStateReducer.focus(PanelTarget.ById(outlineId), withOutline).state
    val byPosition = PanelStateReducer.focus(PanelTarget.ByPosition(PanelPosition.Right), withOutline).state

    byId.persisted.focus shouldBe Focus.Surface(outlineId)
    byPosition shouldBe byId
    valid(byId) shouldBe true
  }

  "PanelStateReducer.resize(PanelTarget)" should "resize the target by id and by position alike" in {
    val byId       = PanelStateReducer.resize(PanelTarget.ById(outlineId), 40, withOutline).state
    val byPosition = PanelStateReducer.resize(PanelTarget.ByPosition(PanelPosition.Right), 40, withOutline).state

    byId.persisted.layout.workspaceTree should not be withOutline.persisted.layout.workspaceTree
    byPosition shouldBe byId
    valid(byId) shouldBe true
  }

  "ModalStateReducer.show and dismiss" should "open and close a modal with focus following it" in {
    val modal = Modal.CloseWorkflow(CloseWorkflowState(CloseScope.Current, BufferId(0), "notes.scala"))

    val shown     = ModalStateReducer.show(modal, AppState.initial).state
    val dismissed = ModalStateReducer.dismiss(shown).state

    shown.persisted.focus shouldBe Focus.Modal
    valid(shown) shouldBe true
    dismissed.runtime.modalStack shouldBe empty
    valid(dismissed) shouldBe true
  }
