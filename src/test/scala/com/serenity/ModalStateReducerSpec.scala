package com.serenity

import com.serenity.rope.Balance
import com.serenity.state.models.*
import com.serenity.state.reducers.ModalStateReducer
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ModalStateReducerSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private val closeWorkflow =
    Modal.CloseWorkflow(CloseWorkflowState(CloseScope.Current, BufferId(0), "notes.scala"))

  "ModalStateReducer" should "keep a blocking confirmation above its parent on an explicit modal layer, outside uiSurfaces" in {
    val parentShown = ModalStateReducer.show(closeWorkflow, AppState.initial).state
    val childShown  = ModalStateReducer.show(closeWorkflow, parentShown).state

    val Seq(parent, child) = childShown.runtime.modalStack: @unchecked

    child.id should not be parent.id
    childShown.runtime.modalStack.map(_.id) shouldBe List(parent.id, child.id)
    childShown.persisted.focus shouldBe Focus.Modal
    // The modal layer is structurally outside uiSurfaces, not a tagged member of it (#814's outcome).
    childShown.runtime.uiSurfaces shouldBe empty

    val dismissedChild = ModalStateReducer.dismiss(childShown).state
    dismissedChild.runtime.modalStack.map(_.id) shouldBe List(parent.id)
    // A dialog remains open, so focus scope stays at the modal layer rather than escaping to whatever was behind it.
    dismissedChild.persisted.focus shouldBe Focus.Modal

    val dismissedParent = ModalStateReducer.dismiss(dismissedChild).state
    dismissedParent.runtime.modalStack shouldBe empty
    // The last dialog closed: focus scope releases back to what was focused before any modal opened.
    dismissedParent.persisted.focus shouldBe Focus.EditorPane(PaneId(0))
  }

  it should "keep non-blocking find workflows on the floating layer" in {
    val shown = ModalStateReducer.show(Modal.Find("", Nil, 0), AppState.initial).state

    shown.runtime.modalStack shouldBe Nil
    shown.modalSurface.map(_.id) shouldBe shown.floatingSurfaces.headOption.map(_.id)
  }

  it should "reject a modeless workflow while a blocking confirmation owns focus" in {
    val blocking = ModalStateReducer.show(closeWorkflow, AppState.initial).state

    ModalStateReducer.show(Modal.Find("", Nil, 0), blocking).state shouldBe blocking
  }

  it should "put FileWorkflow on the modal layer, centered, so the open dialog is always visible" in {
    val fileWorkflow = Modal.FileWorkflow(FileWorkflowState(mode = FileWorkflowMode.Open))
    val shown        = ModalStateReducer.show(fileWorkflow, AppState.initial).state

    shown.runtime.modalStack should have size 1
    shown.runtime.modalStack.head.modal shouldBe fileWorkflow
    shown.runtime.modalStack.head.placement shouldBe ModalPlacement.Centered
  }

  it should "block a non-blocking modal while a file dialog is open" in {
    val fileWorkflow = Modal.FileWorkflow(FileWorkflowState(mode = FileWorkflowMode.Open))
    val withDialog   = ModalStateReducer.show(fileWorkflow, AppState.initial).state

    ModalStateReducer.show(Modal.Find("", Nil, 0), withDialog).state shouldBe withDialog
  }
end ModalStateReducerSpec
