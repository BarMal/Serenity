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

  "ModalStateReducer" should "keep a blocking confirmation above its parent and restore parent focus on dismissal" in {
    val parent = UiSurface(
      SurfaceId("parent"),
      SurfaceContent.ModalWorkflow(closeWorkflow),
      SurfacePresentation.Modal
    )
    val initialState = AppState.initial
    val parentState = initialState.copy(
      persisted = initialState.persisted.copy(focus = Focus.Surface(parent.id)),
      runtime = initialState.runtime.copy(uiSurfaces = List(parent))
    )

    val shown     = ModalStateReducer.show(closeWorkflow, parentState).state
    val child     = shown.topBlockingModalSurface.getOrElse(fail("expected child confirmation"))
    val dismissed = ModalStateReducer.dismiss(shown).state

    child.id should not be parent.id
    child.presentation shouldBe SurfacePresentation.Modal
    shown.modalSurfaces.map(_.id) shouldBe List(parent.id, child.id)
    shown.blockingModalSurfaces.map(_.id) shouldBe List(parent.id, child.id)
    shown.persisted.focus shouldBe Focus.Surface(child.id)
    dismissed.blockingModalSurfaces.map(_.id) shouldBe List(parent.id)
    dismissed.persisted.focus shouldBe Focus.Surface(parent.id)
  }

  it should "keep non-blocking find workflows on the floating layer" in {
    val shown = ModalStateReducer.show(Modal.Find("", Nil, 0), AppState.initial).state

    shown.blockingModalSurfaces shouldBe Nil
    shown.modalSurface.map(_.id) shouldBe shown.floatingSurfaces.headOption.map(_.id)
  }

  it should "reject a modeless workflow while a blocking confirmation owns focus" in {
    val blocking = ModalStateReducer.show(closeWorkflow, AppState.initial).state

    ModalStateReducer.show(Modal.Find("", Nil, 0), blocking).state shouldBe blocking
  }

  it should "use Modal presentation for FileWorkflow so the dialog is always centered and visible" in {
    val fileWorkflow = Modal.FileWorkflow(FileWorkflowState(mode = FileWorkflowMode.Open))
    val shown        = ModalStateReducer.show(fileWorkflow, AppState.initial).state

    shown.blockingModalSurfaces should have size 1
    shown.blockingModalSurfaces.head.presentation shouldBe SurfacePresentation.Modal
  }

  it should "block a non-blocking modal while a file dialog is open" in {
    val fileWorkflow = Modal.FileWorkflow(FileWorkflowState(mode = FileWorkflowMode.Open))
    val withDialog   = ModalStateReducer.show(fileWorkflow, AppState.initial).state

    ModalStateReducer.show(Modal.Find("", Nil, 0), withDialog).state shouldBe withDialog
  }
end ModalStateReducerSpec
