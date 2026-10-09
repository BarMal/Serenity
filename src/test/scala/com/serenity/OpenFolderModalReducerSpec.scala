package com.serenity

import java.io.File.separator

import com.serenity.keystroke.events.*
import com.serenity.rope.Balance
import com.serenity.state.models.*
import com.serenity.state.reducers.{AppEffect, ModalEventReducer, WorkflowEffect}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class OpenFolderModalReducerSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private val surfaceId = SurfaceId("open-folder")

  private val folderForm = FileWorkflowState(
    mode = FileWorkflowMode.OpenFolder,
    path = "/tmp",
    activeField = FileWorkflowField.Path,
    suggestions = List(
      FileWorkflowSuggestion("/tmp/alpha", isDirectory = true),
      FileWorkflowSuggestion("/tmp/beta", isDirectory = true)
    )
  )

  private def stateWith(workflow: FileWorkflowState): AppState =
    AppState.initial.copy(
      persisted = AppState.initial.persisted.copy(focus = Focus.Modal),
      runtime = AppState.initial.runtime.copy(
        modalStack = List(ModalDialog(surfaceId, Modal.FileWorkflow(workflow), ModalPlacement.Centered))
      )
    )

  private def workflowIn(state: AppState): Option[FileWorkflowState] =
    state.topModal.map(_.modal).collect { case Modal.FileWorkflow(workflow) => workflow }

  private def press(event: ModalInputEvent, state: AppState) =
    ModalEventReducer.reduce(ModalType.FileWorkflow, event, state)

  "An Open Folder form" should "be titled Open Folder and be its own mode" in {
    folderForm.mode shouldBe FileWorkflowMode.OpenFolder
    folderForm.operationLabel shouldBe "Open Folder"
    folderForm.canOpenAsProjectRoot shouldBe true
  }

  it should "keep the generic Open form and Save As on their own modes, only Save As unable to open a root" in {
    FileWorkflowState(mode = FileWorkflowMode.Open).canOpenAsProjectRoot shouldBe true
    FileWorkflowState(mode = FileWorkflowMode.SaveAs).canOpenAsProjectRoot shouldBe false
  }

  it should "queue a submission on Enter, which browses into the folder the Path names" in {
    val result = press(ModalSubmit, stateWith(folderForm))

    result.state shouldBe stateWith(folderForm)
    result.effects shouldBe List(AppEffect.Workflow(WorkflowEffect.SubmitFileWorkflow(surfaceId)))
  }

  it should "queue opening the shown folder as the project root on the confirm key" in {
    val result = press(ModalOpenAsProjectRoot, stateWith(folderForm))

    result.state shouldBe stateWith(folderForm)
    result.effects shouldBe List(AppEffect.Workflow(WorkflowEffect.OpenFileWorkflowAsProjectRoot(surfaceId)))
  }

  it should "cancel on Escape, dismissing the form without any effect" in {
    val result = press(ModalDismiss, stateWith(folderForm))

    result.state.topModal shouldBe None
    result.effects shouldBe Nil
  }

  it should "move the highlight with the arrow keys and wrap round" in {
    val down = press(ModalNavigate(Direction.Down), stateWith(folderForm)).state
    workflowIn(down).map(_.selectedSuggestionIndex) shouldBe Some(1)

    val wrapped = press(ModalNavigate(Direction.Down), down).state
    workflowIn(wrapped).map(_.selectedSuggestionIndex) shouldBe Some(0)

    val up = press(ModalNavigate(Direction.Up), stateWith(folderForm)).state
    workflowIn(up).map(_.selectedSuggestionIndex) shouldBe Some(1)
  }

  it should "jump to the first and last folder with Ctrl+Home and Ctrl+End" in {
    val last = press(ModalLast, stateWith(folderForm)).state
    workflowIn(last).map(_.selectedSuggestionIndex) shouldBe Some(1)
    workflowIn(press(ModalFirst, last).state).map(_.selectedSuggestionIndex) shouldBe Some(0)
  }

  it should "descend into the highlighted folder on Tab and relist, never submitting" in {
    val highlighted = press(ModalNavigate(Direction.Down), stateWith(folderForm)).state
    val result      = press(ModalNextField, highlighted)

    workflowIn(result.state).map(_.path) shouldBe Some(s"/tmp/beta$separator")
    result.effects shouldBe List(AppEffect.Workflow(WorkflowEffect.RefreshFileWorkflow(surfaceId)))
  }

  it should "keep Tab on the Path field when there is nothing to descend into" in {
    val empty  = folderForm.updated(suggestions = Nil)
    val result = press(ModalNextField, stateWith(empty))

    workflowIn(result.state).map(_.activeField) shouldBe Some(FileWorkflowField.Path)
    result.effects shouldBe List(AppEffect.Workflow(WorkflowEffect.RefreshFileWorkflow(surfaceId)))
  }

  it should "edit the Path as the user types and relist" in {
    val typed = press(ModalInsertChar('x'), stateWith(folderForm))
    workflowIn(typed.state).map(_.path) shouldBe Some("/tmpx")
    typed.effects shouldBe List(AppEffect.Workflow(WorkflowEffect.RefreshFileWorkflow(surfaceId)))

    val deleted = press(ModalDeleteBackward, typed.state)
    workflowIn(deleted.state).map(_.path) shouldBe Some("/tmp")
  }

  it should "ignore a click on the Filename row, which this form does not have" in {
    val result = press(ModalClick("filename", None), stateWith(folderForm))

    workflowIn(result.state).map(_.activeField) shouldBe Some(FileWorkflowField.Path)
    result.effects shouldBe Nil
  }

  it should "select a folder by clicking it" in {
    val result = press(ModalClick("file-suggestion-1", Some("file-suggestion-1")), stateWith(folderForm))

    workflowIn(result.state).map(_.selectedSuggestionIndex) shouldBe Some(1)
  }

  it should "leave the generic Open form's confirm key working" in {
    val open   = FileWorkflowState(mode = FileWorkflowMode.Open, path = "/tmp", activeField = FileWorkflowField.Path)
    val result = press(ModalOpenAsProjectRoot, stateWith(open))

    result.effects shouldBe List(AppEffect.Workflow(WorkflowEffect.OpenFileWorkflowAsProjectRoot(surfaceId)))
  }
end OpenFolderModalReducerSpec
