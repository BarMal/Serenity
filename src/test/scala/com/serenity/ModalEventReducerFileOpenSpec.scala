package com.serenity

import com.serenity.keystroke.events.*
import com.serenity.rope.Balance
import com.serenity.state.models.*
import com.serenity.state.reducers.{AppEffect, ModalEventReducer, WorkflowEffect}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Reducer-level coverage for the open dialog's directory-browser Tab behavior (#1289), kept out of
  * `ModalEventReducerSpec` so that already-oversized file does not grow past its architecture-check baseline.
  */
class ModalEventReducerFileOpenSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  "ModalEventReducer" should "submit immediately when tab accepts a file suggestion in the open dialog's Path field" in {
    val initialWorkflow = OpenFileWorkflowState(
      path = "/tmp",
      activeField = FileWorkflowField.Path,
      suggestions = List(
        FileWorkflowSuggestion("/tmp/notes.txt", isDirectory = false)
      )
    )
    val initialState = AppState.initial.copy(
      persisted = AppState.initial.persisted.copy(focus = Focus.Surface(SurfaceId("file-workflow"))),
      runtime = AppState.initial.runtime.copy(
        uiSurfaces = List(
          UiSurface(
            SurfaceId("file-workflow"),
            SurfaceContent.ModalWorkflow(Modal.FileWorkflow(initialWorkflow)),
            SurfacePresentation.Modal
          )
        )
      )
    )

    val result = ModalEventReducer.reduce(ModalType.FileWorkflow, TabKey, initialState)

    result.effects shouldBe List(AppEffect.Workflow(WorkflowEffect.SubmitFileWorkflow(SurfaceId("file-workflow"))))
    result.state.modalSurface.map(_.content) shouldBe Some(
      SurfaceContent.ModalWorkflow(
        Modal.FileWorkflow(initialWorkflow.copy(path = "/tmp/notes.txt"))
      )
    )
  }
