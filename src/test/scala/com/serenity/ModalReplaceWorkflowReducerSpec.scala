package com.serenity

import com.serenity.keystroke.events.*
import com.serenity.rope.Balance
import com.serenity.state.models.*
import com.serenity.state.reducers.{AppEffect, ModalEventReducer, WorkflowEffect}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ModalReplaceWorkflowReducerSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  "ModalEventReducer" should "edit replace workflow fields, switch action and scope, and queue submission" in {
    val initialWorkflow = ReplaceWorkflowState()
    val initialState = AppState.initial.copy(
      persisted = AppState.initial.persisted.copy(focus = Focus.Surface(SurfaceId("replace-workflow"))),
      runtime = AppState.initial.runtime.copy(
        uiSurfaces = List(
          UiSurface(
            SurfaceId("replace-workflow"),
            SurfaceContent.ModalWorkflow(Modal.ReplaceWorkflow(initialWorkflow)),
            SurfacePresentation.Floating(None, SurfacePlacement.BelowCursor)
          )
        )
      )
    )

    val withFind = ModalEventReducer.reduce(ModalType.ReplaceWorkflow, InsertChar('n'), initialState).state
    withFind.modalSurface.map(_.content) shouldBe Some(
      SurfaceContent.ModalWorkflow(
        Modal.ReplaceWorkflow(
          initialWorkflow.copy(findText = "n", statusMessage = Some("0 matches in current buffer"))
        )
      )
    )

    val withReplacementField = ModalEventReducer.reduce(ModalType.ReplaceWorkflow, TabKey, withFind).state
    withReplacementField.modalSurface.map(_.content) shouldBe Some(
      SurfaceContent.ModalWorkflow(
        Modal.ReplaceWorkflow(
          initialWorkflow.copy(
            findText = "n",
            activeField = ReplaceWorkflowField.ReplaceWith,
            statusMessage = Some("0 matches in current buffer")
          )
        )
      )
    )

    val withReplacement =
      ModalEventReducer.reduce(ModalType.ReplaceWorkflow, InsertChar('x'), withReplacementField).state
    withReplacement.modalSurface.map(_.content) shouldBe Some(
      SurfaceContent.ModalWorkflow(
        Modal.ReplaceWorkflow(
          initialWorkflow.copy(
            findText = "n",
            replacementText = "x",
            activeField = ReplaceWorkflowField.ReplaceWith,
            statusMessage = Some("0 matches in current buffer")
          )
        )
      )
    )

    val withReplaceNext = ModalEventReducer
      .reduce(
        ModalType.ReplaceWorkflow,
        ModalNavigate(Direction.Left),
        withReplacement
      )
      .state
    withReplaceNext.modalSurface.map(_.content) shouldBe Some(
      SurfaceContent.ModalWorkflow(
        Modal.ReplaceWorkflow(
          initialWorkflow.copy(
            findText = "n",
            replacementText = "x",
            activeField = ReplaceWorkflowField.ReplaceWith,
            selectedAction = ReplaceWorkflowAction.ReplaceNext,
            statusMessage = Some("0 matches in current buffer")
          )
        )
      )
    )

    val withSelectionScope = ModalEventReducer
      .reduce(
        ModalType.ReplaceWorkflow,
        ModalNavigate(Direction.Down),
        withReplaceNext
      )
      .state
    withSelectionScope.modalSurface.map(_.content) shouldBe Some(
      SurfaceContent.ModalWorkflow(
        Modal.ReplaceWorkflow(
          initialWorkflow.copy(
            findText = "n",
            replacementText = "x",
            activeField = ReplaceWorkflowField.ReplaceWith,
            selectedAction = ReplaceWorkflowAction.ReplaceNext,
            selectedScope = ReplaceWorkflowScope.Selection,
            statusMessage = Some("Select text to preview selection matches")
          )
        )
      )
    )

    val submitted = ModalEventReducer.reduce(ModalType.ReplaceWorkflow, Enter, withSelectionScope)
    submitted.state shouldBe withSelectionScope
    submitted.effects shouldBe List(
      AppEffect.Workflow(WorkflowEffect.SubmitReplaceWorkflow(SurfaceId("replace-workflow")))
    )
  }

  it should "preview replace match counts while editing the find text" in {
    val initialWorkflow = ReplaceWorkflowState()
    val buffer = AppState.initial.persisted
      .buffers(BufferId(0))
      .copy(
        document = AppState.initial.persisted
          .buffers(BufferId(0))
          .document
          .copy(content = com.serenity.rope.Rope("needle one\nneedle two\nplain"))
      )
    val initialState = AppState.initial.copy(
      persisted = AppState.initial.persisted.copy(
        buffers = AppState.initial.persisted.buffers + (BufferId(0) -> buffer),
        focus = Focus.Surface(SurfaceId("replace-workflow"))
      ),
      runtime = AppState.initial.runtime.copy(
        uiSurfaces = List(
          UiSurface(
            SurfaceId("replace-workflow"),
            SurfaceContent.ModalWorkflow(Modal.ReplaceWorkflow(initialWorkflow)),
            SurfacePresentation.Floating(None, SurfacePlacement.BelowCursor)
          )
        )
      )
    )

    val updatedState = "needle".foldLeft(initialState) { (state, char) =>
      ModalEventReducer.reduce(ModalType.ReplaceWorkflow, InsertChar(char), state).state
    }

    updatedState.modalSurface.map(_.content) shouldBe Some(
      SurfaceContent.ModalWorkflow(
        Modal.ReplaceWorkflow(
          initialWorkflow.copy(findText = "needle", statusMessage = Some("2 matches in current buffer"))
        )
      )
    )
  }

  it should "preview replace matches inside the active selection scope" in {
    val initialWorkflow = ReplaceWorkflowState(
      findText = "needle",
      replacementText = "thread",
      activeField = ReplaceWorkflowField.ReplaceWith,
      selectedAction = ReplaceWorkflowAction.ReplaceNext,
      statusMessage = Some("2 matches in current buffer")
    )
    val buffer = AppState.initial.persisted
      .buffers(BufferId(0))
      .copy(
        document = AppState.initial.persisted
          .buffers(BufferId(0))
          .document
          .copy(content = com.serenity.rope.Rope("needle one\nneedle two\nplain")),
        editing = AppState.initial.persisted
          .buffers(BufferId(0))
          .editing
          .copy(selection = Some(Selection(CursorPosition(1, 0), CursorPosition(1, "needle two".length))))
      )
    val initialState = AppState.initial.copy(
      persisted = AppState.initial.persisted.copy(
        buffers = AppState.initial.persisted.buffers + (BufferId(0) -> buffer),
        focus = Focus.Surface(SurfaceId("replace-workflow"))
      ),
      runtime = AppState.initial.runtime.copy(
        uiSurfaces = List(
          UiSurface(
            SurfaceId("replace-workflow"),
            SurfaceContent.ModalWorkflow(Modal.ReplaceWorkflow(initialWorkflow)),
            SurfacePresentation.Floating(None, SurfacePlacement.BelowCursor)
          )
        )
      )
    )

    val updatedState = ModalEventReducer
      .reduce(
        ModalType.ReplaceWorkflow,
        ModalNavigate(Direction.Down),
        initialState
      )
      .state

    updatedState.modalSurface.map(_.content) shouldBe Some(
      SurfaceContent.ModalWorkflow(
        Modal.ReplaceWorkflow(
          initialWorkflow.copy(
            selectedScope = ReplaceWorkflowScope.Selection,
            statusMessage = Some("1 match in selection")
          )
        )
      )
    )
  }
