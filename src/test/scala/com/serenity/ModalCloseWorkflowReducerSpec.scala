package com.serenity

import com.serenity.keystroke.events.*
import com.serenity.rope.Balance
import com.serenity.state.models.*
import com.serenity.state.reducers.{AppEffect, ModalEventReducer, WorkflowEffect}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ModalCloseWorkflowReducerSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  "ModalEventReducer" should "cycle close workflow choices and queue close workflow submission on enter" in {
    val initialWorkflow = CloseWorkflowState(
      scope = CloseScope.Current,
      currentBufferId = BufferId(0),
      currentBufferLabel = "notes.scala"
    )
    val initialState = AppState.initial.copy(
      persisted = AppState.initial.persisted.copy(focus = Focus.Surface(SurfaceId("close-workflow"))),
      runtime = AppState.initial.runtime.copy(
        uiSurfaces = List(
          UiSurface(
            SurfaceId("close-workflow"),
            SurfaceContent.ModalWorkflow(Modal.CloseWorkflow(initialWorkflow)),
            SurfacePresentation.Floating(None, SurfacePlacement.BelowCursor)
          )
        )
      )
    )

    val moved = ModalEventReducer.reduce(ModalType.CloseWorkflow, TabKey, initialState).state
    moved.modalSurface.map(_.content) shouldBe Some(
      SurfaceContent.ModalWorkflow(
        Modal.CloseWorkflow(initialWorkflow.copy(selectedChoice = CloseWorkflowChoice.Discard))
      )
    )

    val result = ModalEventReducer.reduce(ModalType.CloseWorkflow, Enter, moved)
    result.state shouldBe moved
    result.effects shouldBe List(AppEffect.Workflow(WorkflowEffect.SubmitCloseWorkflow(SurfaceId("close-workflow"))))
  }
