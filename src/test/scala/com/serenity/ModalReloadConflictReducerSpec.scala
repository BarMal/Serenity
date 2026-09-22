package com.serenity

import com.serenity.keystroke.events.*
import com.serenity.rope.Balance
import com.serenity.state.models.*
import com.serenity.state.reducers.{AppEffect, ModalEventReducer, WorkflowEffect}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Covers `ModalReloadConflictReducer` (#1623): the "file changed on disk" prompt's choice cycling, click selection,
  * submission, and dismiss -- mirrors `ModalCloseWorkflowReducerSpec`'s coverage shape for its sibling reducer.
  */
class ModalReloadConflictReducerSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private def stateWith(modal: Modal): AppState =
    AppState.initial.copy(
      persisted = AppState.initial.persisted.copy(focus = Focus.Surface(SurfaceId("reload-conflict"))),
      runtime = AppState.initial.runtime.copy(
        uiSurfaces = List(
          UiSurface(
            SurfaceId("reload-conflict"),
            SurfaceContent.ModalWorkflow(modal),
            SurfacePresentation.Floating(None, SurfacePlacement.BelowCursor)
          )
        )
      )
    )

  "ModalEventReducer" should "cycle reload-conflict choices and queue submission on enter" in {
    val initialWorkflow = ReloadConflictState(BufferId(0), "notes.md")
    val initialState    = stateWith(Modal.ReloadConflict(initialWorkflow))

    val moved = ModalEventReducer.reduce(ModalType.ReloadConflict, TabKey, initialState).state
    moved.modalSurface.map(_.content) shouldBe Some(
      SurfaceContent.ModalWorkflow(
        Modal.ReloadConflict(initialWorkflow.copy(selectedChoice = ReloadConflictChoice.Overwrite))
      )
    )

    val result = ModalEventReducer.reduce(ModalType.ReloadConflict, Enter, moved)
    result.state shouldBe moved
    result.effects shouldBe List(
      AppEffect.Workflow(WorkflowEffect.SubmitReloadConflict(SurfaceId("reload-conflict")))
    )
  }

  it should "select a reload-conflict choice on click without submitting" in {
    val initialWorkflow = ReloadConflictState(BufferId(0), "notes.md")
    val initialState    = stateWith(Modal.ReloadConflict(initialWorkflow))

    val clicked = ModalEventReducer.reduce(
      ModalType.ReloadConflict,
      ModalClick("reload-conflict-cancel", Some("reload-conflict-cancel")),
      initialState
    )

    clicked.effects shouldBe Nil
    clicked.state.modalSurface.map(_.content) shouldBe Some(
      SurfaceContent.ModalWorkflow(Modal.ReloadConflict(initialWorkflow.copy(selectedChoice = ReloadConflictChoice.Cancel)))
    )
  }

  it should "dismiss the reload-conflict modal without queuing an effect" in {
    val initialState = stateWith(Modal.ReloadConflict(ReloadConflictState(BufferId(0), "notes.md")))

    val result = ModalEventReducer.reduce(ModalType.ReloadConflict, ModalDismiss, initialState)

    result.effects shouldBe Nil
    result.state.modalSurface shouldBe None
  }

  it should "ignore reload-conflict input when no such modal is active" in {
    val plainState = AppState.initial

    ModalEventReducer.reduce(ModalType.ReloadConflict, Enter, plainState) shouldBe
      com.serenity.state.reducers.ReducerResult.noEffects(plainState)
  }
