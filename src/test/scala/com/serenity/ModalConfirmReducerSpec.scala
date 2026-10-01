package com.serenity

import com.serenity.command.ExternalChangeCommands
import com.serenity.keystroke.events.*
import com.serenity.rope.Balance
import com.serenity.state.models.*
import com.serenity.state.reducers.{AppEffect, ModalEventReducer, ReducerResult}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Covers `ModalConfirmReducer` through the reload-conflict prompt (#1623): choice cycling, click selection, submission
  * running the chosen command, and dismissal.
  */
class ModalConfirmReducerSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private val prompt = ConfirmPrompt.reloadConflict(BufferId(0), "notes.md")

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

  private def highlighted(state: AppState): Option[String] =
    state.modalSurface
      .map(_.content)
      .collect { case SurfaceContent.ModalWorkflow(Modal.Confirm(shown)) => shown.selectedChoice.map(_.label) }
      .flatten

  "ModalEventReducer" should "cycle confirm choices, and run the chosen command on Enter" in {
    val moved = ModalEventReducer.reduce(ModalType.Confirm, TabKey, stateWith(Modal.Confirm(prompt))).state
    highlighted(moved) shouldBe Some("Overwrite")

    val result = ModalEventReducer.reduce(ModalType.Confirm, Enter, moved)
    result.state.modalSurface shouldBe None
    result.effects shouldBe List(AppEffect.ExecuteCommand(ExternalChangeCommands.overwriteOnDisk(BufferId(0))))
  }

  it should "just close when the chosen answer only dismisses" in {
    val onCancel = ModalEventReducer.reduce(ModalType.Confirm, MoveUp, stateWith(Modal.Confirm(prompt))).state
    highlighted(onCancel) shouldBe Some("Cancel")

    val result = ModalEventReducer.reduce(ModalType.Confirm, Enter, onCancel)
    result.state.modalSurface shouldBe None
    result.effects shouldBe Nil
  }

  it should "select a confirm choice on click without submitting" in {
    val clicked = ModalEventReducer.reduce(
      ModalType.Confirm,
      ModalClick("confirm-choice-2", Some("confirm-choice-2")),
      stateWith(Modal.Confirm(prompt))
    )

    clicked.effects shouldBe Nil
    highlighted(clicked.state) shouldBe Some("Cancel")
  }

  it should "dismiss the prompt without queuing an effect" in {
    val result = ModalEventReducer.reduce(ModalType.Confirm, ModalDismiss, stateWith(Modal.Confirm(prompt)))

    result.effects shouldBe Nil
    result.state.modalSurface shouldBe None
  }

  it should "ignore confirm input when no such modal is active" in {
    val plainState = AppState.initial

    ModalEventReducer.reduce(ModalType.Confirm, Enter, plainState) shouldBe ReducerResult.noEffects(plainState)
  }
end ModalConfirmReducerSpec
