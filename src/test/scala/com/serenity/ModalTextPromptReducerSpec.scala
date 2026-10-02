package com.serenity

import com.serenity.keystroke.events.*
import com.serenity.rope.Balance
import com.serenity.state.models.*
import com.serenity.state.reducers.{AppEffect, ModalEventReducer, WorkflowEffect}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Editing every one-field prompt goes through the toolkit `TextField`: the caret moves, and text goes in and comes out
  * where it is, not just at the end.
  */
class ModalTextPromptReducerSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private val promptId = SurfaceId("prompt")

  private def stateWith(prompt: TextPrompt): AppState =
    AppState.initial.copy(
      persisted = AppState.initial.persisted.copy(focus = Focus.Surface(promptId)),
      runtime = AppState.initial.runtime.copy(
        uiSurfaces = List(
          UiSurface(
            promptId,
            SurfaceContent.ModalWorkflow(Modal.TextPrompt(prompt)),
            SurfacePresentation.Floating(None, SurfacePlacement.BelowCursor)
          )
        )
      )
    )

  private def after(state: AppState, events: Event*): AppState =
    events.foldLeft(state)((current, event) => ModalEventReducer.reduce(ModalType.TextPrompt, event, current).state)

  private def shown(state: AppState): Option[TextPrompt] =
    state.modalSurface.map(_.content).collect { case SurfaceContent.ModalWorkflow(Modal.TextPrompt(prompt)) => prompt }

  "A text prompt" should "insert and delete at the caret, not only at the end" in {
    val edited = after(stateWith(TextPrompt.sessionName(SessionNamePromptMode.SaveAs, "drft")), MoveLeft, MoveLeft)
    val fixed  = after(edited, InsertChar('a'), MoveRight, DeleteForward)

    shown(fixed).map(field => (field.input, field.field.caret)) shouldBe Some(("draf", 4))
  }

  it should "move the caret to either end with Home and End" in {
    val atStart = after(stateWith(TextPrompt.sessionName(SessionNamePromptMode.SaveAs, "draft")), ModalLineStart)
    shown(atStart).map(_.field.caret) shouldBe Some(0)

    val edited = after(atStart, InsertChar('a'), ModalLineEnd, InsertChar('z'))
    shown(edited).map(field => (field.input, field.field.caret)) shouldBe Some(("adraftz", 7))
  }

  it should "take only digits for go to line" in {
    shown(after(stateWith(TextPrompt.gotoLine("1")), InsertChar('x'), InsertChar('2'))).map(_.input) shouldBe Some("12")
  }

  it should "hand a non-blank session name to the session workflow, keeping the prompt until it is saved" in {
    val result = ModalEventReducer.reduce(
      ModalType.TextPrompt,
      Enter,
      stateWith(TextPrompt.sessionName(SessionNamePromptMode.SaveAs, "Novel"))
    )

    result.effects shouldBe List(AppEffect.Workflow(WorkflowEffect.SubmitSessionNamePrompt(promptId)))
    shown(result.state).map(_.input) shouldBe Some("Novel")
  }

  it should "just close when a session name is left blank" in {
    val result =
      ModalEventReducer.reduce(
        ModalType.TextPrompt,
        Enter,
        stateWith(TextPrompt.sessionName(SessionNamePromptMode.SaveAs, "  "))
      )

    result.effects shouldBe Nil
    result.state.modalSurface shouldBe None
  }
end ModalTextPromptReducerSpec
