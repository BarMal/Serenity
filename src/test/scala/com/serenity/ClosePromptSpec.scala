package com.serenity

import com.serenity.command.CloseCommands
import com.serenity.keystroke.events.*
import com.serenity.rope.Balance
import com.serenity.state.models.*
import com.serenity.state.reducers.{AppEffect, ModalEventReducer, ModalStateReducer}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** "Save changes before closing?" is a [[ConfirmPrompt.closeUnsaved]]: its answers, Escape included, run the commands
  * that resolve the close waiting on the action stack.
  */
class ClosePromptSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private val workflow = CloseWorkflowState(CloseScope.Current, BufferId(0), "notes.scala")

  private val prompted: AppState =
    val pending = AppState.initial.copy(runtime =
      AppState.initial.runtime.copy(actionStack = List(AppAction.CloseWorkflow(workflow)))
    )
    ModalStateReducer.show(Modal.Confirm(workflow.prompt), pending).state

  "The close prompt" should "move between its answers and run the highlighted one on Enter" in {
    val moved = ModalEventReducer.reduce(ModalType.Confirm, TabKey, prompted).state
    ClosePromptFixtures.closePromptHighlight(moved) shouldBe Some("Close Anyway")

    val result = ModalEventReducer.reduce(ModalType.Confirm, Enter, moved)
    result.state.topModal shouldBe None
    result.effects shouldBe List(AppEffect.ExecuteCommand(CloseCommands.resolve(CloseWorkflowChoice.Discard)))
  }

  it should "cancel the close on Escape, as Cancel does" in {
    val result = ModalEventReducer.reduce(ModalType.Confirm, Escape, prompted)

    result.state.topModal shouldBe None
    result.effects shouldBe List(AppEffect.ExecuteCommand(CloseCommands.resolve(CloseWorkflowChoice.Cancel)))
  }

  "Dismissing the Save As a close's Save opened" should "bring the close prompt back" in {
    val saveAs = ModalStateReducer
      .show(Modal.FileWorkflow(FileWorkflowState(mode = FileWorkflowMode.SaveAs)), prompted.dismissTopModal)
      .state

    val dismissed = ModalEventReducer.reduce(ModalType.FileWorkflow, Escape, saveAs).state

    ClosePromptFixtures.closePromptShown(dismissed) shouldBe Some(workflow)
  }
