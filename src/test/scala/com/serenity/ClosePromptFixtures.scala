package com.serenity

import com.serenity.state.models.*
import com.serenity.ui.layout.{ConfirmComposition, SurfaceActionId}

/** The close prompt as tests see it: a [[ConfirmPrompt.closeUnsaved]] on the modal stack, answering the close that
  * waits on the action stack.
  */
object ClosePromptFixtures:

  val SaveChoice: SurfaceActionId    = ConfirmComposition.choiceActionId(0)
  val DiscardChoice: SurfaceActionId = ConfirmComposition.choiceActionId(1)
  val CancelChoice: SurfaceActionId  = ConfirmComposition.choiceActionId(2)

  def isClosePrompt(modal: Modal): Boolean =
    modal match
      case Modal.Confirm(prompt) => prompt.title == ConfirmPrompt.closeUnsaved("").title
      case _                     => false

  /** The close the shown close prompt is asking about, if the prompt is up. */
  def closePromptShown(state: AppState): Option[CloseWorkflowState] =
    state.topModal.filter(dialog => isClosePrompt(dialog.modal)).flatMap(_ => pendingClose(state))

  /** The label of the close prompt's highlighted choice, if the prompt is up. */
  def closePromptHighlight(state: AppState): Option[String] =
    state.topModal
      .map(_.modal)
      .collect {
        case modal @ Modal.Confirm(prompt) if isClosePrompt(modal) => prompt.selectedChoice.map(_.label)
      }
      .flatten

  def pendingClose(state: AppState): Option[CloseWorkflowState] =
    state.runtime.actionStack.collectFirst { case AppAction.CloseWorkflow(workflow) => workflow }
