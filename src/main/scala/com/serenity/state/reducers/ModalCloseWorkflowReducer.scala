package com.serenity.state.reducers

import com.serenity.keystroke.events.*
import com.serenity.state.models.*

/** Close-workflow modal input -- the "save changes before closing?" prompt's choice cycling and submission. Split out
  * of `ModalEventReducer`'s per-modal-type dispatch when that file grew past its 600-line target.
  */
private[reducers] object ModalCloseWorkflowReducer:
  import ModalEventReducer.{currentModal, dismissToPane, updateModal}

  def reduce(event: ModalInputEvent, currentState: AppState): ReducerResult =
    event match
      case ModalDismiss =>
        ReducerResult.noEffects(cancelCloseWorkflow(currentState))
      case ModalNextField | ModalNavigate(Direction.Right) | ModalNavigate(Direction.Down) =>
        currentModal(currentState) match
          case Some((id, Modal.CloseWorkflow(workflow))) =>
            ReducerResult.noEffects(updateModal(currentState, id, Modal.CloseWorkflow(workflow.moveChoice(1))))
          case _ =>
            ReducerResult.noEffects(currentState)
      case ModalPreviousField | ModalNavigate(Direction.Left) | ModalNavigate(Direction.Up) =>
        currentModal(currentState) match
          case Some((id, Modal.CloseWorkflow(workflow))) =>
            ReducerResult.noEffects(updateModal(currentState, id, Modal.CloseWorkflow(workflow.moveChoice(-1))))
          case _ =>
            ReducerResult.noEffects(currentState)
      case ModalSubmit =>
        currentModal(currentState) match
          case Some((id, Modal.CloseWorkflow(_))) =>
            ReducerResult.withEffect(currentState, AppEffect.Workflow(WorkflowEffect.SubmitCloseWorkflow(id)))
          case _ =>
            ReducerResult.noEffects(currentState)
      case ModalClick(_, Some(actionId)) =>
        currentModal(currentState) match
          case Some((id, Modal.CloseWorkflow(workflow))) =>
            val choice = actionId match
              case "close-save"    => Some(CloseWorkflowChoice.Save)
              case "close-discard" => Some(CloseWorkflowChoice.Discard)
              case "close-cancel"  => Some(CloseWorkflowChoice.Cancel)
              case _               => None
            ReducerResult.noEffects(
              choice.fold(currentState)(selected =>
                updateModal(
                  currentState,
                  id,
                  Modal.CloseWorkflow(workflow.copy(selectedChoice = selected))
                )
              )
            )
          case _ =>
            ReducerResult.noEffects(currentState)
      case ModalClick(_, _) =>
        ReducerResult.noEffects(currentState)
      case _ =>
        ReducerResult.noEffects(currentState)

  def selectCloseWorkflowChoice(choice: CloseWorkflowChoice, currentState: AppState): ReducerResult =
    currentModal(currentState) match
      case Some((id, Modal.CloseWorkflow(workflow))) =>
        ReducerResult.noEffects(
          updateModal(currentState, id, Modal.CloseWorkflow(workflow.copy(selectedChoice = choice)))
        )
      case _ =>
        ReducerResult.noEffects(currentState)

  private def cancelCloseWorkflow(state: AppState): AppState =
    dismissToPane(
      state.copy(runtime = state.runtime.copy(actionStack = state.runtime.actionStack.filter {
        case AppAction.CloseWorkflow(_) => false
      }))
    )
