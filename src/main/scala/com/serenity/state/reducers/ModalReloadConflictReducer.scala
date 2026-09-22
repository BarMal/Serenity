package com.serenity.state.reducers

import com.serenity.keystroke.events.*
import com.serenity.state.models.*

/** External-change conflict modal input (#1623) -- the "file changed on disk since it was opened" prompt's choice
  * cycling and submission. Mirrors [[ModalCloseWorkflowReducer]]'s shape, minus the actionStack/queued-buffers
  * machinery that only close workflows need: a reload conflict always targets exactly one buffer, carried directly in
  * [[Modal.ReloadConflict]] itself.
  */
private[reducers] object ModalReloadConflictReducer:
  import ModalEventReducer.{currentModal, dismissToPane, updateModal}

  def reduce(event: ModalInputEvent, currentState: AppState): ReducerResult =
    event match
      case ModalDismiss =>
        ReducerResult.noEffects(dismissToPane(currentState))
      case ModalNextField | ModalNavigate(Direction.Right) | ModalNavigate(Direction.Down) =>
        currentModal(currentState) match
          case Some((id, Modal.ReloadConflict(workflow))) =>
            ReducerResult.noEffects(updateModal(currentState, id, Modal.ReloadConflict(workflow.moveChoice(1))))
          case _ =>
            ReducerResult.noEffects(currentState)
      case ModalPreviousField | ModalNavigate(Direction.Left) | ModalNavigate(Direction.Up) =>
        currentModal(currentState) match
          case Some((id, Modal.ReloadConflict(workflow))) =>
            ReducerResult.noEffects(updateModal(currentState, id, Modal.ReloadConflict(workflow.moveChoice(-1))))
          case _ =>
            ReducerResult.noEffects(currentState)
      case ModalSubmit =>
        currentModal(currentState) match
          case Some((id, Modal.ReloadConflict(_))) =>
            ReducerResult.withEffect(currentState, AppEffect.Workflow(WorkflowEffect.SubmitReloadConflict(id)))
          case _ =>
            ReducerResult.noEffects(currentState)
      case ModalClick(_, Some(actionId)) =>
        currentModal(currentState) match
          case Some((id, Modal.ReloadConflict(workflow))) =>
            val choice = actionId match
              case "reload-conflict-reload"    => Some(ReloadConflictChoice.Reload)
              case "reload-conflict-overwrite" => Some(ReloadConflictChoice.Overwrite)
              case "reload-conflict-cancel"    => Some(ReloadConflictChoice.Cancel)
              case _                           => None
            ReducerResult.noEffects(
              choice.fold(currentState)(selected =>
                updateModal(currentState, id, Modal.ReloadConflict(workflow.copy(selectedChoice = selected)))
              )
            )
          case _ =>
            ReducerResult.noEffects(currentState)
      case ModalClick(_, _) =>
        ReducerResult.noEffects(currentState)
      case _ =>
        ReducerResult.noEffects(currentState)
