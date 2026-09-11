package com.serenity.state.reducers

import com.serenity.keystroke.events.*
import com.serenity.state.models.*

/** File-workflow modal input (Open/SaveAs) -- editing the active field, cycling suggestions and save formats, and
  * submitting or creating missing directories. Split out of `ModalEventReducer`'s per-modal-type dispatch when that
  * file grew past its 600-line target; `reduceFileWorkflow` itself was decomposed into one private helper per
  * sub-event branch to bring it under the method-length target.
  */
private[reducers] object ModalFileWorkflowReducer:
  import ModalEventReducer.{currentModal, dismissToPane, updateModal}

  def reduce(event: ModalInputEvent, currentState: AppState): ReducerResult =
    event match
      case ModalDismiss                  => ReducerResult.noEffects(dismissToPane(currentState))
      case ModalInsertChar(char)         => handleInsertChar(currentState, char)
      case ModalDeleteBackward           => handleDeleteBackward(currentState)
      case ModalDeleteForward            => handleDeleteForward(currentState)
      case ModalDeleteWordBackward       => handleDeleteWordBackward(currentState)
      case ModalDeleteWordForward        => handleDeleteWordForward(currentState)
      case ModalNextField                => handleNextField(currentState)
      case ModalPreviousField            => handlePreviousField(currentState)
      case ModalNavigate(Direction.Up)   => handleNavigateUp(currentState)
      case ModalNavigate(Direction.Down) => handleNavigateDown(currentState)
      case ModalSubmit                   => handleSubmit(currentState)
      case ModalCreateDirectory          => handleCreateDirectory(currentState)
      case ModalClick(focusId, actionId) => handleClick(currentState, focusId, actionId)
      case _                             => ReducerResult.noEffects(currentState)

  /** The common shape shared by every field-editing branch: apply `f` to the active workflow and request a refresh. */
  private def withFieldEdit(currentState: AppState)(f: FileWorkflowState => FileWorkflowState): ReducerResult =
    currentModal(currentState) match
      case Some((id, Modal.FileWorkflow(workflow))) =>
        ReducerResult.withEffect(
          updateModal(currentState, id, Modal.FileWorkflow(f(workflow))),
          AppEffect.Workflow(WorkflowEffect.RefreshFileWorkflow(id))
        )
      case _ =>
        ReducerResult.noEffects(currentState)

  private def handleInsertChar(currentState: AppState, char: Char): ReducerResult =
    withFieldEdit(currentState)(_.appendToActiveField(char))

  private def handleDeleteBackward(currentState: AppState): ReducerResult =
    withFieldEdit(currentState)(_.deleteFromActiveField)

  private def handleDeleteForward(currentState: AppState): ReducerResult =
    withFieldEdit(currentState)(_.deleteForwardFromActiveField)

  private def handleDeleteWordBackward(currentState: AppState): ReducerResult =
    withFieldEdit(currentState)(_.deleteWordBackwardFromActiveField)

  private def handleDeleteWordForward(currentState: AppState): ReducerResult =
    withFieldEdit(currentState)(_.deleteWordForwardFromActiveField)

  private def handlePreviousField(currentState: AppState): ReducerResult =
    withFieldEdit(currentState)(_.switchField(-1))

  private def handleNextField(currentState: AppState): ReducerResult =
    currentModal(currentState) match
      case Some((id, Modal.FileWorkflow(workflow))) if workflow.suggestions.nonEmpty =>
        val effect =
          if workflow.acceptedSuggestionOpensFile then WorkflowEffect.SubmitFileWorkflow(id)
          else WorkflowEffect.RefreshFileWorkflow(id)
        val updated = updateModal(currentState, id, Modal.FileWorkflow(workflow.applySelectedSuggestion))
        ReducerResult.withEffect(updated, AppEffect.Workflow(effect))
      case Some((id, Modal.FileWorkflow(workflow))) =>
        ReducerResult.withEffect(
          updateModal(currentState, id, Modal.FileWorkflow(workflow.switchField(1))),
          AppEffect.Workflow(WorkflowEffect.RefreshFileWorkflow(id))
        )
      case _ =>
        ReducerResult.noEffects(currentState)

  private def handleNavigateUp(currentState: AppState): ReducerResult =
    currentModal(currentState) match
      case Some((id, Modal.FileWorkflow(workflow: SaveAsFileWorkflowState)))
          if workflow.activeField == FileWorkflowField.Format =>
        ReducerResult.noEffects(updateModal(currentState, id, Modal.FileWorkflow(workflow.cycleFormat(-1))))
      case Some((id, Modal.FileWorkflow(workflow))) =>
        ReducerResult.noEffects(updateModal(currentState, id, Modal.FileWorkflow(workflow.moveSuggestion(-1))))
      case _ =>
        ReducerResult.noEffects(currentState)

  private def handleNavigateDown(currentState: AppState): ReducerResult =
    currentModal(currentState) match
      case Some((id, Modal.FileWorkflow(workflow: SaveAsFileWorkflowState)))
          if workflow.activeField == FileWorkflowField.Format =>
        ReducerResult.noEffects(updateModal(currentState, id, Modal.FileWorkflow(workflow.cycleFormat(1))))
      case Some((id, Modal.FileWorkflow(workflow))) =>
        ReducerResult.noEffects(updateModal(currentState, id, Modal.FileWorkflow(workflow.moveSuggestion(1))))
      case _ =>
        ReducerResult.noEffects(currentState)

  private def handleSubmit(currentState: AppState): ReducerResult =
    currentModal(currentState) match
      case Some((id, Modal.FileWorkflow(_))) =>
        ReducerResult.withEffect(currentState, AppEffect.Workflow(WorkflowEffect.SubmitFileWorkflow(id)))
      case _ =>
        ReducerResult.noEffects(currentState)

  private def handleCreateDirectory(currentState: AppState): ReducerResult =
    currentModal(currentState) match
      case Some((id, Modal.FileWorkflow(workflow)))
          if workflow.mode == FileWorkflowMode.SaveAs && workflow.missingPathSegments.nonEmpty =>
        ReducerResult.withEffect(
          currentState,
          AppEffect.Workflow(WorkflowEffect.CreateFileWorkflowDirectories(id))
        )
      case _ =>
        ReducerResult.noEffects(currentState)

  private def handleClick(currentState: AppState, focusId: String, actionId: Option[String]): ReducerResult =
    currentModal(currentState) match
      case Some((id, Modal.FileWorkflow(workflow))) =>
        val updated = actionId match
          case Some(suggestionId) if suggestionId.startsWith("file-suggestion-") =>
            suggestionId
              .stripPrefix("file-suggestion-")
              .toIntOption
              .filter(index => index >= 0 && index < workflow.suggestions.length)
              .map(index => workflow.updated(selectedSuggestionIndex = index, statusMessage = None))
          case _ => None
        val fieldUpdated = focusId match
          case "filename" => Some(workflow.updated(activeField = FileWorkflowField.Filename, statusMessage = None))
          case "path"     => Some(workflow.updated(activeField = FileWorkflowField.Path, statusMessage = None))
          case _          => None
        val nextState =
          updateModal(currentState, id, Modal.FileWorkflow(updated.orElse(fieldUpdated).getOrElse(workflow)))
        if fieldUpdated.nonEmpty then
          ReducerResult.withEffect(nextState, AppEffect.Workflow(WorkflowEffect.RefreshFileWorkflow(id)))
        else ReducerResult.noEffects(nextState)
      case _ =>
        ReducerResult.noEffects(currentState)
