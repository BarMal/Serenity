package com.serenity.state.reducers

import com.serenity.keystroke.events.*
import com.serenity.rope.*
import com.serenity.state.models.*

/** Replace-workflow modal input -- editing the find/replace fields, cycling the action and scope, and previewing the
  * match count for the selected scope. Split out of `ModalEventReducer`'s per-modal-type dispatch when that file grew
  * past its 600-line target; `reduceReplaceWorkflow` itself was decomposed into one private helper per sub-event
  * branch to bring it under the method-length target.
  */
private[reducers] object ModalReplaceWorkflowReducer:
  import ModalEventReducer.{currentModal, dismissToPane, updateModal}

  def reduce(event: ModalInputEvent, currentState: AppState): ReducerResult =
    event match
      case ModalDismiss                    => ReducerResult.noEffects(dismissToPane(currentState))
      case ModalInsertChar(char)           => handleInsertChar(currentState, char)
      case ModalDeleteBackward             => handleDeleteBackward(currentState)
      case ModalDeleteForward              => handleDeleteForward(currentState)
      case ModalDeleteWordBackward         => handleDeleteWordBackward(currentState)
      case ModalDeleteWordForward          => handleDeleteWordForward(currentState)
      case ModalNextField                  => handleNextField(currentState)
      case ModalPreviousField              => handlePreviousField(currentState)
      case ModalNavigate(Direction.Left)   => handleNavigateLeft(currentState)
      case ModalNavigate(Direction.Right)  => handleNavigateRight(currentState)
      case ModalNavigate(Direction.Up)     => handleNavigateUp(currentState)
      case ModalNavigate(Direction.Down)   => handleNavigateDown(currentState)
      case ModalSubmit                     => handleSubmit(currentState)
      case ModalClick(focusId, actionId)   => handleClick(currentState, focusId, actionId)
      case _                               => ReducerResult.noEffects(currentState)

  /** The common shape shared by every field/scope/action-editing branch: apply `f` to the active workflow and refresh
    * its replace preview.
    */
  private def withWorkflowUpdate(currentState: AppState)(f: ReplaceWorkflowState => ReplaceWorkflowState): ReducerResult =
    currentModal(currentState) match
      case Some((id, Modal.ReplaceWorkflow(workflow))) =>
        ReducerResult.noEffects(updateReplaceWorkflow(currentState, id, f(workflow)))
      case _ =>
        ReducerResult.noEffects(currentState)

  private def handleInsertChar(currentState: AppState, char: Char): ReducerResult =
    withWorkflowUpdate(currentState)(_.appendToActiveField(char))

  private def handleDeleteBackward(currentState: AppState): ReducerResult =
    withWorkflowUpdate(currentState)(_.deleteFromActiveField)

  private def handleDeleteForward(currentState: AppState): ReducerResult =
    withWorkflowUpdate(currentState)(_.deleteForwardFromActiveField)

  private def handleDeleteWordBackward(currentState: AppState): ReducerResult =
    withWorkflowUpdate(currentState)(_.deleteWordBackwardFromActiveField)

  private def handleDeleteWordForward(currentState: AppState): ReducerResult =
    withWorkflowUpdate(currentState)(_.deleteWordForwardFromActiveField)

  private def handleNextField(currentState: AppState): ReducerResult =
    withWorkflowUpdate(currentState)(_.switchField(1))

  private def handlePreviousField(currentState: AppState): ReducerResult =
    withWorkflowUpdate(currentState)(_.switchField(-1))

  private def handleNavigateLeft(currentState: AppState): ReducerResult =
    withWorkflowUpdate(currentState)(_.moveAction(-1))

  private def handleNavigateRight(currentState: AppState): ReducerResult =
    withWorkflowUpdate(currentState)(_.moveAction(1))

  private def handleNavigateUp(currentState: AppState): ReducerResult =
    withWorkflowUpdate(currentState)(_.moveScope(-1))

  private def handleNavigateDown(currentState: AppState): ReducerResult =
    withWorkflowUpdate(currentState)(_.moveScope(1))

  private def handleSubmit(currentState: AppState): ReducerResult =
    currentModal(currentState) match
      case Some((id, Modal.ReplaceWorkflow(_))) =>
        ReducerResult.withEffect(currentState, AppEffect.Workflow(WorkflowEffect.SubmitReplaceWorkflow(id)))
      case _ =>
        ReducerResult.noEffects(currentState)

  private def handleClick(currentState: AppState, focusId: String, actionId: Option[String]): ReducerResult =
    currentModal(currentState) match
      case Some((id, Modal.ReplaceWorkflow(workflow))) =>
        val clicked = actionId match
          case Some("replace-next")      => Some(workflow.copy(selectedAction = ReplaceWorkflowAction.ReplaceNext))
          case Some("replace-all")       => Some(workflow.copy(selectedAction = ReplaceWorkflowAction.ReplaceAll))
          case Some("current-buffer")    => Some(workflow.copy(selectedScope = ReplaceWorkflowScope.CurrentBuffer))
          case Some("replace-selection") => Some(workflow.copy(selectedScope = ReplaceWorkflowScope.Selection))
          case _                         => None
        val field = focusId match
          case "find"    => Some(workflow.copy(activeField = ReplaceWorkflowField.Find))
          case "replace" => Some(workflow.copy(activeField = ReplaceWorkflowField.ReplaceWith))
          case _         => None
        ReducerResult.noEffects(
          updateReplaceWorkflow(currentState, id, clicked.orElse(field).getOrElse(workflow))
        )
      case _ =>
        ReducerResult.noEffects(currentState)

  private def updateReplaceWorkflow(
    state: AppState,
    id: SurfaceId,
    workflow: ReplaceWorkflowState
  ): AppState =
    updateModal(state, id, Modal.ReplaceWorkflow(withReplacePreview(state, workflow)))

  private def withReplacePreview(state: AppState, workflow: ReplaceWorkflowState): ReplaceWorkflowState =
    if workflow.findText.isEmpty then workflow.copy(statusMessage = None)
    else
      activeBuffer(state) match
        case None =>
          workflow.copy(statusMessage = Some("No active buffer"))
        case Some(buffer) =>
          replacePreviewRange(buffer, workflow) match
            case Left(message) =>
              workflow.copy(statusMessage = Some(message))
            case Right(range) =>
              val matchCount = scopedReplaceMatches(buffer, workflow.findText, range).length
              val scopeLabel = workflow.selectedScope match
                case ReplaceWorkflowScope.CurrentBuffer => "current buffer"
                case ReplaceWorkflowScope.Selection     => "selection"
              val countLabel =
                matchCount match
                  case 1     => "1 match"
                  case count => s"$count matches"
              workflow.copy(statusMessage = Some(s"$countLabel in $scopeLabel"))

  private def replacePreviewRange(
    buffer: Buffer,
    workflow: ReplaceWorkflowState
  ): Either[String, Option[(Int, Int)]] =
    workflow.selectedScope match
      case ReplaceWorkflowScope.CurrentBuffer =>
        Right(None)
      case ReplaceWorkflowScope.Selection =>
        buffer.primarySelection match
          case Some(selection) =>
            val startOffset = offsetForCursor(buffer.document.content, selection.start)
            val endOffset   = offsetForCursor(buffer.document.content, selection.end)
            Right(Some((math.min(startOffset, endOffset), math.max(startOffset, endOffset))))
          case None =>
            Left("Select text to preview selection matches")

  private def scopedReplaceMatches(
    buffer: Buffer,
    findText: String,
    range: Option[(Int, Int)]
  ): List[Int] =
    buffer.document.content.searchAll(findText).filter { offset =>
      val insideScope = range match
        case Some((startOffset, endOffset)) =>
          offset >= startOffset && (offset + findText.length) <= endOffset
        case None =>
          true
      insideScope && isWholeGraphemeMatch(buffer.document.content, offset, findText.length)
    }

  private def isWholeGraphemeMatch(content: Rope, offset: Int, length: Int): Boolean =
    content.isWholeGraphemeRange(offset, offset + length)

  private def activeBuffer(state: AppState): Option[Buffer] =
    Focused.bufferOf(state)

  private def offsetForCursor(content: Rope, cursor: CursorPosition): Int =
    content.lineColumnToOffset(cursor.line, cursor.column)
