package com.serenity.state.reducers

import com.serenity.keystroke.events.*
import com.serenity.state.models.*
import com.serenity.text.TextEditing

/** Goto-line modal input -- appending/deleting digits in the line-number field and jumping to the submitted line.
  * Split out of `ModalEventReducer`'s per-modal-type dispatch when that file grew past its 600-line target.
  */
private[reducers] object ModalGotoLineReducer:
  import ModalEventReducer.{currentModal, dismissToPane, updateModal}

  def reduce(event: ModalInputEvent, currentState: AppState): ReducerResult =
    event match
      case ModalDismiss => ReducerResult.noEffects(dismissToPane(currentState))
      case ModalInsertChar(char) if char.isDigit =>
        currentModal(currentState) match
          case Some((id, Modal.GotoLine(input))) =>
            ReducerResult.noEffects(updateModal(currentState, id, Modal.GotoLine(input + char)))
          case _ => ReducerResult.noEffects(currentState)
      case ModalDeleteBackward =>
        currentModal(currentState) match
          case Some((id, Modal.GotoLine(input))) if input.nonEmpty =>
            ReducerResult.noEffects(updateModal(currentState, id, Modal.GotoLine(input.dropRight(1))))
          case _ => ReducerResult.noEffects(currentState)
      case ModalDeleteForward =>
        ReducerResult.noEffects(currentState)
      case ModalDeleteWordBackward =>
        currentModal(currentState) match
          case Some((id, Modal.GotoLine(input))) =>
            ReducerResult.noEffects(
              updateModal(currentState, id, Modal.GotoLine(TextEditing.deleteWordBackward(input)))
            )
          case _ => ReducerResult.noEffects(currentState)
      case ModalDeleteWordForward =>
        currentModal(currentState) match
          case Some((id, Modal.GotoLine(input))) =>
            ReducerResult.noEffects(
              updateModal(currentState, id, Modal.GotoLine(TextEditing.deleteWordForward(input)))
            )
          case _ => ReducerResult.noEffects(currentState)
      case ModalSubmit =>
        currentModal(currentState) match
          case Some((_, Modal.GotoLine(input))) =>
            input.toIntOption match
              case Some(lineNumber) if lineNumber > 0 =>
                ReducerResult.noEffects(jumpToLine(currentState, lineNumber - 1))
              case _ =>
                ReducerResult.noEffects(dismissToPane(currentState))
          case _ =>
            ReducerResult.noEffects(dismissToPane(currentState))
      case ModalClick(_, _) =>
        ReducerResult.noEffects(currentState)
      case _ =>
        ReducerResult.noEffects(currentState)

  private def jumpToLine(state: AppState, targetLine: Int): AppState =
    state.persisted.layout.activeEditorPaneId.flatMap(paneId => Focused.bufferOf(state, paneId)) match
      case Some(buffer) =>
        val halfVisible = buffer.viewport.visibleLines / 2
        val newTopLine  = math.max(0, targetLine - halfVisible)
        val updatedBuffer = buffer.copy(
          editing = buffer.editing.copy(cursors = List(CursorPosition(targetLine, 0))),
          viewport = buffer.viewport.copy(topLine = newTopLine)
        )
        val dismissed = state.dismissTopModal
        dismissed.copy(
          persisted = dismissed.persisted.copy(buffers = dismissed.persisted.buffers + (buffer.id -> updatedBuffer))
        )
      case None =>
        state.dismissTopModal
