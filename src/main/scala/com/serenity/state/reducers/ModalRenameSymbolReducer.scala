package com.serenity.state.reducers

import com.serenity.keystroke.events.*
import com.serenity.lsp.LspEffect
import com.serenity.state.models.*
import com.serenity.text.TextEditing

/** Rename-symbol modal input (#1467) -- appending/deleting characters in the new-name field and, on submit, queuing the
  * `textDocument/rename` request against the invocation site captured when the prompt opened (see [[Modal.RenameSymbol]]
  * doc). Mirrors [[ModalGotoLineReducer]], except any non-empty free text is accepted rather than digits only, and
  * submitting enqueues an [[LspEffect]] instead of acting on state directly.
  */
private[reducers] object ModalRenameSymbolReducer:
  import ModalEventReducer.{currentModal, dismissToPane, updateModal}

  def reduce(event: ModalInputEvent, currentState: AppState): ReducerResult =
    event match
      case ModalDismiss => ReducerResult.noEffects(dismissToPane(currentState))
      case ModalInsertChar(char) =>
        currentModal(currentState) match
          case Some((id, modal: Modal.RenameSymbol)) =>
            ReducerResult.noEffects(updateModal(currentState, id, modal.copy(input = modal.input + char)))
          case _ => ReducerResult.noEffects(currentState)
      case ModalDeleteBackward =>
        currentModal(currentState) match
          case Some((id, modal: Modal.RenameSymbol)) if modal.input.nonEmpty =>
            ReducerResult.noEffects(updateModal(currentState, id, modal.copy(input = modal.input.dropRight(1))))
          case _ => ReducerResult.noEffects(currentState)
      case ModalDeleteForward =>
        ReducerResult.noEffects(currentState)
      case ModalDeleteWordBackward =>
        currentModal(currentState) match
          case Some((id, modal: Modal.RenameSymbol)) =>
            ReducerResult.noEffects(
              updateModal(currentState, id, modal.copy(input = TextEditing.deleteWordBackward(modal.input)))
            )
          case _ => ReducerResult.noEffects(currentState)
      case ModalDeleteWordForward =>
        currentModal(currentState) match
          case Some((id, modal: Modal.RenameSymbol)) =>
            ReducerResult.noEffects(
              updateModal(currentState, id, modal.copy(input = TextEditing.deleteWordForward(modal.input)))
            )
          case _ => ReducerResult.noEffects(currentState)
      case ModalSubmit =>
        currentModal(currentState) match
          case Some((_, modal: Modal.RenameSymbol)) if modal.input.nonEmpty =>
            ReducerResult(
              dismissToPane(currentState),
              List(
                AppEffect.LspQueue(
                  LspQueueEffect.Enqueue(
                    LspEffect.RenameRequested(
                      modal.uri,
                      modal.languageId,
                      modal.line,
                      modal.character,
                      modal.anchor,
                      modal.input
                    )
                  )
                )
              )
            )
          case _ =>
            ReducerResult.noEffects(dismissToPane(currentState))
      case ModalClick(_, _) =>
        ReducerResult.noEffects(currentState)
      case _ =>
        ReducerResult.noEffects(currentState)
