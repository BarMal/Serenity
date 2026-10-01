package com.serenity.state.reducers

import com.serenity.keystroke.events.*
import com.serenity.lsp.LspEffect
import com.serenity.state.models.*
import com.serenity.ui.widget.WidgetInput

/** Input for every [[Modal.TextPrompt]]: editing goes through the prompt's `TextField` (caret, word and grapheme
  * deletion), and submitting does what the prompt's purpose says -- jump to a line, name a session, rename a symbol.
  */
private[reducers] object ModalTextPromptReducer:
  import ModalEventReducer.{currentModal, dismissToPane, updateModal}

  def reduce(event: ModalInputEvent, currentState: AppState): ReducerResult =
    currentModal(currentState) match
      case Some((id, Modal.TextPrompt(prompt))) =>
        event match
          case ModalDismiss => ReducerResult.noEffects(dismissToPane(currentState))
          case ModalSubmit  => submitted(id, prompt, currentState)
          case other =>
            editing(other, prompt).fold(ReducerResult.noEffects(currentState)) { input =>
              val (field, _) = prompt.field.update(input)
              ReducerResult.noEffects(updateModal(currentState, id, Modal.TextPrompt(prompt.copy(field = field))))
            }
      case _ => ReducerResult.noEffects(currentState)

  private def editing(event: ModalInputEvent, prompt: TextPrompt): Option[WidgetInput] =
    event match
      case ModalInsertChar(char) if prompt.accepts(char) => Some(WidgetInput.Insert(char))
      case ModalDeleteBackward                           => Some(WidgetInput.DeleteBackward)
      case ModalDeleteForward                            => Some(WidgetInput.DeleteForward)
      case ModalDeleteWordBackward                       => Some(WidgetInput.DeleteWordBackward)
      case ModalDeleteWordForward                        => Some(WidgetInput.DeleteWordForward)
      case ModalNavigate(Direction.Left)                 => Some(WidgetInput.Left)
      case ModalNavigate(Direction.Right)                => Some(WidgetInput.Right)
      case _                                             => None

  private def submitted(id: SurfaceId, prompt: TextPrompt, state: AppState): ReducerResult =
    prompt.purpose match
      case TextPromptPurpose.GotoLine =>
        ReducerResult.noEffects(
          prompt.input.toIntOption.filter(_ > 0).fold(dismissToPane(state))(line => jumpToLine(state, line - 1))
        )
      case TextPromptPurpose.SessionName(_) if prompt.input.trim.nonEmpty =>
        ReducerResult.withEffect(state, AppEffect.Workflow(WorkflowEffect.SubmitSessionNamePrompt(id)))
      case TextPromptPurpose.RenameSymbol(site) if prompt.input.nonEmpty =>
        val rename =
          LspEffect.RenameRequested(site.uri, site.languageId, site.line, site.character, site.anchor, prompt.input)
        ReducerResult(dismissToPane(state), List(AppEffect.LspQueue(LspQueueEffect.Enqueue(rename))))
      case _ => ReducerResult.noEffects(dismissToPane(state))

  private def jumpToLine(state: AppState, targetLine: Int): AppState =
    state.persisted.layout.activeEditorPaneId.flatMap(paneId => Focused.bufferOf(state, paneId)) match
      case Some(buffer) =>
        val newTopLine = math.max(0, targetLine - buffer.viewport.visibleLines / 2)
        val updatedBuffer = buffer.copy(
          editing = EditingState(List(CursorPosition(targetLine, 0))),
          viewport = buffer.viewport.copy(topLine = newTopLine)
        )
        val dismissed = state.dismissTopModal
        dismissed.copy(
          persisted = dismissed.persisted.copy(buffers = dismissed.persisted.buffers + (buffer.id -> updatedBuffer))
        )
      case None => state.dismissTopModal
