package com.serenity.state.components

import com.googlecode.lanterna.input.KeyType
import com.serenity.keystroke.KeyStrokeInfo
import com.serenity.keystroke.events.*
import com.serenity.state.models.{AppState, CursorPosition, FindState, Focus, Modal, ModalType, PaneId}

class ModalComponent(
    modalType: ModalType
) extends FocusedComponent:

  def processEvent(event: Event, currentState: AppState): ComponentResult =
    modalType match
      case ModalType.CommandPalette =>
        processCommandPaletteEvent(event, currentState)
      case ModalType.FileSearch =>
        processFileSearchEvent(event, currentState)
      case ModalType.QuickOpen =>
        processQuickOpenEvent(event, currentState)
      case ModalType.GotoLine =>
        processGotoLineEvent(event, currentState)
      case ModalType.Find =>
        processFindEvent(event, currentState)
      case ModalType.Custom(name) =>
        processCustomModalEvent(name, event, currentState)

  private def processCommandPaletteEvent(event: Event, currentState: AppState): ComponentResult =
    event match
      case textEvent: TextEntryEvent => processModalTextEvent(textEvent, currentState)
      case UnhandledEvent(keyStroke, _) =>
        val keyInfo = KeyStrokeInfo.fromKeyStroke(keyStroke)
        processModalKeyStroke(keyInfo, currentState)
      case _ => ComponentResult.noChange

  private def processFileSearchEvent(event: Event, currentState: AppState): ComponentResult =
    event match
      case textEvent: TextEntryEvent => processModalTextEvent(textEvent, currentState)
      case UnhandledEvent(keyStroke, _) =>
        val keyInfo = KeyStrokeInfo.fromKeyStroke(keyStroke)
        processModalKeyStroke(keyInfo, currentState)
      case _ => ComponentResult.noChange

  private def processQuickOpenEvent(event: Event, currentState: AppState): ComponentResult =
    event match
      case textEvent: TextEntryEvent => processModalTextEvent(textEvent, currentState)
      case UnhandledEvent(keyStroke, _) =>
        val keyInfo = KeyStrokeInfo.fromKeyStroke(keyStroke)
        processModalKeyStroke(keyInfo, currentState)
      case _ => ComponentResult.noChange

  private def processCustomModalEvent(name: String, event: Event, currentState: AppState): ComponentResult =
    event match
      case textEvent: TextEntryEvent => processModalTextEvent(textEvent, currentState)
      case UnhandledEvent(keyStroke, _) =>
        val keyInfo = KeyStrokeInfo.fromKeyStroke(keyStroke)
        processModalKeyStroke(keyInfo, currentState)
      case _ => ComponentResult.noChange

  private def processGotoLineEvent(event: Event, currentState: AppState): ComponentResult =
    event match
      case Escape => ComponentResult.dismiss
      case InsertChar(char) if char.isDigit =>
        currentState.modal match
          case Some(Modal.GotoLine(input)) =>
            ComponentResult.updateState(_.copy(modal = Some(Modal.GotoLine(input + char))))
          case _ => ComponentResult.noChange
      case DeleteBackward =>
        currentState.modal match
          case Some(Modal.GotoLine(input)) if input.nonEmpty =>
            ComponentResult.updateState(_.copy(modal = Some(Modal.GotoLine(input.dropRight(1)))))
          case _ => ComponentResult.noChange
      case Enter =>
        currentState.modal match
          case Some(Modal.GotoLine(input)) =>
            input.toIntOption match
              case Some(lineNumber) if lineNumber > 0 =>
                val targetLine = lineNumber - 1
                ComponentResult.updateState { state =>
                  state.layout.activeEditorPaneId match
                    case Some(paneId) =>
                      state.layout.editorPanes.get(paneId) match
                        case Some(pane) =>
                          val halfVisible = pane.viewport.visibleLines / 2
                          val newTopLine  = math.max(0, targetLine - halfVisible)
                          val updatedPane = pane.copy(
                            cursors = List(CursorPosition(targetLine, 0)),
                            viewport = pane.viewport.copy(topLine = newTopLine)
                          )
                          state.copy(
                            modal = None,
                            focus = Focus.EditorPane(paneId),
                            layout = state.layout.copy(
                              editorPanes = state.layout.editorPanes + (paneId -> updatedPane)
                            )
                          )
                        case None => state.copy(modal = None, focus = Focus.EditorPane(PaneId(0)))
                    case None => state.copy(modal = None, focus = Focus.EditorPane(PaneId(0)))
                }
              case _ => ComponentResult.dismiss
          case _ => ComponentResult.dismiss
      case UnhandledEvent(keyStroke, _) =>
        val keyInfo = KeyStrokeInfo.fromKeyStroke(keyStroke)
        if keyInfo.keyType == KeyType.Escape then ComponentResult.dismiss
        else ComponentResult.noChange
      case _ => ComponentResult.noChange

  private def processFindEvent(event: Event, currentState: AppState): ComponentResult =
    event match
      case Escape => ComponentResult.dismiss
      case InsertChar(char) =>
        currentState.modal match
          case Some(Modal.Find(query, results, idx)) =>
            ComponentResult.updateState(_.copy(modal = Some(Modal.Find(query + char, results, idx))))
          case _ => ComponentResult.noChange
      case DeleteBackward =>
        currentState.modal match
          case Some(Modal.Find(query, results, idx)) if query.nonEmpty =>
            ComponentResult.updateState(_.copy(modal = Some(Modal.Find(query.dropRight(1), results, idx))))
          case _ => ComponentResult.noChange
      case Enter =>
        currentState.modal match
          case Some(Modal.Find(query, _, _)) if query.nonEmpty =>
            val resultLines = currentState.layout.activeEditorPaneId.toList.flatMap { paneId =>
              currentState.layout.editorPanes.get(paneId).toList.flatMap { pane =>
                pane.bufferId.toList.flatMap { bufferId =>
                  currentState.buffers.get(bufferId).toList.flatMap { buffer =>
                    val lines = buffer.content.collect().split('\n')
                    lines.zipWithIndex.collect {
                      case (line, idx) if line.contains(query) => idx
                    }.toList
                  }
                }
              }
            }
            if resultLines.nonEmpty then
              val firstLine = resultLines.head
              ComponentResult.updateState { state =>
                val findState = FindState(query, resultLines, 0)
                state.layout.activeEditorPaneId match
                  case Some(paneId) =>
                    state.layout.editorPanes.get(paneId) match
                      case Some(pane) =>
                        val halfVisible = pane.viewport.visibleLines / 2
                        val newTopLine  = math.max(0, firstLine - halfVisible)
                        val updatedPane = pane.copy(
                          cursors = List(CursorPosition(firstLine, 0)),
                          viewport = pane.viewport.copy(topLine = newTopLine)
                        )
                        state.copy(
                          modal = None,
                          findState = Some(findState),
                          focus = Focus.EditorPane(paneId),
                          layout = state.layout.copy(
                            editorPanes = state.layout.editorPanes + (paneId -> updatedPane)
                          )
                        )
                      case None => state.copy(modal = None)
                  case None => state.copy(modal = None)
              }
            else ComponentResult.dismiss
          case _ => ComponentResult.dismiss
      case _ => ComponentResult.noChange

  private def processModalTextEvent(event: TextEntryEvent, currentState: AppState): ComponentResult =
    event match
      case InsertChar(_) =>
        // TODO: Handle text input for search/command input
        ComponentResult.noChange
      case DeleteBackward =>
        // TODO: Handle backspace in search/command input
        ComponentResult.noChange
      case MoveUp | MoveDown =>
        // TODO: Handle navigation in suggestions/results
        ComponentResult.noChange
      case _ => ComponentResult.noChange

  private def processModalKeyStroke(keyInfo: KeyStrokeInfo, currentState: AppState): ComponentResult =
    keyInfo.keyType match
      case KeyType.Escape => ComponentResult.dismiss
      case KeyType.Enter  =>
        // TODO: Execute selected command/action
        ComponentResult.dismiss
      case _ => ComponentResult.noChange
