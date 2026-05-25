package com.serenity.state.reducers

import com.googlecode.lanterna.input.KeyType
import com.serenity.keystroke.KeyStrokeInfo
import com.serenity.keystroke.events.*
import com.serenity.state.models.*

object ModalEventReducer:

  def reduce(modalType: ModalType, event: Event, currentState: AppState): ReducerResult =
    modalType match
      case ModalType.GotoLine => reduceGotoLine(event, currentState)
      case ModalType.Find     => reduceFind(event, currentState)
      case _                  => ReducerResult.noEffects(currentState)

  private def reduceGotoLine(event: Event, currentState: AppState): ReducerResult =
    event match
      case Escape => ReducerResult.noEffects(dismissToPane(currentState))
      case InsertChar(char) if char.isDigit =>
        currentState.modal match
          case Some(Modal.GotoLine(input)) =>
            ReducerResult.noEffects(currentState.copy(modal = Some(Modal.GotoLine(input + char))))
          case _ => ReducerResult.noEffects(currentState)
      case DeleteBackward =>
        currentState.modal match
          case Some(Modal.GotoLine(input)) if input.nonEmpty =>
            ReducerResult.noEffects(currentState.copy(modal = Some(Modal.GotoLine(input.dropRight(1)))))
          case _ => ReducerResult.noEffects(currentState)
      case Enter =>
        currentState.modal match
          case Some(Modal.GotoLine(input)) =>
            input.toIntOption match
              case Some(lineNumber) if lineNumber > 0 =>
                ReducerResult.noEffects(jumpToLine(currentState, lineNumber - 1))
              case _ =>
                ReducerResult.noEffects(dismissToPane(currentState))
          case _ =>
            ReducerResult.noEffects(dismissToPane(currentState))
      case UnhandledEvent(keyStroke, _) =>
        val keyInfo = KeyStrokeInfo.fromKeyStroke(keyStroke)
        if keyInfo.keyType == KeyType.Escape then ReducerResult.noEffects(dismissToPane(currentState))
        else ReducerResult.noEffects(currentState)
      case _ =>
        ReducerResult.noEffects(currentState)

  private def reduceFind(event: Event, currentState: AppState): ReducerResult =
    event match
      case Escape => ReducerResult.noEffects(dismissToPane(currentState))
      case InsertChar(char) =>
        currentState.modal match
          case Some(Modal.Find(query, results, idx)) =>
            ReducerResult.noEffects(currentState.copy(modal = Some(Modal.Find(query + char, results, idx))))
          case _ => ReducerResult.noEffects(currentState)
      case DeleteBackward =>
        currentState.modal match
          case Some(Modal.Find(query, results, idx)) if query.nonEmpty =>
            ReducerResult.noEffects(currentState.copy(modal = Some(Modal.Find(query.dropRight(1), results, idx))))
          case _ => ReducerResult.noEffects(currentState)
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
              val findState = FindState(query, resultLines, 0)
              ReducerResult.noEffects(applyFindResult(currentState, findState, resultLines.head))
            else
              ReducerResult.noEffects(dismissToPane(currentState))
          case _ =>
            ReducerResult.noEffects(dismissToPane(currentState))
      case _ =>
        ReducerResult.noEffects(currentState)

  private def dismissToPane(state: AppState): AppState =
    state.layout.activeEditorPaneId match
      case Some(paneId) => state.copy(modal = None, focus = Focus.EditorPane(paneId))
      case None         => state.copy(modal = None)

  private def jumpToLine(state: AppState, targetLine: Int): AppState =
    state.layout.activeEditorPaneId match
      case Some(paneId) =>
        state.layout.editorPanes.get(paneId) match
          case Some(pane) =>
            pane.bufferId.flatMap(state.buffers.get) match
              case Some(buffer) =>
                val halfVisible = buffer.viewport.visibleLines / 2
                val newTopLine  = math.max(0, targetLine - halfVisible)
                val updatedBuffer = buffer.copy(
                  cursors = List(CursorPosition(targetLine, 0)),
                  viewport = buffer.viewport.copy(topLine = newTopLine)
                )
                state.copy(
                  modal = None,
                  focus = Focus.EditorPane(paneId),
                  buffers = state.buffers + (buffer.id -> updatedBuffer)
                )
              case None =>
                state.copy(modal = None, focus = Focus.EditorPane(paneId))
          case None =>
            state.copy(modal = None, focus = Focus.EditorPane(PaneId(0)))
      case None =>
        state.copy(modal = None, focus = Focus.EditorPane(PaneId(0)))

  private def applyFindResult(state: AppState, findState: FindState, firstLine: Int): AppState =
    state.layout.activeEditorPaneId match
      case Some(paneId) =>
        state.layout.editorPanes.get(paneId) match
          case Some(pane) =>
            pane.bufferId.flatMap(state.buffers.get) match
              case Some(buffer) =>
                val halfVisible = buffer.viewport.visibleLines / 2
                val newTopLine  = math.max(0, firstLine - halfVisible)
                val updatedBuffer = buffer.copy(
                  cursors = List(CursorPosition(firstLine, 0)),
                  viewport = buffer.viewport.copy(topLine = newTopLine)
                )
                state.copy(
                  modal = None,
                  findState = Some(findState),
                  focus = Focus.EditorPane(paneId),
                  buffers = state.buffers + (buffer.id -> updatedBuffer)
                )
              case None =>
                state.copy(modal = None, findState = Some(findState))
          case None =>
            state.copy(modal = None)
      case None =>
        state.copy(modal = None)

