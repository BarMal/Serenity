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
      case ModalType.FileWorkflow => reduceFileWorkflow(event, currentState)
      case ModalType.ReplaceWorkflow => reduceReplaceWorkflow(event, currentState)
      case ModalType.CloseWorkflow => reduceCloseWorkflow(event, currentState)
      case ModalType.Custom(_) => ReducerResult.noEffects(currentState)

  private def reduceGotoLine(event: Event, currentState: AppState): ReducerResult =
    event match
      case Escape => ReducerResult.noEffects(dismissToPane(currentState))
      case InsertChar(char) if char.isDigit =>
        currentModal(currentState) match
          case Some((surface, Modal.GotoLine(input))) =>
            ReducerResult.noEffects(updateModal(currentState, surface, Modal.GotoLine(input + char)))
          case _ => ReducerResult.noEffects(currentState)
      case DeleteBackward =>
        currentModal(currentState) match
          case Some((surface, Modal.GotoLine(input))) if input.nonEmpty =>
            ReducerResult.noEffects(updateModal(currentState, surface, Modal.GotoLine(input.dropRight(1))))
          case _ => ReducerResult.noEffects(currentState)
      case Enter =>
        currentModal(currentState) match
          case Some((_, Modal.GotoLine(input))) =>
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
        currentModal(currentState) match
          case Some((surface, Modal.Find(query, results, idx))) =>
            ReducerResult.noEffects(updateModal(currentState, surface, Modal.Find(query + char, results, idx)))
          case _ => ReducerResult.noEffects(currentState)
      case DeleteBackward =>
        currentModal(currentState) match
          case Some((surface, Modal.Find(query, results, idx))) if query.nonEmpty =>
            ReducerResult.noEffects(updateModal(currentState, surface, Modal.Find(query.dropRight(1), results, idx)))
          case _ => ReducerResult.noEffects(currentState)
      case Enter =>
        currentModal(currentState) match
          case Some((_, Modal.Find(query, _, _))) if query.nonEmpty =>
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

  private def reduceFileWorkflow(event: Event, currentState: AppState): ReducerResult =
    event match
      case Escape =>
        ReducerResult.noEffects(dismissToPane(currentState))
      case InsertChar(char) =>
        currentModal(currentState) match
          case Some((surface, Modal.FileWorkflow(workflow))) =>
            ReducerResult.withEffect(
              updateModal(currentState, surface, Modal.FileWorkflow(workflow.appendToActiveField(char))),
              AppEffect.RefreshFileWorkflow(surface.id)
            )
          case _ =>
            ReducerResult.noEffects(currentState)
      case DeleteBackward =>
        currentModal(currentState) match
          case Some((surface, Modal.FileWorkflow(workflow))) =>
            ReducerResult.withEffect(
              updateModal(currentState, surface, Modal.FileWorkflow(workflow.deleteFromActiveField)),
              AppEffect.RefreshFileWorkflow(surface.id)
            )
          case _ =>
            ReducerResult.noEffects(currentState)
      case TabKey =>
        currentModal(currentState) match
          case Some((surface, Modal.FileWorkflow(workflow))) =>
            ReducerResult.withEffect(
              updateModal(currentState, surface, Modal.FileWorkflow(workflow.switchField(1))),
              AppEffect.RefreshFileWorkflow(surface.id)
            )
          case _ =>
            ReducerResult.noEffects(currentState)
      case ReverseTabKey =>
        currentModal(currentState) match
          case Some((surface, Modal.FileWorkflow(workflow))) =>
            ReducerResult.withEffect(
              updateModal(currentState, surface, Modal.FileWorkflow(workflow.switchField(-1))),
              AppEffect.RefreshFileWorkflow(surface.id)
            )
          case _ =>
            ReducerResult.noEffects(currentState)
      case MoveUp =>
        currentModal(currentState) match
          case Some((surface, Modal.FileWorkflow(workflow))) =>
            ReducerResult.noEffects(updateModal(currentState, surface, Modal.FileWorkflow(workflow.moveSuggestion(-1))))
          case _ =>
            ReducerResult.noEffects(currentState)
      case MoveDown =>
        currentModal(currentState) match
          case Some((surface, Modal.FileWorkflow(workflow))) =>
            ReducerResult.noEffects(updateModal(currentState, surface, Modal.FileWorkflow(workflow.moveSuggestion(1))))
          case _ =>
            ReducerResult.noEffects(currentState)
      case Enter =>
        currentModal(currentState) match
          case Some((surface, Modal.FileWorkflow(workflow))) if workflow.suggestions.nonEmpty && workflow.activeField == FileWorkflowField.Path =>
            ReducerResult.withEffect(
              updateModal(currentState, surface, Modal.FileWorkflow(workflow.applySelectedSuggestion)),
              AppEffect.RefreshFileWorkflow(surface.id)
            )
          case Some((surface, Modal.FileWorkflow(_))) =>
            ReducerResult.withEffect(currentState, AppEffect.SubmitFileWorkflow(surface.id))
          case _ =>
            ReducerResult.noEffects(currentState)
      case _ =>
        ReducerResult.noEffects(currentState)

  private def reduceCloseWorkflow(event: Event, currentState: AppState): ReducerResult =
    event match
      case Escape =>
        ReducerResult.noEffects(cancelCloseWorkflow(currentState))
      case TabKey | MoveRight =>
        currentModal(currentState) match
          case Some((surface, Modal.CloseWorkflow(workflow))) =>
            ReducerResult.noEffects(updateModal(currentState, surface, Modal.CloseWorkflow(workflow.moveChoice(1))))
          case _ =>
            ReducerResult.noEffects(currentState)
      case ReverseTabKey | MoveLeft =>
        currentModal(currentState) match
          case Some((surface, Modal.CloseWorkflow(workflow))) =>
            ReducerResult.noEffects(updateModal(currentState, surface, Modal.CloseWorkflow(workflow.moveChoice(-1))))
          case _ =>
            ReducerResult.noEffects(currentState)
      case Enter =>
        currentModal(currentState) match
          case Some((surface, Modal.CloseWorkflow(_))) =>
            ReducerResult.withEffect(currentState, AppEffect.SubmitCloseWorkflow(surface.id))
          case _ =>
            ReducerResult.noEffects(currentState)
      case _ =>
        ReducerResult.noEffects(currentState)

  private def reduceReplaceWorkflow(event: Event, currentState: AppState): ReducerResult =
    event match
      case Escape =>
        ReducerResult.noEffects(dismissToPane(currentState))
      case InsertChar(char) =>
        currentModal(currentState) match
          case Some((surface, Modal.ReplaceWorkflow(workflow))) =>
            ReducerResult.noEffects(
              updateModal(currentState, surface, Modal.ReplaceWorkflow(workflow.appendToActiveField(char)))
            )
          case _ =>
            ReducerResult.noEffects(currentState)
      case DeleteBackward =>
        currentModal(currentState) match
          case Some((surface, Modal.ReplaceWorkflow(workflow))) =>
            ReducerResult.noEffects(
              updateModal(currentState, surface, Modal.ReplaceWorkflow(workflow.deleteFromActiveField))
            )
          case _ =>
            ReducerResult.noEffects(currentState)
      case TabKey =>
        currentModal(currentState) match
          case Some((surface, Modal.ReplaceWorkflow(workflow))) =>
            ReducerResult.noEffects(
              updateModal(currentState, surface, Modal.ReplaceWorkflow(workflow.switchField(1)))
            )
          case _ =>
            ReducerResult.noEffects(currentState)
      case ReverseTabKey =>
        currentModal(currentState) match
          case Some((surface, Modal.ReplaceWorkflow(workflow))) =>
            ReducerResult.noEffects(
              updateModal(currentState, surface, Modal.ReplaceWorkflow(workflow.switchField(-1)))
            )
          case _ =>
            ReducerResult.noEffects(currentState)
      case Enter =>
        currentModal(currentState) match
          case Some((surface, Modal.ReplaceWorkflow(_))) =>
            ReducerResult.withEffect(currentState, AppEffect.SubmitReplaceWorkflow(surface.id))
          case _ =>
            ReducerResult.noEffects(currentState)
      case _ =>
        ReducerResult.noEffects(currentState)

  private def dismissToPane(state: AppState): AppState =
    state.layout.activeEditorPaneId match
      case Some(paneId) => state.copy(uiSurfaces = state.uiSurfaces.filterNot(isModalSurface), focus = Focus.EditorPane(paneId))
      case None         => state.copy(uiSurfaces = state.uiSurfaces.filterNot(isModalSurface))

  private def cancelCloseWorkflow(state: AppState): AppState =
    dismissToPane(
      state.copy(actionStack = state.actionStack.filter {
        case AppAction.CloseWorkflow(_) => false
      })
    )

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
                  uiSurfaces = state.uiSurfaces.filterNot(isModalSurface),
                  focus = Focus.EditorPane(paneId),
                  buffers = state.buffers + (buffer.id -> updatedBuffer)
                )
              case None =>
                state.copy(uiSurfaces = state.uiSurfaces.filterNot(isModalSurface), focus = Focus.EditorPane(paneId))
          case None =>
            state.copy(uiSurfaces = state.uiSurfaces.filterNot(isModalSurface), focus = Focus.EditorPane(PaneId(0)))
      case None =>
        state.copy(uiSurfaces = state.uiSurfaces.filterNot(isModalSurface), focus = Focus.EditorPane(PaneId(0)))

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
                  uiSurfaces = state.uiSurfaces.filterNot(isModalSurface),
                  findState = Some(findState),
                  focus = Focus.EditorPane(paneId),
                  buffers = state.buffers + (buffer.id -> updatedBuffer)
                )
              case None =>
                state.copy(uiSurfaces = state.uiSurfaces.filterNot(isModalSurface), findState = Some(findState))
          case None =>
            state.copy(uiSurfaces = state.uiSurfaces.filterNot(isModalSurface))
      case None =>
        state.copy(uiSurfaces = state.uiSurfaces.filterNot(isModalSurface))

  private def currentModal(state: AppState): Option[(UiSurface, Modal)] =
    state.modalSurface.flatMap { surface =>
      surface.content match
        case SurfaceContent.ModalWorkflow(modal) => Some((surface, modal))
        case _                                   => None
    }

  private def updateModal(state: AppState, surface: UiSurface, modal: Modal): AppState =
    val updatedSurface = surface.copy(content = SurfaceContent.ModalWorkflow(modal))
    state.copy(uiSurfaces = state.uiSurfaces.filterNot(_.id == surface.id) :+ updatedSurface)

  private def isModalSurface(surface: UiSurface): Boolean =
    surface.content match
      case SurfaceContent.ModalWorkflow(_) => true
      case _                               => false
