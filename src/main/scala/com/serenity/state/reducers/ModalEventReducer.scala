package com.serenity.state.reducers

import com.serenity.keystroke.events.*
import com.serenity.rope.Rope
import com.serenity.state.models.*
import com.serenity.text.TextEditing

object ModalEventReducer:

  def reducer(modalType: ModalType): Reducer[ModalInputEvent] =
    Reducer.instance((event, state) => reduce(modalType, event, state))

  def reduce(modalType: ModalType, event: Event, currentState: AppState): ReducerResult =
    ModalInputEvent
      .fromEvent(event)
      .map(reduce(modalType, _, currentState))
      .getOrElse(ReducerResult.noEffects(currentState))

  def reduce(modalType: ModalType, event: ModalInputEvent, currentState: AppState): ReducerResult =
    modalType match
      case ModalType.GotoLine        => reduceGotoLine(event, currentState)
      case ModalType.Find            => reduceFind(event, currentState)
      case ModalType.FileWorkflow    => reduceFileWorkflow(event, currentState)
      case ModalType.ReplaceWorkflow => reduceReplaceWorkflow(event, currentState)
      case ModalType.CloseWorkflow   => reduceCloseWorkflow(event, currentState)
      case ModalType.Custom(_)       => ReducerResult.noEffects(currentState)

  private def reduceGotoLine(event: ModalInputEvent, currentState: AppState): ReducerResult =
    event match
      case ModalDismiss => ReducerResult.noEffects(dismissToPane(currentState))
      case ModalInsertChar(char) if char.isDigit =>
        currentModal(currentState) match
          case Some((surface, Modal.GotoLine(input))) =>
            ReducerResult.noEffects(updateModal(currentState, surface, Modal.GotoLine(input + char)))
          case _ => ReducerResult.noEffects(currentState)
      case ModalDeleteBackward =>
        currentModal(currentState) match
          case Some((surface, Modal.GotoLine(input))) if input.nonEmpty =>
            ReducerResult.noEffects(updateModal(currentState, surface, Modal.GotoLine(input.dropRight(1))))
          case _ => ReducerResult.noEffects(currentState)
      case ModalDeleteForward =>
        ReducerResult.noEffects(currentState)
      case ModalDeleteWordBackward =>
        currentModal(currentState) match
          case Some((surface, Modal.GotoLine(input))) =>
            ReducerResult.noEffects(
              updateModal(currentState, surface, Modal.GotoLine(TextEditing.deleteWordBackward(input)))
            )
          case _ => ReducerResult.noEffects(currentState)
      case ModalDeleteWordForward =>
        currentModal(currentState) match
          case Some((surface, Modal.GotoLine(input))) =>
            ReducerResult.noEffects(
              updateModal(currentState, surface, Modal.GotoLine(TextEditing.deleteWordForward(input)))
            )
          case _ => ReducerResult.noEffects(currentState)
      case ModalSubmit =>
        currentModal(currentState) match
          case Some((_, Modal.GotoLine(input))) =>
            input.toIntOption match
              case Some(lineNumber) =>
                ReducerResult.noEffects(jumpToLine(currentState, lineNumber - 1))
              case _ =>
                ReducerResult.noEffects(dismissToPane(currentState))
          case _ =>
            ReducerResult.noEffects(dismissToPane(currentState))
      case _ =>
        ReducerResult.noEffects(currentState)

  private def reduceFind(event: ModalInputEvent, currentState: AppState): ReducerResult =
    event match
      case ModalDismiss => ReducerResult.noEffects(dismissToPane(currentState))
      case ModalInsertChar(char) =>
        currentModal(currentState) match
          case Some((surface, Modal.Find(query, _, _))) =>
            ReducerResult.noEffects(updateFindQuery(currentState, surface, query + char))
          case _ => ReducerResult.noEffects(currentState)
      case ModalDeleteBackward =>
        currentModal(currentState) match
          case Some((surface, Modal.Find(query, _, _))) if query.nonEmpty =>
            ReducerResult.noEffects(updateFindQuery(currentState, surface, query.dropRight(1)))
          case _ => ReducerResult.noEffects(currentState)
      case ModalDeleteForward =>
        ReducerResult.noEffects(currentState)
      case ModalDeleteWordBackward =>
        currentModal(currentState) match
          case Some((surface, Modal.Find(query, _, _))) =>
            ReducerResult.noEffects(updateFindQuery(currentState, surface, TextEditing.deleteWordBackward(query)))
          case _ => ReducerResult.noEffects(currentState)
      case ModalDeleteWordForward =>
        currentModal(currentState) match
          case Some((surface, Modal.Find(query, _, _))) =>
            ReducerResult.noEffects(updateFindQuery(currentState, surface, TextEditing.deleteWordForward(query)))
          case _ => ReducerResult.noEffects(currentState)
      case ModalFindNext | ModalNavigate(Direction.Down) | ModalNavigate(Direction.Right) =>
        currentModal(currentState) match
          case Some((surface, Modal.Find(query, _, currentIndex))) if query.nonEmpty =>
            ReducerResult.noEffects(updateFindSelection(currentState, surface, query, currentIndex + 1))
          case _ => ReducerResult.noEffects(currentState)
      case ModalNavigate(Direction.Up) | ModalNavigate(Direction.Left) =>
        currentModal(currentState) match
          case Some((surface, Modal.Find(query, _, currentIndex))) if query.nonEmpty =>
            ReducerResult.noEffects(updateFindSelection(currentState, surface, query, currentIndex - 1))
          case _ => ReducerResult.noEffects(currentState)
      case ModalSubmit =>
        currentModal(currentState) match
          case Some((surface, Modal.Find(query, results, currentIndex))) if query.nonEmpty =>
            val nextIndex =
              if results.nonEmpty then currentIndex + 1
              else 0
            ReducerResult.noEffects(updateFindSelection(currentState, surface, query, nextIndex))
          case _ =>
            ReducerResult.noEffects(currentState)
      case _ =>
        ReducerResult.noEffects(currentState)

  private def reduceFileWorkflow(event: ModalInputEvent, currentState: AppState): ReducerResult =
    event match
      case ModalDismiss =>
        ReducerResult.noEffects(dismissToPane(currentState))
      case ModalInsertChar(char) =>
        currentModal(currentState) match
          case Some((surface, Modal.FileWorkflow(workflow))) =>
            ReducerResult.withEffect(
              updateModal(currentState, surface, Modal.FileWorkflow(workflow.appendToActiveField(char))),
              AppEffect.RefreshFileWorkflow(surface.id)
            )
          case _ =>
            ReducerResult.noEffects(currentState)
      case ModalDeleteBackward =>
        currentModal(currentState) match
          case Some((surface, Modal.FileWorkflow(workflow))) =>
            ReducerResult.withEffect(
              updateModal(currentState, surface, Modal.FileWorkflow(workflow.deleteFromActiveField)),
              AppEffect.RefreshFileWorkflow(surface.id)
            )
          case _ =>
            ReducerResult.noEffects(currentState)
      case ModalDeleteForward =>
        currentModal(currentState) match
          case Some((surface, Modal.FileWorkflow(workflow))) =>
            ReducerResult.withEffect(
              updateModal(currentState, surface, Modal.FileWorkflow(workflow.deleteForwardFromActiveField)),
              AppEffect.RefreshFileWorkflow(surface.id)
            )
          case _ =>
            ReducerResult.noEffects(currentState)
      case ModalDeleteWordBackward =>
        currentModal(currentState) match
          case Some((surface, Modal.FileWorkflow(workflow))) =>
            ReducerResult.withEffect(
              updateModal(currentState, surface, Modal.FileWorkflow(workflow.deleteWordBackwardFromActiveField)),
              AppEffect.RefreshFileWorkflow(surface.id)
            )
          case _ =>
            ReducerResult.noEffects(currentState)
      case ModalDeleteWordForward =>
        currentModal(currentState) match
          case Some((surface, Modal.FileWorkflow(workflow))) =>
            ReducerResult.withEffect(
              updateModal(currentState, surface, Modal.FileWorkflow(workflow.deleteWordForwardFromActiveField)),
              AppEffect.RefreshFileWorkflow(surface.id)
            )
          case _ =>
            ReducerResult.noEffects(currentState)
      case ModalNextField =>
        currentModal(currentState) match
          case Some((surface, Modal.FileWorkflow(workflow))) if workflow.suggestions.nonEmpty =>
            ReducerResult.withEffect(
              updateModal(currentState, surface, Modal.FileWorkflow(workflow.applySelectedSuggestion)),
              AppEffect.RefreshFileWorkflow(surface.id)
            )
          case Some((surface, Modal.FileWorkflow(workflow))) =>
            ReducerResult.withEffect(
              updateModal(currentState, surface, Modal.FileWorkflow(workflow.switchField(1))),
              AppEffect.RefreshFileWorkflow(surface.id)
            )
          case _ =>
            ReducerResult.noEffects(currentState)
      case ModalPreviousField =>
        currentModal(currentState) match
          case Some((surface, Modal.FileWorkflow(workflow))) =>
            ReducerResult.withEffect(
              updateModal(currentState, surface, Modal.FileWorkflow(workflow.switchField(-1))),
              AppEffect.RefreshFileWorkflow(surface.id)
            )
          case _ =>
            ReducerResult.noEffects(currentState)
      case ModalNavigate(Direction.Up) =>
        currentModal(currentState) match
          case Some((surface, Modal.FileWorkflow(workflow))) =>
            ReducerResult.noEffects(updateModal(currentState, surface, Modal.FileWorkflow(workflow.moveSuggestion(-1))))
          case _ =>
            ReducerResult.noEffects(currentState)
      case ModalNavigate(Direction.Down) =>
        currentModal(currentState) match
          case Some((surface, Modal.FileWorkflow(workflow))) =>
            ReducerResult.noEffects(updateModal(currentState, surface, Modal.FileWorkflow(workflow.moveSuggestion(1))))
          case _ =>
            ReducerResult.noEffects(currentState)
      case ModalSubmit =>
        currentModal(currentState) match
          case Some((surface, Modal.FileWorkflow(_))) =>
            ReducerResult.withEffect(currentState, AppEffect.SubmitFileWorkflow(surface.id))
          case _ =>
            ReducerResult.noEffects(currentState)
      case _ =>
        ReducerResult.noEffects(currentState)

  private def reduceCloseWorkflow(event: ModalInputEvent, currentState: AppState): ReducerResult =
    event match
      case ModalDismiss =>
        ReducerResult.noEffects(cancelCloseWorkflow(currentState))
      case ModalNextField | ModalNavigate(Direction.Right) =>
        currentModal(currentState) match
          case Some((surface, Modal.CloseWorkflow(workflow))) =>
            ReducerResult.noEffects(updateModal(currentState, surface, Modal.CloseWorkflow(workflow.moveChoice(1))))
          case _ =>
            ReducerResult.noEffects(currentState)
      case ModalPreviousField | ModalNavigate(Direction.Left) =>
        currentModal(currentState) match
          case Some((surface, Modal.CloseWorkflow(workflow))) =>
            ReducerResult.noEffects(updateModal(currentState, surface, Modal.CloseWorkflow(workflow.moveChoice(-1))))
          case _ =>
            ReducerResult.noEffects(currentState)
      case ModalSubmit =>
        currentModal(currentState) match
          case Some((surface, Modal.CloseWorkflow(_))) =>
            ReducerResult.withEffect(currentState, AppEffect.SubmitCloseWorkflow(surface.id))
          case _ =>
            ReducerResult.noEffects(currentState)
      case _ =>
        ReducerResult.noEffects(currentState)

  private def reduceReplaceWorkflow(event: ModalInputEvent, currentState: AppState): ReducerResult =
    event match
      case ModalDismiss =>
        ReducerResult.noEffects(dismissToPane(currentState))
      case ModalInsertChar(char) =>
        currentModal(currentState) match
          case Some((surface, Modal.ReplaceWorkflow(workflow))) =>
            ReducerResult.noEffects(
              updateReplaceWorkflow(currentState, surface, workflow.appendToActiveField(char))
            )
          case _ =>
            ReducerResult.noEffects(currentState)
      case ModalDeleteBackward =>
        currentModal(currentState) match
          case Some((surface, Modal.ReplaceWorkflow(workflow))) =>
            ReducerResult.noEffects(
              updateReplaceWorkflow(currentState, surface, workflow.deleteFromActiveField)
            )
          case _ =>
            ReducerResult.noEffects(currentState)
      case ModalDeleteForward =>
        currentModal(currentState) match
          case Some((surface, Modal.ReplaceWorkflow(workflow))) =>
            ReducerResult.noEffects(
              updateReplaceWorkflow(currentState, surface, workflow.deleteForwardFromActiveField)
            )
          case _ =>
            ReducerResult.noEffects(currentState)
      case ModalDeleteWordBackward =>
        currentModal(currentState) match
          case Some((surface, Modal.ReplaceWorkflow(workflow))) =>
            ReducerResult.noEffects(
              updateReplaceWorkflow(currentState, surface, workflow.deleteWordBackwardFromActiveField)
            )
          case _ =>
            ReducerResult.noEffects(currentState)
      case ModalDeleteWordForward =>
        currentModal(currentState) match
          case Some((surface, Modal.ReplaceWorkflow(workflow))) =>
            ReducerResult.noEffects(
              updateReplaceWorkflow(currentState, surface, workflow.deleteWordForwardFromActiveField)
            )
          case _ =>
            ReducerResult.noEffects(currentState)
      case ModalNextField =>
        currentModal(currentState) match
          case Some((surface, Modal.ReplaceWorkflow(workflow))) =>
            ReducerResult.noEffects(
              updateReplaceWorkflow(currentState, surface, workflow.switchField(1))
            )
          case _ =>
            ReducerResult.noEffects(currentState)
      case ModalPreviousField =>
        currentModal(currentState) match
          case Some((surface, Modal.ReplaceWorkflow(workflow))) =>
            ReducerResult.noEffects(
              updateReplaceWorkflow(currentState, surface, workflow.switchField(-1))
            )
          case _ =>
            ReducerResult.noEffects(currentState)
      case ModalNavigate(Direction.Left) =>
        currentModal(currentState) match
          case Some((surface, Modal.ReplaceWorkflow(workflow))) =>
            ReducerResult.noEffects(
              updateReplaceWorkflow(currentState, surface, workflow.moveAction(-1))
            )
          case _ =>
            ReducerResult.noEffects(currentState)
      case ModalNavigate(Direction.Right) =>
        currentModal(currentState) match
          case Some((surface, Modal.ReplaceWorkflow(workflow))) =>
            ReducerResult.noEffects(
              updateReplaceWorkflow(currentState, surface, workflow.moveAction(1))
            )
          case _ =>
            ReducerResult.noEffects(currentState)
      case ModalNavigate(Direction.Up) =>
        currentModal(currentState) match
          case Some((surface, Modal.ReplaceWorkflow(workflow))) =>
            ReducerResult.noEffects(
              updateReplaceWorkflow(currentState, surface, workflow.moveScope(-1))
            )
          case _ =>
            ReducerResult.noEffects(currentState)
      case ModalNavigate(Direction.Down) =>
        currentModal(currentState) match
          case Some((surface, Modal.ReplaceWorkflow(workflow))) =>
            ReducerResult.noEffects(
              updateReplaceWorkflow(currentState, surface, workflow.moveScope(1))
            )
          case _ =>
            ReducerResult.noEffects(currentState)
      case ModalSubmit =>
        currentModal(currentState) match
          case Some((surface, Modal.ReplaceWorkflow(_))) =>
            ReducerResult.withEffect(currentState, AppEffect.SubmitReplaceWorkflow(surface.id))
          case _ =>
            ReducerResult.noEffects(currentState)
      case _ =>
        ReducerResult.noEffects(currentState)

  private def updateReplaceWorkflow(
    state: AppState,
    surface: UiSurface,
    workflow: ReplaceWorkflowState
  ): AppState =
    updateModal(state, surface, Modal.ReplaceWorkflow(withReplacePreview(state, workflow)))

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
            val startOffset = offsetForCursor(buffer.content, selection.start)
            val endOffset   = offsetForCursor(buffer.content, selection.end)
            Right(Some((math.min(startOffset, endOffset), math.max(startOffset, endOffset))))
          case None =>
            Left("Select text to preview selection matches")

  private def scopedReplaceMatches(
    buffer: Buffer,
    findText: String,
    range: Option[(Int, Int)]
  ): List[Int] =
    buffer.content.searchAll(findText).filter { offset =>
      range match
        case Some((startOffset, endOffset)) =>
          offset >= startOffset && (offset + findText.length) <= endOffset
        case None =>
          true
    }

  private def updateFindQuery(state: AppState, surface: UiSurface, query: String): AppState =
    updateFindSelection(state, surface, query, 0)

  private def updateFindSelection(
    state: AppState,
    surface: UiSurface,
    query: String,
    requestedIndex: Int
  ): AppState =
    val resultSet = FindResultSet.normalized(query, activeFindMatches(state, query).map(toFindResult), requestedIndex)
    val modalState = updateModal(
      state,
      surface,
      Modal.Find(resultSet.query, resultSet.results, resultSet.currentIndex)
    )

    if resultSet.query.isEmpty || resultSet.results.isEmpty then clearActiveFindState(modalState)
    else applyFindMatch(modalState, resultSet)

  private def activeFindMatches(state: AppState, query: String): List[CursorPosition] =
    if query.isEmpty then Nil
    else
      activeBuffer(state)
        .map(buffer => buffer.content.searchAll(query).map(offset => cursorPositionForOffset(buffer.content, offset)))
        .getOrElse(Nil)

  private def applyFindMatch(
    state: AppState,
    resultSet: FindResultSet
  ): AppState =
    activeBufferId(state) match
      case Some(bufferId) =>
        state.buffers.get(bufferId) match
          case Some(buffer) =>
            val selected = resultSet.results(resultSet.currentIndex)
            val target   = CursorPosition(selected.line, selected.column)
            val baseBuffer = buffer.copy(
              cursors = List(target),
              selection = None,
              selections = Nil,
              preferredColumn = Some(target.column),
              preferredXPx = None,
              findState = Some(FindState.fromResultSet(resultSet))
            )
            val updatedBuffer = baseBuffer.copy(
              viewport = CursorViewport.adjustForCursor(baseBuffer, state, target)
            )
            state.copy(buffers = state.buffers + (bufferId -> updatedBuffer))
          case None =>
            state
      case None =>
        state

  private def clearActiveFindState(state: AppState): AppState =
    activeBufferId(state) match
      case Some(bufferId) =>
        state.buffers.get(bufferId) match
          case Some(buffer) =>
            state.copy(buffers = state.buffers + (bufferId -> buffer.copy(findState = None)))
          case None =>
            state
      case None =>
        state

  private def toFindResult(cursor: CursorPosition): FindResult =
    FindResult(cursor.line, cursor.column)

  private def activeBuffer(state: AppState): Option[Buffer] =
    activeBufferId(state).flatMap(state.buffers.get)

  private def activeBufferId(state: AppState): Option[BufferId] =
    state.layout.activeEditorPaneId.flatMap(paneId => state.layout.editorPanes.get(paneId).flatMap(_.bufferId))

  private def offsetForCursor(content: Rope, cursor: CursorPosition): Int =
    content.lineColumnToOffset(cursor.line, cursor.column)

  private def cursorPositionForOffset(content: Rope, offset: Int): CursorPosition =
    val (line, column) = content.offsetToLineColumn(offset)
    CursorPosition(line, column)

  private def dismissToPane(state: AppState): AppState =
    state.layout.activeEditorPaneId match
      case Some(paneId) =>
        state.copy(uiSurfaces = state.uiSurfaces.filterNot(isModalSurface), focus = Focus.EditorPane(paneId))
      case None =>
        state.startPageSurface match
          case Some(startPage) =>
            state.copy(uiSurfaces = state.uiSurfaces.filterNot(isModalSurface), focus = Focus.Surface(startPage.id))
          case None =>
            state.copy(uiSurfaces = state.uiSurfaces.filterNot(isModalSurface))

  private def cancelCloseWorkflow(state: AppState): AppState =
    dismissToPane(
      state.copy(actionStack = state.actionStack.filter { case AppAction.CloseWorkflow(_) => false })
    )

  private def jumpToLine(state: AppState, targetLine: Int): AppState =
    state.layout.activeEditorPaneId match
      case Some(paneId) =>
        state.layout.editorPanes.get(paneId) match
          case Some(pane) =>
            pane.bufferId.flatMap(state.buffers.get) match
              case Some(buffer) =>
                val target      = clampedCursor(buffer, targetLine, 0)
                val halfVisible = buffer.viewport.visibleLines / 2
                val maxTopLine  = math.max(0, buffer.content.lineCount - buffer.viewport.visibleLines)
                val newTopLine  = math.min(maxTopLine, math.max(0, target.line - halfVisible))
                val updatedBuffer = buffer.copy(
                  cursors = List(target),
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

  private def clampedCursor(buffer: Buffer, targetLine: Int, targetColumn: Int): CursorPosition =
    val lastLine      = math.max(0, buffer.content.lineCount - 1)
    val line          = math.max(0, math.min(targetLine, lastLine))
    val lineLength    = buffer.content.getLine(line).map(_.length).getOrElse(0)
    val clampedColumn = math.max(0, math.min(targetColumn, lineLength))
    CursorPosition(line, clampedColumn)

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
