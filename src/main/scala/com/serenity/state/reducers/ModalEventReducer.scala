package com.serenity.state.reducers

import com.serenity.keystroke.events.*
import com.serenity.rope.*
import com.serenity.state.models.*
import com.serenity.text.TextEditing

object ModalEventReducer:

  def selectCloseWorkflowChoice(choice: CloseWorkflowChoice, currentState: AppState): ReducerResult =
    currentModal(currentState) match
      case Some((id, Modal.CloseWorkflow(workflow))) =>
        ReducerResult.noEffects(
          updateModal(currentState, id, Modal.CloseWorkflow(workflow.copy(selectedChoice = choice)))
        )
      case _ =>
        ReducerResult.noEffects(currentState)

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

  private def reduceFind(event: ModalInputEvent, currentState: AppState): ReducerResult =
    event match
      case ModalDismiss => ReducerResult.noEffects(dismissToPane(currentState))
      case ModalInsertChar(char) =>
        currentModal(currentState) match
          case Some((id, Modal.Find(query, _, _))) =>
            updateFindQuery(currentState, id, query + char)
          case _ => ReducerResult.noEffects(currentState)
      case ModalDeleteBackward =>
        currentModal(currentState) match
          case Some((id, Modal.Find(query, _, _))) if query.nonEmpty =>
            updateFindQuery(currentState, id, query.dropRight(1))
          case _ => ReducerResult.noEffects(currentState)
      case ModalDeleteForward =>
        ReducerResult.noEffects(currentState)
      case ModalDeleteWordBackward =>
        currentModal(currentState) match
          case Some((id, Modal.Find(query, _, _))) =>
            updateFindQuery(currentState, id, TextEditing.deleteWordBackward(query))
          case _ => ReducerResult.noEffects(currentState)
      case ModalDeleteWordForward =>
        currentModal(currentState) match
          case Some((id, Modal.Find(query, _, _))) =>
            updateFindQuery(currentState, id, TextEditing.deleteWordForward(query))
          case _ => ReducerResult.noEffects(currentState)
      case ModalFindNext | ModalNavigate(Direction.Down) | ModalNavigate(Direction.Right) =>
        currentModal(currentState) match
          case Some((id, Modal.Find(query, results, currentIndex))) if query.nonEmpty =>
            ReducerResult.noEffects(updateFindSelection(currentState, id, query, results, currentIndex + 1))
          case _ => ReducerResult.noEffects(currentState)
      case ModalNavigate(Direction.Up) | ModalNavigate(Direction.Left) =>
        currentModal(currentState) match
          case Some((id, Modal.Find(query, results, currentIndex))) if query.nonEmpty =>
            ReducerResult.noEffects(updateFindSelection(currentState, id, query, results, currentIndex - 1))
          case _ => ReducerResult.noEffects(currentState)
      case ModalSubmit =>
        currentModal(currentState) match
          case Some((id, Modal.Find(query, results, currentIndex))) if query.nonEmpty =>
            val nextIndex =
              if results.nonEmpty then currentIndex + 1
              else 0
            ReducerResult.noEffects(updateFindSelection(currentState, id, query, results, nextIndex))
          case _ =>
            ReducerResult.noEffects(currentState)
      case ModalClick(_, Some(actionId)) if actionId.startsWith("find-result-") =>
        currentModal(currentState) match
          case Some((id, Modal.Find(query, results, _))) if query.nonEmpty =>
            actionId.stripPrefix("find-result-").toIntOption match
              case Some(index) if index >= 0 && index < results.length =>
                ReducerResult.noEffects(updateFindSelection(currentState, id, query, results, index))
              case _ => ReducerResult.noEffects(currentState)
          case _ => ReducerResult.noEffects(currentState)
      case ModalClick(_, _) =>
        ReducerResult.noEffects(currentState)
      case _ =>
        ReducerResult.noEffects(currentState)

  private def reduceFileWorkflow(event: ModalInputEvent, currentState: AppState): ReducerResult =
    event match
      case ModalDismiss => ReducerResult.noEffects(dismissToPane(currentState))
      case ModalInsertChar(char) =>
        currentModal(currentState) match
          case Some((id, Modal.FileWorkflow(workflow))) =>
            ReducerResult.withEffect(
              updateModal(currentState, id, Modal.FileWorkflow(workflow.appendToActiveField(char))),
              AppEffect.Workflow(WorkflowEffect.RefreshFileWorkflow(id))
            )
          case _ =>
            ReducerResult.noEffects(currentState)
      case ModalDeleteBackward =>
        currentModal(currentState) match
          case Some((id, Modal.FileWorkflow(workflow))) =>
            ReducerResult.withEffect(
              updateModal(currentState, id, Modal.FileWorkflow(workflow.deleteFromActiveField)),
              AppEffect.Workflow(WorkflowEffect.RefreshFileWorkflow(id))
            )
          case _ =>
            ReducerResult.noEffects(currentState)
      case ModalDeleteForward =>
        currentModal(currentState) match
          case Some((id, Modal.FileWorkflow(workflow))) =>
            ReducerResult.withEffect(
              updateModal(currentState, id, Modal.FileWorkflow(workflow.deleteForwardFromActiveField)),
              AppEffect.Workflow(WorkflowEffect.RefreshFileWorkflow(id))
            )
          case _ =>
            ReducerResult.noEffects(currentState)
      case ModalDeleteWordBackward =>
        currentModal(currentState) match
          case Some((id, Modal.FileWorkflow(workflow))) =>
            ReducerResult.withEffect(
              updateModal(currentState, id, Modal.FileWorkflow(workflow.deleteWordBackwardFromActiveField)),
              AppEffect.Workflow(WorkflowEffect.RefreshFileWorkflow(id))
            )
          case _ =>
            ReducerResult.noEffects(currentState)
      case ModalDeleteWordForward =>
        currentModal(currentState) match
          case Some((id, Modal.FileWorkflow(workflow))) =>
            ReducerResult.withEffect(
              updateModal(currentState, id, Modal.FileWorkflow(workflow.deleteWordForwardFromActiveField)),
              AppEffect.Workflow(WorkflowEffect.RefreshFileWorkflow(id))
            )
          case _ =>
            ReducerResult.noEffects(currentState)
      case ModalNextField =>
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
      case ModalPreviousField =>
        currentModal(currentState) match
          case Some((id, Modal.FileWorkflow(workflow))) =>
            ReducerResult.withEffect(
              updateModal(currentState, id, Modal.FileWorkflow(workflow.switchField(-1))),
              AppEffect.Workflow(WorkflowEffect.RefreshFileWorkflow(id))
            )
          case _ =>
            ReducerResult.noEffects(currentState)
      case ModalNavigate(Direction.Up) =>
        currentModal(currentState) match
          case Some((id, Modal.FileWorkflow(workflow: SaveAsFileWorkflowState)))
              if workflow.activeField == FileWorkflowField.Format =>
            ReducerResult.noEffects(updateModal(currentState, id, Modal.FileWorkflow(workflow.cycleFormat(-1))))
          case Some((id, Modal.FileWorkflow(workflow))) =>
            ReducerResult.noEffects(updateModal(currentState, id, Modal.FileWorkflow(workflow.moveSuggestion(-1))))
          case _ =>
            ReducerResult.noEffects(currentState)
      case ModalNavigate(Direction.Down) =>
        currentModal(currentState) match
          case Some((id, Modal.FileWorkflow(workflow: SaveAsFileWorkflowState)))
              if workflow.activeField == FileWorkflowField.Format =>
            ReducerResult.noEffects(updateModal(currentState, id, Modal.FileWorkflow(workflow.cycleFormat(1))))
          case Some((id, Modal.FileWorkflow(workflow))) =>
            ReducerResult.noEffects(updateModal(currentState, id, Modal.FileWorkflow(workflow.moveSuggestion(1))))
          case _ =>
            ReducerResult.noEffects(currentState)
      case ModalSubmit =>
        currentModal(currentState) match
          case Some((id, Modal.FileWorkflow(_))) =>
            ReducerResult.withEffect(currentState, AppEffect.Workflow(WorkflowEffect.SubmitFileWorkflow(id)))
          case _ =>
            ReducerResult.noEffects(currentState)
      case ModalCreateDirectory =>
        currentModal(currentState) match
          case Some((id, Modal.FileWorkflow(workflow)))
              if workflow.mode == FileWorkflowMode.SaveAs && workflow.missingPathSegments.nonEmpty =>
            ReducerResult.withEffect(
              currentState,
              AppEffect.Workflow(WorkflowEffect.CreateFileWorkflowDirectories(id))
            )
          case _ =>
            ReducerResult.noEffects(currentState)
      case ModalClick(focusId, actionId) =>
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
      case _ =>
        ReducerResult.noEffects(currentState)

  private def reduceCloseWorkflow(event: ModalInputEvent, currentState: AppState): ReducerResult =
    event match
      case ModalDismiss =>
        ReducerResult.noEffects(cancelCloseWorkflow(currentState))
      case ModalNextField | ModalNavigate(Direction.Right) | ModalNavigate(Direction.Down) =>
        currentModal(currentState) match
          case Some((id, Modal.CloseWorkflow(workflow))) =>
            ReducerResult.noEffects(updateModal(currentState, id, Modal.CloseWorkflow(workflow.moveChoice(1))))
          case _ =>
            ReducerResult.noEffects(currentState)
      case ModalPreviousField | ModalNavigate(Direction.Left) | ModalNavigate(Direction.Up) =>
        currentModal(currentState) match
          case Some((id, Modal.CloseWorkflow(workflow))) =>
            ReducerResult.noEffects(updateModal(currentState, id, Modal.CloseWorkflow(workflow.moveChoice(-1))))
          case _ =>
            ReducerResult.noEffects(currentState)
      case ModalSubmit =>
        currentModal(currentState) match
          case Some((id, Modal.CloseWorkflow(_))) =>
            ReducerResult.withEffect(currentState, AppEffect.Workflow(WorkflowEffect.SubmitCloseWorkflow(id)))
          case _ =>
            ReducerResult.noEffects(currentState)
      case ModalClick(_, Some(actionId)) =>
        currentModal(currentState) match
          case Some((id, Modal.CloseWorkflow(workflow))) =>
            val choice = actionId match
              case "close-save"    => Some(CloseWorkflowChoice.Save)
              case "close-discard" => Some(CloseWorkflowChoice.Discard)
              case "close-cancel"  => Some(CloseWorkflowChoice.Cancel)
              case _               => None
            ReducerResult.noEffects(
              choice.fold(currentState)(selected =>
                updateModal(
                  currentState,
                  id,
                  Modal.CloseWorkflow(workflow.copy(selectedChoice = selected))
                )
              )
            )
          case _ =>
            ReducerResult.noEffects(currentState)
      case ModalClick(_, _) =>
        ReducerResult.noEffects(currentState)
      case _ =>
        ReducerResult.noEffects(currentState)

  private def reduceReplaceWorkflow(event: ModalInputEvent, currentState: AppState): ReducerResult =
    event match
      case ModalDismiss =>
        ReducerResult.noEffects(dismissToPane(currentState))
      case ModalInsertChar(char) =>
        currentModal(currentState) match
          case Some((id, Modal.ReplaceWorkflow(workflow))) =>
            ReducerResult.noEffects(
              updateReplaceWorkflow(currentState, id, workflow.appendToActiveField(char))
            )
          case _ =>
            ReducerResult.noEffects(currentState)
      case ModalDeleteBackward =>
        currentModal(currentState) match
          case Some((id, Modal.ReplaceWorkflow(workflow))) =>
            ReducerResult.noEffects(
              updateReplaceWorkflow(currentState, id, workflow.deleteFromActiveField)
            )
          case _ =>
            ReducerResult.noEffects(currentState)
      case ModalDeleteForward =>
        currentModal(currentState) match
          case Some((id, Modal.ReplaceWorkflow(workflow))) =>
            ReducerResult.noEffects(
              updateReplaceWorkflow(currentState, id, workflow.deleteForwardFromActiveField)
            )
          case _ =>
            ReducerResult.noEffects(currentState)
      case ModalDeleteWordBackward =>
        currentModal(currentState) match
          case Some((id, Modal.ReplaceWorkflow(workflow))) =>
            ReducerResult.noEffects(
              updateReplaceWorkflow(currentState, id, workflow.deleteWordBackwardFromActiveField)
            )
          case _ =>
            ReducerResult.noEffects(currentState)
      case ModalDeleteWordForward =>
        currentModal(currentState) match
          case Some((id, Modal.ReplaceWorkflow(workflow))) =>
            ReducerResult.noEffects(
              updateReplaceWorkflow(currentState, id, workflow.deleteWordForwardFromActiveField)
            )
          case _ =>
            ReducerResult.noEffects(currentState)
      case ModalNextField =>
        currentModal(currentState) match
          case Some((id, Modal.ReplaceWorkflow(workflow))) =>
            ReducerResult.noEffects(
              updateReplaceWorkflow(currentState, id, workflow.switchField(1))
            )
          case _ =>
            ReducerResult.noEffects(currentState)
      case ModalPreviousField =>
        currentModal(currentState) match
          case Some((id, Modal.ReplaceWorkflow(workflow))) =>
            ReducerResult.noEffects(
              updateReplaceWorkflow(currentState, id, workflow.switchField(-1))
            )
          case _ =>
            ReducerResult.noEffects(currentState)
      case ModalNavigate(Direction.Left) =>
        currentModal(currentState) match
          case Some((id, Modal.ReplaceWorkflow(workflow))) =>
            ReducerResult.noEffects(
              updateReplaceWorkflow(currentState, id, workflow.moveAction(-1))
            )
          case _ =>
            ReducerResult.noEffects(currentState)
      case ModalNavigate(Direction.Right) =>
        currentModal(currentState) match
          case Some((id, Modal.ReplaceWorkflow(workflow))) =>
            ReducerResult.noEffects(
              updateReplaceWorkflow(currentState, id, workflow.moveAction(1))
            )
          case _ =>
            ReducerResult.noEffects(currentState)
      case ModalNavigate(Direction.Up) =>
        currentModal(currentState) match
          case Some((id, Modal.ReplaceWorkflow(workflow))) =>
            ReducerResult.noEffects(
              updateReplaceWorkflow(currentState, id, workflow.moveScope(-1))
            )
          case _ =>
            ReducerResult.noEffects(currentState)
      case ModalNavigate(Direction.Down) =>
        currentModal(currentState) match
          case Some((id, Modal.ReplaceWorkflow(workflow))) =>
            ReducerResult.noEffects(
              updateReplaceWorkflow(currentState, id, workflow.moveScope(1))
            )
          case _ =>
            ReducerResult.noEffects(currentState)
      case ModalSubmit =>
        currentModal(currentState) match
          case Some((id, Modal.ReplaceWorkflow(_))) =>
            ReducerResult.withEffect(currentState, AppEffect.Workflow(WorkflowEffect.SubmitReplaceWorkflow(id)))
          case _ =>
            ReducerResult.noEffects(currentState)
      case ModalClick(focusId, actionId) =>
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

  private def updateFindQuery(state: AppState, id: SurfaceId, query: String): ReducerResult =
    val currentQueryMatches = state
      .surfaceById(id)
      .exists(_.content match
        case SurfaceContent.ModalWorkflow(Modal.Find(currentQuery, _, _)) => currentQuery == query
        case _                                                            => false)
    if currentQueryMatches then ReducerResult.noEffects(state)
    else
      val queryState = updateModal(state, id, Modal.Find(query, Nil, 0))
      val clearedState = activeBufferId(queryState)
        .map(bufferId => clearFindState(queryState, bufferId))
        .getOrElse(queryState)
      if query.isEmpty then ReducerResult.noEffects(clearedState)
      else
        (for
          bufferId <- activeBufferId(clearedState)
          buffer   <- clearedState.persisted.buffers.get(bufferId)
        yield ReducerResult.withEffect(
          clearedState,
          AppEffect.Workflow(
            WorkflowEffect.RefreshFind(FindSearchRequest(id, bufferId, query, buffer.document.content))
          )
        )).getOrElse(ReducerResult.noEffects(clearedState))

  private def updateFindSelection(
    state: AppState,
    id: SurfaceId,
    query: String,
    results: List[FindResult],
    requestedIndex: Int
  ): AppState =
    val resultSet = FindResultSet.normalized(query, results, requestedIndex)
    val modalState = updateModal(
      state,
      id,
      Modal.Find(resultSet.query, resultSet.results, resultSet.currentIndex)
    )

    if resultSet.query.isEmpty || resultSet.results.isEmpty then clearActiveFindState(modalState)
    else applyFindMatch(modalState, resultSet)

  def applyFindSearchResults(
    state: AppState,
    request: FindSearchRequest,
    results: List[FindResult]
  ): AppState =
    val modalIsCurrent = state.runtime.uiSurfaces.exists {
      case UiSurface(id, SurfaceContent.ModalWorkflow(Modal.Find(query, _, _)), _, _) =>
        id == request.surfaceId && query == request.query
      case _ =>
        false
    }
    val contentIsCurrent =
      state.persisted.buffers.get(request.bufferId).exists(_.document.content.eq(request.content))

    if !modalIsCurrent || !contentIsCurrent || !activeBufferId(state).contains(request.bufferId) then state
    else
      state.runtime.uiSurfaces.find(_.id == request.surfaceId) match
        case Some(surface) => updateFindSelection(state, surface.id, request.query, results, requestedIndex = 0)
        case None          => state

  private def isWholeGraphemeMatch(content: Rope, offset: Int, length: Int): Boolean =
    content.isWholeGraphemeRange(offset, offset + length)

  private def applyFindMatch(
    state: AppState,
    resultSet: FindResultSet
  ): AppState =
    activeBufferId(state) match
      case Some(bufferId) =>
        state.persisted.buffers.get(bufferId) match
          case Some(buffer) =>
            val selected = resultSet.results(resultSet.currentIndex)
            val target   = CursorPosition(selected.line, selected.column)
            val updatedBuffer = buffer.copy(
              editing = buffer.editing.copy(
                cursors = List(target),
                selection = None,
                selections = Nil,
                preferredColumn = Some(target.column),
                preferredXPx = None
              ),
              findState = Some(FindState.fromResultSet(resultSet))
            )
            state.copy(persisted =
              state.persisted.copy(buffers = state.persisted.buffers + (bufferId -> updatedBuffer))
            )
          case None =>
            state
      case None =>
        state

  private def clearActiveFindState(state: AppState): AppState =
    activeBufferId(state).map(bufferId => clearFindState(state, bufferId)).getOrElse(state)

  private def clearFindState(state: AppState, bufferId: BufferId): AppState =
    state.persisted.buffers.get(bufferId) match
      case Some(buffer) =>
        state.copy(persisted =
          state.persisted.copy(buffers = state.persisted.buffers + (bufferId -> buffer.copy(findState = None)))
        )
      case None => state

  private def activeBuffer(state: AppState): Option[Buffer] =
    Focused.bufferOf(state)

  private def activeBufferId(state: AppState): Option[BufferId] =
    state.persisted.layout.activeEditorPaneId.flatMap(paneId =>
      state.persisted.layout.editorPanes.get(paneId).flatMap(_.bufferId)
    )

  private def offsetForCursor(content: Rope, cursor: CursorPosition): Int =
    content.lineColumnToOffset(cursor.line, cursor.column)

  private def dismissToPane(state: AppState): AppState =
    state.dismissTopModal

  private def cancelCloseWorkflow(state: AppState): AppState =
    dismissToPane(
      state.copy(runtime = state.runtime.copy(actionStack = state.runtime.actionStack.filter {
        case AppAction.CloseWorkflow(_) => false
      }))
    )

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

  /** The id/payload owning input: a blocking `ModalDialog` (#814) if open, else the focused modeless workflow. */
  private def currentModal(state: AppState): Option[(SurfaceId, Modal)] =
    state.topModal
      .map(dialog => (dialog.id, dialog.modal))
      .orElse(state.activeSurface.flatMap {
        case UiSurface(id, SurfaceContent.ModalWorkflow(modal), _, _) => Some((id, modal))
        case _                                                        => None
      })

  private def updateModal(state: AppState, id: SurfaceId, modal: Modal): AppState =
    state.topModal.filter(_.id == id) match
      case Some(dialog) =>
        state.copy(runtime =
          state.runtime.copy(modalStack = state.runtime.modalStack.dropRight(1) :+ dialog.copy(modal = modal))
        )
      case None =>
        state.runtime.uiSurfaces.find(_.id == id) match
          case Some(surface) =>
            val updatedSurface = surface.copy(content = SurfaceContent.ModalWorkflow(modal))
            state.copy(runtime =
              state.runtime.copy(uiSurfaces = state.runtime.uiSurfaces.filterNot(_.id == id) :+ updatedSurface)
            )
          case None => state
