package com.serenity.state.reducers

import com.serenity.keystroke.events.*
import com.serenity.rope.Rope
import com.serenity.state.models.*
import com.serenity.text.TextEditing

object ModalEventReducer:

  def selectCloseWorkflowChoice(choice: CloseWorkflowChoice, currentState: AppState): ReducerResult =
    currentModal(currentState) match
      case Some((surface, Modal.CloseWorkflow(workflow))) =>
        ReducerResult.noEffects(
          updateModal(currentState, surface, Modal.CloseWorkflow(workflow.copy(selectedChoice = choice)))
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
          case Some((surface, Modal.Find(query, _, _))) =>
            updateFindQuery(currentState, surface, query + char)
          case _ => ReducerResult.noEffects(currentState)
      case ModalDeleteBackward =>
        currentModal(currentState) match
          case Some((surface, Modal.Find(query, _, _))) if query.nonEmpty =>
            updateFindQuery(currentState, surface, query.dropRight(1))
          case _ => ReducerResult.noEffects(currentState)
      case ModalDeleteForward =>
        ReducerResult.noEffects(currentState)
      case ModalDeleteWordBackward =>
        currentModal(currentState) match
          case Some((surface, Modal.Find(query, _, _))) =>
            updateFindQuery(currentState, surface, TextEditing.deleteWordBackward(query))
          case _ => ReducerResult.noEffects(currentState)
      case ModalDeleteWordForward =>
        currentModal(currentState) match
          case Some((surface, Modal.Find(query, _, _))) =>
            updateFindQuery(currentState, surface, TextEditing.deleteWordForward(query))
          case _ => ReducerResult.noEffects(currentState)
      case ModalFindNext | ModalNavigate(Direction.Down) | ModalNavigate(Direction.Right) =>
        currentModal(currentState) match
          case Some((surface, Modal.Find(query, results, currentIndex))) if query.nonEmpty =>
            ReducerResult.noEffects(updateFindSelection(currentState, surface, query, results, currentIndex + 1))
          case _ => ReducerResult.noEffects(currentState)
      case ModalNavigate(Direction.Up) | ModalNavigate(Direction.Left) =>
        currentModal(currentState) match
          case Some((surface, Modal.Find(query, results, currentIndex))) if query.nonEmpty =>
            ReducerResult.noEffects(updateFindSelection(currentState, surface, query, results, currentIndex - 1))
          case _ => ReducerResult.noEffects(currentState)
      case ModalSubmit =>
        currentModal(currentState) match
          case Some((surface, Modal.Find(query, results, currentIndex))) if query.nonEmpty =>
            val nextIndex =
              if results.nonEmpty then currentIndex + 1
              else 0
            ReducerResult.noEffects(updateFindSelection(currentState, surface, query, results, nextIndex))
          case _ =>
            ReducerResult.noEffects(currentState)
      case ModalClick(_, Some(actionId)) if actionId.startsWith("find-result-") =>
        currentModal(currentState) match
          case Some((surface, Modal.Find(query, results, _))) if query.nonEmpty =>
            actionId.stripPrefix("find-result-").toIntOption match
              case Some(index) if index >= 0 && index < results.length =>
                ReducerResult.noEffects(updateFindSelection(currentState, surface, query, results, index))
              case _ => ReducerResult.noEffects(currentState)
          case _ => ReducerResult.noEffects(currentState)
      case ModalClick(_, _) =>
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
      case ModalClick(focusId, actionId) =>
        currentModal(currentState) match
          case Some((surface, Modal.FileWorkflow(workflow))) =>
            val updated = actionId match
              case Some(id) if id.startsWith("file-suggestion-") =>
                id.stripPrefix("file-suggestion-")
                  .toIntOption
                  .filter(index => index >= 0 && index < workflow.suggestions.length)
                  .map(index => workflow.updated(selectedSuggestionIndex = index, statusMessage = None))
              case _ => None
            val fieldUpdated = focusId match
              case "filename" => Some(workflow.updated(activeField = FileWorkflowField.Filename, statusMessage = None))
              case "path"     => Some(workflow.updated(activeField = FileWorkflowField.Path, statusMessage = None))
              case _          => None
            val nextState =
              updateModal(currentState, surface, Modal.FileWorkflow(updated.orElse(fieldUpdated).getOrElse(workflow)))
            if fieldUpdated.nonEmpty then ReducerResult.withEffect(nextState, AppEffect.RefreshFileWorkflow(surface.id))
            else ReducerResult.noEffects(nextState)
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
      case ModalClick(_, Some(actionId)) =>
        currentModal(currentState) match
          case Some((surface, Modal.CloseWorkflow(workflow))) =>
            val choice = actionId match
              case "close-save"    => Some(CloseWorkflowChoice.Save)
              case "close-discard" => Some(CloseWorkflowChoice.Discard)
              case "close-cancel"  => Some(CloseWorkflowChoice.Cancel)
              case _               => None
            ReducerResult.noEffects(
              choice.fold(currentState)(selected =>
                updateModal(
                  currentState,
                  surface,
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
      case ModalClick(focusId, actionId) =>
        currentModal(currentState) match
          case Some((surface, Modal.ReplaceWorkflow(workflow))) =>
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
              updateReplaceWorkflow(currentState, surface, clicked.orElse(field).getOrElse(workflow))
            )
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
      val insideScope = range match
        case Some((startOffset, endOffset)) =>
          offset >= startOffset && (offset + findText.length) <= endOffset
        case None =>
          true
      insideScope && isWholeGraphemeMatch(buffer.content, offset, findText.length)
    }

  private def updateFindQuery(state: AppState, surface: UiSurface, query: String): ReducerResult =
    surface.content match
      case SurfaceContent.ModalWorkflow(Modal.Find(currentQuery, _, _)) if currentQuery == query =>
        ReducerResult.noEffects(state)
      case _ =>
        val queryState = updateModal(state, surface, Modal.Find(query, Nil, 0))
        val clearedState = activeBufferId(queryState)
          .map(bufferId => clearFindState(queryState, bufferId))
          .getOrElse(queryState)
        if query.isEmpty then ReducerResult.noEffects(clearedState)
        else
          (for
            bufferId <- activeBufferId(clearedState)
            buffer   <- clearedState.buffers.get(bufferId)
          yield ReducerResult.withEffect(
            clearedState,
            AppEffect.RefreshFind(FindSearchRequest(surface.id, bufferId, query, buffer.content))
          )).getOrElse(ReducerResult.noEffects(clearedState))

  private def updateFindSelection(
    state: AppState,
    surface: UiSurface,
    query: String,
    results: List[FindResult],
    requestedIndex: Int
  ): AppState =
    val resultSet = FindResultSet.normalized(query, results, requestedIndex)
    val modalState = updateModal(
      state,
      surface,
      Modal.Find(resultSet.query, resultSet.results, resultSet.currentIndex)
    )

    if resultSet.query.isEmpty || resultSet.results.isEmpty then clearActiveFindState(modalState)
    else applyFindMatch(modalState, resultSet)

  def applyFindSearchResults(
    state: AppState,
    request: FindSearchRequest,
    results: List[FindResult]
  ): AppState =
    val modalIsCurrent = state.uiSurfaces.exists {
      case UiSurface(id, SurfaceContent.ModalWorkflow(Modal.Find(query, _, _)), _, _) =>
        id == request.surfaceId && query == request.query
      case _ =>
        false
    }
    val contentIsCurrent = state.buffers.get(request.bufferId).exists(_.content.eq(request.content))

    if !modalIsCurrent || !contentIsCurrent || !activeBufferId(state).contains(request.bufferId) then state
    else
      state.uiSurfaces.find(_.id == request.surfaceId) match
        case Some(surface) => updateFindSelection(state, surface, request.query, results, requestedIndex = 0)
        case None          => state

  private def isWholeGraphemeMatch(content: Rope, offset: Int, length: Int): Boolean =
    TextEditing.isWholeGraphemeRange(RopeCharacterSource(content), offset, offset + length)

  final private case class RopeCharacterSource(content: Rope) extends TextEditing.CharacterSource:
    override def length: Int = content.weight

    override def charAt(index: Int): Char = content.index(index).getOrElse('\u0000')

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
            val updatedBuffer = buffer.copy(
              cursors = List(target),
              selection = None,
              selections = Nil,
              preferredColumn = Some(target.column),
              preferredXPx = None,
              findState = Some(FindState.fromResultSet(resultSet))
            )
            state.copy(buffers = state.buffers + (bufferId -> updatedBuffer))
          case None =>
            state
      case None =>
        state

  private def clearActiveFindState(state: AppState): AppState =
    activeBufferId(state).map(bufferId => clearFindState(state, bufferId)).getOrElse(state)

  private def clearFindState(state: AppState, bufferId: BufferId): AppState =
    state.buffers.get(bufferId) match
      case Some(buffer) => state.copy(buffers = state.buffers + (bufferId -> buffer.copy(findState = None)))
      case None         => state

  private def activeBuffer(state: AppState): Option[Buffer] =
    activeBufferId(state).flatMap(state.buffers.get)

  private def activeBufferId(state: AppState): Option[BufferId] =
    state.layout.activeEditorPaneId.flatMap(paneId => state.layout.editorPanes.get(paneId).flatMap(_.bufferId))

  private def offsetForCursor(content: Rope, cursor: CursorPosition): Int =
    content.lineColumnToOffset(cursor.line, cursor.column)

  private def dismissToPane(state: AppState): AppState =
    state.dismissTopModal

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
                val halfVisible = buffer.viewport.visibleLines / 2
                val newTopLine  = math.max(0, targetLine - halfVisible)
                val updatedBuffer = buffer.copy(
                  cursors = List(CursorPosition(targetLine, 0)),
                  viewport = buffer.viewport.copy(topLine = newTopLine)
                )
                state.dismissTopModal.copy(buffers = state.buffers + (buffer.id -> updatedBuffer))
              case None =>
                state.dismissTopModal
          case None =>
            state.dismissTopModal
      case None =>
        state.dismissTopModal

  private def currentModal(state: AppState): Option[(UiSurface, Modal)] =
    state.topModalSurface.orElse(state.activeSurface).flatMap { surface =>
      surface.content match
        case SurfaceContent.ModalWorkflow(modal) => Some((surface, modal))
        case _                                   => None
    }

  private def updateModal(state: AppState, surface: UiSurface, modal: Modal): AppState =
    val updatedSurface = surface.copy(content = SurfaceContent.ModalWorkflow(modal))
    state.copy(uiSurfaces = state.uiSurfaces.filterNot(_.id == surface.id) :+ updatedSurface)
