package com.serenity.state.reducers

import com.serenity.keystroke.events.*
import com.serenity.state.models.*
import com.serenity.text.TextEditing

/** Find modal input -- query editing, navigating between results, and applying search results computed asynchronously
  * by the find-search effect. Split out of `ModalEventReducer`'s per-modal-type dispatch when that file grew past its
  * 600-line target.
  */
private[reducers] object ModalFindReducer:
  import ModalEventReducer.{currentModal, dismissToPane, updateModal}

  def reduce(event: ModalInputEvent, currentState: AppState): ReducerResult =
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

  private def activeBufferId(state: AppState): Option[BufferId] =
    state.persisted.layout.activeEditorPaneId.flatMap(paneId =>
      state.persisted.layout.editorPanes.get(paneId).flatMap(_.bufferId)
    )
