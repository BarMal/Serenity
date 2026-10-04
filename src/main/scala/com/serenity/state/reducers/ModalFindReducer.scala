package com.serenity.state.reducers

import com.serenity.keystroke.events.*
import com.serenity.state.models.*
import com.serenity.ui.layout.SurfaceAction
import com.serenity.ui.widget.{TextField, TextFieldOutcome, WidgetInput}

/** Find modal input: typing, deletes, paste, Left/Right and Home/End edit the query through its `TextField`, and each
  * change to the query's text starts a new search; Up/Down, Enter, find-next and Ctrl+Home/Ctrl+End move between the
  * results, as does clicking one. Also applies search results computed asynchronously by the find-search effect.
  */
private[reducers] object ModalFindReducer:
  import ModalEventReducer.{currentModal, dismissToPane, updateModal}

  def reduce(event: ModalInputEvent, currentState: AppState): ReducerResult =
    event match
      case ModalDismiss => ReducerResult.noEffects(dismissToPane(currentState))
      case _ =>
        currentModal(currentState) match
          case Some((id, find: Modal.Find)) =>
            queryEditing(event, currentState).fold(navigated(event, id, find, currentState))(
              edited(id, find, _, currentState)
            )
          case _ => ReducerResult.noEffects(currentState)

  private def queryEditing(event: ModalInputEvent, state: AppState): Option[WidgetInput] =
    event match
      case ModalInsertChar(char)          => Some(WidgetInput.Insert(char))
      case ModalDeleteBackward            => Some(WidgetInput.DeleteBackward)
      case ModalDeleteForward             => Some(WidgetInput.DeleteForward)
      case ModalDeleteWordBackward        => Some(WidgetInput.DeleteWordBackward)
      case ModalDeleteWordForward         => Some(WidgetInput.DeleteWordForward)
      case ModalPaste                     => state.runtime.clipboard.map(WidgetInput.InsertText(_))
      case ModalNavigate(Direction.Left)  => Some(WidgetInput.Left)
      case ModalNavigate(Direction.Right) => Some(WidgetInput.Right)
      case ModalLineStart                 => Some(WidgetInput.First)
      case ModalLineEnd                   => Some(WidgetInput.Last)
      case _                              => None

  private def edited(id: SurfaceId, find: Modal.Find, input: WidgetInput, state: AppState): ReducerResult =
    val (query, outcome) = find.query.update(input)
    outcome match
      case Some(TextFieldOutcome.Changed(_)) => updateFindQuery(state, id, query)
      case _ if query == find.query          => ReducerResult.noEffects(state)
      case _ => ReducerResult.noEffects(updateModal(state, id, find.copy(query = query)))

  private def navigated(event: ModalInputEvent, id: SurfaceId, find: Modal.Find, state: AppState): ReducerResult =
    val requestedIndex = event match
      case ModalFindNext | ModalNavigate(Direction.Down) => Some(find.currentIndex + 1)
      case ModalNavigate(Direction.Up)                   => Some(find.currentIndex - 1)
      case ModalFirst                                    => Some(0)
      case ModalLast                                     => Some(find.results.length - 1)
      case ModalSubmit => Some(if find.results.nonEmpty then find.currentIndex + 1 else 0)
      case ModalActionClick(SurfaceAction.SelectFindResult(index)) if find.results.indices.contains(index) =>
        Some(index)
      case _ => None
    requestedIndex
      .filter(_ => find.query.text.nonEmpty)
      .fold(ReducerResult.noEffects(state))(index =>
        ReducerResult.noEffects(updateFindSelection(state, id, find.query, find.results, index))
      )

  def applyFindSearchResults(
    state: AppState,
    request: FindSearchRequest,
    results: Vector[FindResult]
  ): AppState =
    val currentQuery = state.runtime.uiSurfaces.collectFirst {
      case UiSurface(id, SurfaceContent.ModalWorkflow(Modal.Find(query, _, _)), _, _)
          if id == request.surfaceId && query.text == request.query =>
        query
    }
    val contentIsCurrent =
      state.persisted.buffers.get(request.bufferId).exists(_.document.content.eq(request.content))

    currentQuery match
      case Some(query) if contentIsCurrent && activeBufferId(state).contains(request.bufferId) =>
        val firstAtOrAfterCaret = state.persisted.buffers
          .get(request.bufferId)
          .fold(0)(buffer => FindResultSet.indexAtOrAfter(results, buffer.editing.cursors.head.position))
        updateFindSelection(state, request.surfaceId, query, results, firstAtOrAfterCaret)
      case _ => state

  private def updateFindQuery(state: AppState, id: SurfaceId, query: TextField): ReducerResult =
    val queryState = updateModal(state, id, Modal.Find(query, Vector.empty, 0))
    val clearedState = activeBufferId(queryState)
      .map(bufferId => clearFindState(queryState, bufferId))
      .getOrElse(queryState)
    if query.text.isEmpty then ReducerResult.noEffects(clearedState)
    else
      (for
        bufferId <- activeBufferId(clearedState)
        buffer   <- clearedState.persisted.buffers.get(bufferId)
      yield ReducerResult.withEffect(
        clearedState,
        AppEffect.Workflow(
          WorkflowEffect.RefreshFind(FindSearchRequest(id, bufferId, query.text, buffer.document.content))
        )
      )).getOrElse(ReducerResult.noEffects(clearedState))

  private def updateFindSelection(
    state: AppState,
    id: SurfaceId,
    query: TextField,
    results: Vector[FindResult],
    requestedIndex: Int
  ): AppState =
    val resultSet  = FindResultSet.normalized(query.text, results, requestedIndex)
    val modalState = updateModal(state, id, Modal.Find(query, resultSet.results, resultSet.currentIndex))

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
              editing = EditingState(List(target)),
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
