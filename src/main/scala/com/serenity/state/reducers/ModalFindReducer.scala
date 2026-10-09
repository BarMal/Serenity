package com.serenity.state.reducers

import com.serenity.keystroke.events.*
import com.serenity.state.models.*
import com.serenity.ui.layout.SurfaceAction
import com.serenity.ui.widget.{TextField, TextFieldOutcome, WidgetInput}

/** Find modal input: typing, deletes, paste, Left/Right and Home/End edit the query through its `TextField`, and each
  * change to the query's text or options starts a new search seeded from the caret; Enter, Down and find-next step to
  * the next match after the caret, Up and find-previous to the one before it, Ctrl+Home/Ctrl+End jump to the ends of
  * the results, as does clicking one. Also applies search results computed asynchronously by the find-search effect.
  */
private[reducers] object ModalFindReducer:
  import ModalEventReducer.{currentModal, dismissToPane, updateModal}

  def reduce(event: ModalInputEvent, currentState: AppState): ReducerResult =
    event match
      case ModalDismiss => ReducerResult.noEffects(dismissToPane(currentState))
      case _ =>
        currentModal(currentState) match
          case Some((id, find: Modal.Find)) =>
            event match
              case ModalToggleFindOption(option) =>
                updateFindQuery(currentState, id, find.query, find.options.toggled(option))
              case _ =>
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
      case Some(TextFieldOutcome.Changed(_)) => updateFindQuery(state, id, query, find.options)
      case _ if query == find.query          => ReducerResult.noEffects(state)
      case _ => ReducerResult.noEffects(updateModal(state, id, find.copy(query = query)))

  private def navigated(event: ModalInputEvent, id: SurfaceId, find: Modal.Find, state: AppState): ReducerResult =
    val move: Option[Either[FindDirection, Int]] = event match
      case ModalFindNext | ModalNavigate(Direction.Down)   => Some(Left(FindDirection.Forward))
      case ModalFindPrevious | ModalNavigate(Direction.Up) => Some(Left(FindDirection.Backward))
      case ModalSubmit                                     => Some(Left(FindDirection.Forward))
      case ModalFirst                                      => Some(Right(0))
      case ModalLast                                       => Some(Right(find.results.length - 1))
      case ModalActionClick(SurfaceAction.SelectFindResult(index)) if find.results.indices.contains(index) =>
        Some(Right(index))
      case _ => None
    move
      .filter(_ => find.query.text.nonEmpty)
      .fold(ReducerResult.noEffects(state)) {
        case Left(direction) if find.results.nonEmpty => ReducerResult.noEffects(stepped(state, id, find, direction))
        case Left(_)      => ReducerResult.noEffects(selected(state, id, find, find.results, 0))
        case Right(index) => ReducerResult.noEffects(selected(state, id, find, find.results, index))
      }

  private def stepped(state: AppState, id: SurfaceId, find: Modal.Find, direction: FindDirection): AppState =
    activeBuffer(state).fold(state) { buffer =>
      val current = FindState(find.query.text, find.results, find.currentIndex, find.options, find.capped)
      FindNavigation.step(buffer.document.content, current, buffer.editing.cursors.head.position, direction) match
        case Some(next) => selected(state, id, withCapped(find, next.capped), next.results, next.currentIndex)
        case None       => selected(state, id, find, Vector.empty, 0)
    }

  def applyFindSearchResults(
    state: AppState,
    request: FindSearchRequest,
    results: Vector[FindResult],
    capped: Boolean
  ): AppState =
    val currentFind = state.runtime.uiSurfaces.collectFirst {
      case UiSurface(id, SurfaceContent.ModalWorkflow(find: Modal.Find), _, _)
          if id == request.surfaceId && find.query.text == request.query && find.options == request.options =>
        find
    }
    val currentBuffer = state.persisted.buffers
      .get(request.bufferId)
      .filter(buffer => buffer.document.content.eq(request.content) && activeBufferId(state).contains(request.bufferId))

    (currentFind, currentBuffer) match
      case (Some(find), Some(buffer)) =>
        val firstAtOrAfterCaret = FindResultSet.indexAtOrAfter(results, buffer.editing.cursors.head.position)
        val landed              = withCapped(find, capped)
        request.purpose match
          case FindSearchPurpose.Seed    => selected(state, request.surfaceId, landed, results, firstAtOrAfterCaret)
          case FindSearchPurpose.Refresh => synced(state, request.surfaceId, landed, results, firstAtOrAfterCaret)
      case _ => state

  def refreshDue(before: AppState, after: AppState): Option[FindSearchRequest] =
    for
      (id, find) <- openFind(after)
      if find.query.text.nonEmpty && FindPattern.compile(find.query.text, find.options).isRight
      bufferId <- activeBufferId(after)
      buffer   <- after.persisted.buffers.get(bufferId)
      opened  = !openFind(before).exists(_._1 == id)
      changed = before.persisted.buffers.get(bufferId).forall(!_.document.content.eq(buffer.document.content))
      if opened || changed
    yield searchRequest(id, bufferId, buffer, find.query.text, find.options, FindSearchPurpose.Refresh)

  private def openFind(state: AppState): Option[(SurfaceId, Modal.Find)] =
    state.runtime.uiSurfaces.collectFirst {
      case UiSurface(id, SurfaceContent.ModalWorkflow(find: Modal.Find), _, _) => id -> find
    }

  private def updateFindQuery(state: AppState, id: SurfaceId, query: TextField, options: FindOptions): ReducerResult =
    val queryState = updateModal(state, id, Modal.Find(query, Vector.empty, 0, options))
    val clearedState = activeBufferId(queryState)
      .map(bufferId => clearFindState(queryState, bufferId))
      .getOrElse(queryState)
    val searchable = query.text.nonEmpty && FindPattern.compile(query.text, options).isRight
    if !searchable then ReducerResult.noEffects(clearedState)
    else
      (for
        bufferId <- activeBufferId(clearedState)
        buffer   <- clearedState.persisted.buffers.get(bufferId)
      yield ReducerResult.withEffect(
        clearedState,
        AppEffect.Workflow(
          WorkflowEffect.RefreshFind(
            searchRequest(id, bufferId, buffer, query.text, options, FindSearchPurpose.Seed)
          )
        )
      )).getOrElse(ReducerResult.noEffects(clearedState))

  private def searchRequest(
    id: SurfaceId,
    bufferId: BufferId,
    buffer: Buffer,
    query: String,
    options: FindOptions,
    purpose: FindSearchPurpose
  ): FindSearchRequest =
    val caret   = buffer.editing.cursors.head.position
    val content = buffer.document.content
    FindSearchRequest(
      id,
      bufferId,
      query,
      content,
      options,
      content.lineColumnToOffset(caret.line, caret.column),
      purpose
    )

  /** Selects `results(requestedIndex)` in the modal and the buffer, moving the caret onto it. */
  private def selected(
    state: AppState,
    id: SurfaceId,
    find: Modal.Find,
    results: Vector[FindResult],
    requestedIndex: Int
  ): AppState =
    val resultSet  = FindResultSet.normalized(find.query.text, results, requestedIndex, find.capped)
    val modalState = updateModal(state, id, modalShowing(find, resultSet))
    resultSet.selectedResult match
      case Some(target) =>
        updateActiveBuffer(modalState)(
          _.copy(
            editing = EditingState(List(CursorPosition(target.line, target.column))),
            findState = Some(FindState.fromResultSet(resultSet, find.options))
          )
        )
      case None => clearActiveFindState(modalState)

  /** As [[selected]], leaving the caret where it is. */
  private def synced(
    state: AppState,
    id: SurfaceId,
    find: Modal.Find,
    results: Vector[FindResult],
    requestedIndex: Int
  ): AppState =
    val resultSet  = FindResultSet.normalized(find.query.text, results, requestedIndex, find.capped)
    val modalState = updateModal(state, id, modalShowing(find, resultSet))
    if resultSet.results.isEmpty then clearActiveFindState(modalState)
    else updateActiveBuffer(modalState)(_.copy(findState = Some(FindState.fromResultSet(resultSet, find.options))))

  // `new`, since an enum case's `apply` and `copy` widen to `Modal`.
  private def withCapped(find: Modal.Find, capped: Boolean): Modal.Find =
    new Modal.Find(find.query, find.results, find.currentIndex, find.options, capped)

  private def modalShowing(find: Modal.Find, resultSet: FindResultSet): Modal =
    find.copy(results = resultSet.results, currentIndex = resultSet.currentIndex, capped = resultSet.capped)

  private def updateActiveBuffer(state: AppState)(f: Buffer => Buffer): AppState =
    (for
      bufferId <- activeBufferId(state)
      buffer   <- state.persisted.buffers.get(bufferId)
    yield state.copy(persisted = state.persisted.copy(buffers = state.persisted.buffers + (bufferId -> f(buffer)))))
      .getOrElse(state)

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
    activeBufferId(state).flatMap(state.persisted.buffers.get)

  private def activeBufferId(state: AppState): Option[BufferId] =
    state.persisted.layout.activeEditorPaneId.flatMap(paneId =>
      state.persisted.layout.editorPanes.get(paneId).flatMap(_.bufferId)
    )
