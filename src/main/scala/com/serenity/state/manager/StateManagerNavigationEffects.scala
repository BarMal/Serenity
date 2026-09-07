package com.serenity.state.manager

import cats.effect.{IO, Ref}
import com.serenity.command.{CommentsIntent, NavigationIntent}
import com.serenity.config.AppConfigMotionOps.*
import com.serenity.document.{CommentRendering, DocumentNavigation}
import com.serenity.rope.*
import com.serenity.state.models.*
import com.serenity.ui.layout.Symbol

/** Document navigation (bookmarks, symbols, comments, back/forward history) and the comment-lens/annotation mutations
  * that go with it.
  */
final private[manager] class StateManagerNavigationEffects(
    stateRef: Ref[IO, AppState],
    bufferAnimationsRef: Ref[IO, Map[BufferId, com.serenity.animation.AnimationState]],
    logger: org.typelevel.log4cats.Logger[IO]
):

  private def updateState(update: AppState => AppState): IO[Unit] = stateRef.update(update)

  private[manager] def interpretComments(intent: CommentsIntent, state: AppState): IO[Unit] =
    intent match
      case CommentsIntent.ToggleCommentLens =>
        toggleCommentLens(state)
      case CommentsIntent.AddDocumentComment(text) =>
        addDocumentComment(state, text)
      case CommentsIntent.DeleteDocumentComment =>
        deleteDocumentComment(state)
      case CommentsIntent.NextDocumentComment =>
        navigateDocumentComment(state, DocumentNavigation.nextSymbol)
      case CommentsIntent.PreviousDocumentComment =>
        navigateDocumentComment(state, DocumentNavigation.previousSymbol)

  private[manager] def interpretNavigation(intent: NavigationIntent, state: AppState): IO[Unit] =
    intent match
      case NavigationIntent.OpenGotoLine =>
        updateState(current => com.serenity.state.reducers.ModalStateReducer.show(Modal.GotoLine(""), current).state)
      case NavigationIntent.ToggleBookmark =>
        toggleBookmark(state)
      case NavigationIntent.NextBookmark =>
        navigateBookmark(state, DocumentNavigation.nextSymbol)
      case NavigationIntent.PreviousBookmark =>
        navigateBookmark(state, DocumentNavigation.previousSymbol)
      case NavigationIntent.NextDocumentSymbol =>
        navigateDocumentSymbol(state, DocumentNavigation.nextSymbol)
      case NavigationIntent.PreviousDocumentSymbol =>
        navigateDocumentSymbol(state, DocumentNavigation.previousSymbol)
      case NavigationIntent.NavigateBack =>
        navigateHistoryBack()
      case NavigationIntent.NavigateForward =>
        navigateHistoryForward()

  private def toggleCommentLens(state: AppState): IO[Unit] =
    state.commentLensSurface match
      case Some(_) =>
        updateState(dismissCommentLens)
      case None =>
        CommentRendering.activeEditorComment(state) match
          case Some(_) =>
            updateState(CommentRendering.openLensAtCursor)
          case None =>
            logger.debug("[CMD] Comment lens requested without an active comment")

  private def dismissCommentLens(state: AppState): AppState =
    state
      .copy(runtime = state.runtime.copy(uiSurfaces = state.runtime.uiSurfaces.filterNot(isCommentLensSurface)))
      .popFocus

  private def isCommentLensSurface(surface: UiSurface): Boolean =
    surface.content match
      case SurfaceContent.CommentLens(_) => true
      case _                             => false

  private def navigateDocumentSymbol(
    state: AppState,
    chooseSymbol: (List[Symbol], CursorPosition) => Option[Symbol]
  ): IO[Unit] =
    navigateSymbols(state, PanelSymbolLookup.outlineSymbolsForBuffer, chooseSymbol, "Document symbol")

  private def navigateBookmark(
    state: AppState,
    chooseSymbol: (List[Symbol], CursorPosition) => Option[Symbol]
  ): IO[Unit] =
    navigateSymbols(
      state,
      buffer => DocumentNavigation.bookmarkSymbols(buffer.annotations.bookmarks),
      chooseSymbol,
      "Bookmark"
    )

  private def navigateDocumentComment(
    state: AppState,
    chooseSymbol: (List[Symbol], CursorPosition) => Option[Symbol]
  ): IO[Unit] =
    navigateSymbols(
      state,
      buffer => DocumentNavigation.commentSymbols(buffer.annotations.documentComments),
      chooseSymbol,
      "Document comment",
      onTargetResolved = Some(CommentRendering.openLensAtCursor)
    )

  private def navigateSymbols(
    state: AppState,
    symbolsForBuffer: Buffer => List[Symbol],
    chooseSymbol: (List[Symbol], CursorPosition) => Option[Symbol],
    label: String,
    onTargetResolved: Option[AppState => AppState] = None
  ): IO[Unit] =
    activeEditorBuffer(state)
      .flatMap {
        case (paneId, buffer) =>
          val cursor  = buffer.editing.cursors.headOption.getOrElse(CursorPosition(0, 0))
          val symbols = symbolsForBuffer(buffer)
          chooseSymbol(symbols, cursor).map { symbol =>
            val before = NavigationPoint(paneId, buffer.id, cursor)
            val after = NavigationPoint(paneId, buffer.id, CursorPosition(symbol.location.line, symbol.location.column))
            before -> after
          }
      } match
      case Some((before, after)) if before != after =>
        val sweep = navigationSweep(before, after)
        stateRef.modify { current =>
          val movedBase = moveToNavigationPoint(current, after)
          val moved = movedBase.copy(runtime =
            movedBase.runtime.copy(navigation =
              NavigationHistory(
                backStack = pushNavigationPoint(before, current.runtime.navigation.backStack),
                forwardStack = Nil
              )
            )
          )
          val animationUpdate = applyNavigationAnimationUpdate(animationUpdateForNavigationTarget(moved, after, sweep))
          (onTargetResolved.fold(moved)(_(moved)), animationUpdate)
        }.flatten
      case Some(_) =>
        onTargetResolved match
          case Some(transform) => updateState(transform)
          case None            => logger.debug(s"[CMD] $label navigation requested for the current location")
      case None =>
        logger.debug(s"[CMD] $label navigation requested without a target")

  private def navigationSweep(before: NavigationPoint, after: NavigationPoint): com.serenity.animation.SweepDirection =
    if after.cursor.line < before.cursor.line ||
        (after.cursor.line == before.cursor.line && after.cursor.column < before.cursor.column)
    then com.serenity.animation.SweepDirection.Backward
    else com.serenity.animation.SweepDirection.Forward

  private def animationUpdateForNavigationTarget(
    state: AppState,
    point: NavigationPoint,
    sweep: com.serenity.animation.SweepDirection
  ): Option[(BufferId, com.serenity.animation.AnimationState => com.serenity.animation.AnimationState)] =
    state.persisted.buffers.get(point.bufferId).flatMap { buffer =>
      val cells = VisibleBufferAnimationCells.fromBuffer(
        buffer,
        state.persisted.config.surfaceConfig.wordWrapEnabled,
        state.persisted.theme.background,
        state.persisted.theme.foreground
      )

      if cells.isEmpty then None
      else
        state.persisted.config.scaledUiAnimation.map { config =>
          val animated = com.serenity.animation.FlowAnimationBuilder.build(
            cells,
            com.serenity.animation.FlowDirection.ByRow,
            sweep,
            config.steps
          )
          val uiAnimations =
            animated.view.mapValues(_.copy(owner = com.serenity.animation.AnimationOwner.UiTransitions)).toMap
          point.bufferId -> ((animations: com.serenity.animation.AnimationState) =>
            animations
              .clear(com.serenity.animation.AnimationOwner.UiTransitions)
              .mergeUiTransitionAnimations(uiAnimations)
          )
        }
    }

  private def applyNavigationAnimationUpdate(
    update: Option[(BufferId, com.serenity.animation.AnimationState => com.serenity.animation.AnimationState)]
  ): IO[Unit] =
    update.fold(IO.unit) {
      case (bufferId, f) =>
        bufferAnimationsRef.update(map =>
          map.updated(bufferId, f(map.getOrElse(bufferId, com.serenity.animation.AnimationState.empty)))
        )
    }

  private def updateNavigationHistory(
    state: AppState,
    target: NavigationPoint,
    backStack: List[NavigationPoint],
    forwardStack: List[NavigationPoint],
    sweep: com.serenity.animation.SweepDirection
  ): (AppState, IO[Unit]) =
    val movedBase = moveToNavigationPoint(state, target)
    val moved = movedBase.copy(runtime =
      movedBase.runtime.copy(navigation = NavigationHistory(backStack = backStack, forwardStack = forwardStack))
    )
    (moved, applyNavigationAnimationUpdate(animationUpdateForNavigationTarget(moved, target, sweep)))

  private def navigateHistoryBack(): IO[Unit] =
    stateRef.modify { current =>
      current.runtime.navigation.backStack match
        case target :: remaining =>
          currentNavigationPoint(current) match
            case Some(point) =>
              updateNavigationHistory(
                current,
                target,
                remaining,
                pushNavigationPoint(point, current.runtime.navigation.forwardStack),
                navigationSweep(point, target)
              )
            case None => (current, IO.unit)
        case Nil => (current, IO.unit)
    }.flatten

  private def navigateHistoryForward(): IO[Unit] =
    stateRef.modify { current =>
      current.runtime.navigation.forwardStack match
        case target :: remaining =>
          currentNavigationPoint(current) match
            case Some(point) =>
              updateNavigationHistory(
                current,
                target,
                pushNavigationPoint(point, current.runtime.navigation.backStack),
                remaining,
                navigationSweep(point, target)
              )
            case None => (current, IO.unit)
        case Nil => (current, IO.unit)
    }.flatten

  private def currentNavigationPoint(state: AppState): Option[NavigationPoint] =
    activeEditorBuffer(state).flatMap {
      case (paneId, buffer) =>
        buffer.editing.cursors.headOption.map(cursor => NavigationPoint(paneId, buffer.id, cursor))
    }

  private def pushNavigationPoint(point: NavigationPoint, stack: List[NavigationPoint]): List[NavigationPoint] =
    stack match
      case head :: _ if head == point => stack
      case _                          => point :: stack

  private def moveToNavigationPoint(state: AppState, point: NavigationPoint): AppState =
    (state.persisted.layout.editorPanes.get(point.paneId), state.persisted.buffers.get(point.bufferId)) match
      case (Some(pane), Some(buffer)) =>
        val viewport = CursorViewport.adjustForCursor(buffer, state, point.cursor)
        val updatedBuffer = buffer.copy(
          editing = buffer.editing.copy(
            cursors = List(point.cursor),
            selection = None,
            selections = Nil,
            preferredColumn = Some(point.cursor.column),
            preferredXPx = None,
            multiCursorVerticalStates = Nil
          ),
          viewport = viewport
        )
        state.copy(persisted =
          state.persisted.copy(
            buffers = state.persisted.buffers + (point.bufferId -> updatedBuffer),
            layout = state.persisted.layout.copy(
              editorPanes =
                state.persisted.layout.editorPanes + (point.paneId -> pane.copy(bufferId = Some(point.bufferId))),
              activeEditorPaneId = Some(point.paneId)
            ),
            focus = Focus.EditorPane(point.paneId)
          )
        )
      case _ => state

  private def toggleBookmark(state: AppState): IO[Unit] =
    activeEditorBuffer(state) match
      case Some((_, buffer)) =>
        val cursor = buffer.editing.cursors.headOption.getOrElse(CursorPosition(0, 0))
        updateState { current =>
          current.persisted.buffers.get(buffer.id) match
            case Some(currentBuffer) =>
              val bookmarks =
                if currentBuffer.annotations.bookmarks.contains(cursor) then
                  currentBuffer.annotations.bookmarks.filterNot(_ == cursor)
                else
                  (cursor :: currentBuffer.annotations.bookmarks).distinct
                    .sortBy(position => (position.line, position.column))

              current.copy(persisted =
                current.persisted.copy(buffers =
                  current.persisted.buffers + (buffer.id ->
                    currentBuffer.copy(annotations = currentBuffer.annotations.copy(bookmarks = bookmarks)))
                )
              )
            case None => current
        }
      case None =>
        logger.debug("[CMD] Toggle bookmark requested without an active editor buffer")

  private def addDocumentComment(state: AppState, text: String): IO[Unit] =
    activeEditorBuffer(state) match
      case Some((_, buffer)) =>
        val cursor           = buffer.editing.cursors.headOption.getOrElse(CursorPosition(0, 0))
        val normalizedCursor = snapCursorAfterGrapheme(buffer, cursor)
        val range = buffer.primarySelection
          .map(selection => normalizedCommentSelectionRange(buffer, selection))
          .getOrElse(normalizedCursor -> normalizedCursor)
        val commentText = Option(text.trim).filter(_.nonEmpty).getOrElse("Comment")
        val comment     = DocumentComment(range._1, range._2, commentText)
        updateState: current =>
          current.persisted.buffers.get(buffer.id) match
            case Some(currentBuffer) =>
              val existingCommentAtCursor =
                currentBuffer.annotations.documentComments.find(_.contains(normalizedCursor))
              val updatedComment = existingCommentAtCursor
                .map(existing => existing.copy(text = commentText))
                .getOrElse(comment)
              val comments = (updatedComment :: currentBuffer.annotations.documentComments.filterNot(existing =>
                existingCommentAtCursor.contains(existing) ||
                  (existing.start == comment.start && existing.end == comment.end)
              )).sortBy(existing => (existing.start.line, existing.start.column, existing.text))
              current.copy(persisted =
                current.persisted.copy(buffers =
                  current.persisted.buffers + (buffer.id ->
                    currentBuffer.copy(
                      annotations = currentBuffer.annotations.copy(documentComments = comments),
                      document = currentBuffer.document.copy(isDirty = true)
                    ))
                )
              )
            case None => current
      case None =>
        logger.debug("[CMD] Add document comment requested without an active editor buffer")

  private def normalizedCommentSelectionRange(
    buffer: Buffer,
    selection: Selection
  ): (CursorPosition, CursorPosition) =
    val startOffset = buffer.document.content.lineColumnToOffset(selection.start.line, selection.start.column)
    val endOffset   = buffer.document.content.lineColumnToOffset(selection.end.line, selection.end.column)
    if startOffset >= endOffset then
      val cursor =
        buffer.document.content.offsetToCursorPosition(buffer.document.content.graphemeBoundaryAfterOrAt(startOffset))
      cursor -> cursor
    else
      buffer.document.content.offsetToCursorPosition(
        buffer.document.content.graphemeBoundaryBeforeOrAt(startOffset)
      ) ->
        buffer.document.content.offsetToCursorPosition(buffer.document.content.graphemeBoundaryAfterOrAt(endOffset))

  private def snapCursorAfterGrapheme(buffer: Buffer, cursor: CursorPosition): CursorPosition =
    val offset = buffer.document.content.lineColumnToOffset(cursor.line, cursor.column)
    buffer.document.content.offsetToCursorPosition(buffer.document.content.graphemeBoundaryAfterOrAt(offset))

  private def deleteDocumentComment(state: AppState): IO[Unit] =
    activeEditorBuffer(state) match
      case Some((_, buffer)) =>
        val cursor = buffer.editing.cursors.headOption.getOrElse(CursorPosition(0, 0))
        updateState: current =>
          current.persisted.buffers.get(buffer.id) match
            case Some(currentBuffer) =>
              val comments = currentBuffer.annotations.documentComments.filterNot(_.contains(cursor))
              current.copy(persisted =
                current.persisted.copy(buffers =
                  current.persisted.buffers + (buffer.id ->
                    currentBuffer.copy(
                      annotations = currentBuffer.annotations.copy(documentComments = comments),
                      document = currentBuffer.document.copy(
                        isDirty =
                          currentBuffer.document.isDirty || comments != currentBuffer.annotations.documentComments
                      )
                    ))
                )
              )
            case None => current
      case None =>
        logger.debug("[CMD] Delete document comment requested without an active editor buffer")

  private[manager] def activeEditorBuffer(state: AppState): Option[(PaneId, Buffer)] =
    for
      paneId   <- state.persisted.layout.activeEditorPaneId
      pane     <- state.persisted.layout.editorPanes.get(paneId)
      bufferId <- pane.bufferId
      buffer   <- state.persisted.buffers.get(bufferId)
    yield (paneId, buffer)
