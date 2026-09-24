package com.serenity.state.manager

import com.serenity.animation.{AnimationOwner, FlowAnimationBuilder, FlowDirection, SweepDirection}
import com.serenity.command.{CommentsIntent, NavigationIntent}
import com.serenity.config.AppConfigMotionOps.*
import com.serenity.document.{CommentRendering, DocumentNavigation}
import com.serenity.rope.*
import com.serenity.state.models.*
import com.serenity.state.reducers.{AnimationEffect, AppEffect, ModalStateReducer, ReducerResult}
import com.serenity.ui.layout.Symbol

private[manager] enum NavigationOutcome:
  case Applied(result: ReducerResult)

  /** Nothing to do; `debugLog` is what the shell logs about it, if anything. */
  case Ignored(debugLog: Option[String])

/** Document navigation (bookmarks, symbols, comments, back/forward history) and the comment-lens/annotation mutations
  * that go with it, as pure functions of the current state.
  */
private[manager] object NavigationTransitions:

  private type SymbolChooser = (List[Symbol], CursorPosition) => Option[Symbol]

  def comments(intent: CommentsIntent, state: AppState): NavigationOutcome =
    intent match
      case CommentsIntent.ToggleCommentLens        => toggleCommentLens(state)
      case CommentsIntent.AddDocumentComment(text) => addDocumentComment(state, text)
      case CommentsIntent.DeleteDocumentComment    => deleteDocumentComment(state)
      case CommentsIntent.NextDocumentComment      => navigateDocumentComment(state, DocumentNavigation.nextSymbol)
      case CommentsIntent.PreviousDocumentComment  => navigateDocumentComment(state, DocumentNavigation.previousSymbol)

  def navigation(intent: NavigationIntent, state: AppState): NavigationOutcome =
    intent match
      case NavigationIntent.OpenGotoLine =>
        NavigationOutcome.Applied(ModalStateReducer.show(Modal.GotoLine(""), state))
      case NavigationIntent.ToggleBookmark         => toggleBookmark(state)
      case NavigationIntent.NextBookmark           => navigateBookmark(state, DocumentNavigation.nextSymbol)
      case NavigationIntent.PreviousBookmark       => navigateBookmark(state, DocumentNavigation.previousSymbol)
      case NavigationIntent.NextDocumentSymbol     => navigateDocumentSymbol(state, DocumentNavigation.nextSymbol)
      case NavigationIntent.PreviousDocumentSymbol => navigateDocumentSymbol(state, DocumentNavigation.previousSymbol)
      case NavigationIntent.NavigateBack           => navigateHistoryBack(state)
      case NavigationIntent.NavigateForward        => navigateHistoryForward(state)

  private def applied(state: AppState): NavigationOutcome =
    NavigationOutcome.Applied(ReducerResult.noEffects(state))

  private def ignored(debugLog: String): NavigationOutcome =
    NavigationOutcome.Ignored(Some(debugLog))

  private def toggleCommentLens(state: AppState): NavigationOutcome =
    state.commentLensSurface match
      case Some(_) => applied(dismissCommentLens(state))
      case None =>
        CommentRendering.activeEditorComment(state) match
          case Some(_) => applied(CommentRendering.openLensAtCursor(state))
          case None    => ignored("[CMD] Comment lens requested without an active comment")

  private def dismissCommentLens(state: AppState): AppState =
    state
      .copy(runtime = state.runtime.copy(uiSurfaces = state.runtime.uiSurfaces.filterNot(isCommentLensSurface)))
      .popFocus

  private def isCommentLensSurface(surface: UiSurface): Boolean =
    surface.content match
      case SurfaceContent.CommentLens(_) => true
      case _                             => false

  private def navigateDocumentSymbol(state: AppState, chooseSymbol: SymbolChooser): NavigationOutcome =
    navigateSymbols(state, PanelSymbolLookup.outlineSymbolsForBuffer, chooseSymbol, "Document symbol")

  private def navigateBookmark(state: AppState, chooseSymbol: SymbolChooser): NavigationOutcome =
    navigateSymbols(
      state,
      buffer => DocumentNavigation.bookmarkSymbols(buffer.annotations.bookmarks),
      chooseSymbol,
      "Bookmark"
    )

  private def navigateDocumentComment(state: AppState, chooseSymbol: SymbolChooser): NavigationOutcome =
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
    chooseSymbol: SymbolChooser,
    label: String,
    onTargetResolved: Option[AppState => AppState] = None
  ): NavigationOutcome =
    val jump = activeEditorBuffer(state).flatMap {
      case (paneId, buffer) =>
        val cursor = primaryCursor(buffer)
        chooseSymbol(symbolsForBuffer(buffer), cursor).map { symbol =>
          NavigationPoint(paneId, buffer.id, cursor) ->
            NavigationPoint(paneId, buffer.id, CursorPosition(symbol.location.line, symbol.location.column))
        }
    }
    jump match
      case Some((before, after)) if before != after =>
        val moved = withHistory(
          moveToNavigationPoint(state, after),
          backStack = pushNavigationPoint(before, state.runtime.navigation.backStack),
          forwardStack = Nil
        )
        NavigationOutcome.Applied(
          ReducerResult(onTargetResolved.fold(moved)(_(moved)), uiTransitionSweep(moved, after, sweep(before, after)))
        )
      case Some(_) =>
        onTargetResolved match
          case Some(transform) => applied(transform(state))
          case None            => ignored(s"[CMD] $label navigation requested for the current location")
      case None =>
        ignored(s"[CMD] $label navigation requested without a target")

  private def navigateHistoryBack(state: AppState): NavigationOutcome =
    (state.runtime.navigation.backStack, currentNavigationPoint(state)) match
      case (target :: remaining, Some(point)) =>
        jumpThroughHistory(
          state,
          point,
          target,
          backStack = remaining,
          forwardStack = pushNavigationPoint(point, state.runtime.navigation.forwardStack)
        )
      case _ => NavigationOutcome.Ignored(None)

  private def navigateHistoryForward(state: AppState): NavigationOutcome =
    (state.runtime.navigation.forwardStack, currentNavigationPoint(state)) match
      case (target :: remaining, Some(point)) =>
        jumpThroughHistory(
          state,
          point,
          target,
          backStack = pushNavigationPoint(point, state.runtime.navigation.backStack),
          forwardStack = remaining
        )
      case _ => NavigationOutcome.Ignored(None)

  private def jumpThroughHistory(
    state: AppState,
    from: NavigationPoint,
    target: NavigationPoint,
    backStack: List[NavigationPoint],
    forwardStack: List[NavigationPoint]
  ): NavigationOutcome =
    val moved = withHistory(moveToNavigationPoint(state, target), backStack, forwardStack)
    NavigationOutcome.Applied(ReducerResult(moved, uiTransitionSweep(moved, target, sweep(from, target))))

  private def withHistory(
    state: AppState,
    backStack: List[NavigationPoint],
    forwardStack: List[NavigationPoint]
  ): AppState =
    state.copy(runtime = state.runtime.copy(navigation = NavigationHistory(backStack, forwardStack)))

  private def sweep(before: NavigationPoint, after: NavigationPoint): SweepDirection =
    if after.cursor.line < before.cursor.line ||
        (after.cursor.line == before.cursor.line && after.cursor.column < before.cursor.column)
    then SweepDirection.Backward
    else SweepDirection.Forward

  private def uiTransitionSweep(state: AppState, point: NavigationPoint, sweep: SweepDirection): List[AppEffect] =
    state.persisted.buffers.get(point.bufferId).toList.flatMap { buffer =>
      val cells = VisibleBufferAnimationCells.fromBuffer(
        buffer,
        state.persisted.config.surfaceConfig.wordWrapEnabled,
        state.persisted.theme.background,
        state.persisted.theme.foreground
      )
      if cells.isEmpty then Nil
      else
        state.persisted.config.scaledUiAnimation.toList.map { config =>
          val animated = FlowAnimationBuilder.build(cells, FlowDirection.ByRow, sweep, config.steps)
          val uiCells  = animated.view.mapValues(_.copy(owner = AnimationOwner.UiTransitions)).toMap
          AppEffect.Animation(AnimationEffect.RestartUiTransitions(point.bufferId, uiCells))
        }
    }

  private def currentNavigationPoint(state: AppState): Option[NavigationPoint] =
    activeEditorBuffer(state).flatMap {
      case (paneId, buffer) =>
        buffer.editing.cursorPositions.headOption.map(cursor => NavigationPoint(paneId, buffer.id, cursor))
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
          editing = EditingState(List(point.cursor)),
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

  private def toggleBookmark(state: AppState): NavigationOutcome =
    activeEditorBuffer(state) match
      case Some((_, buffer)) =>
        val cursor = primaryCursor(buffer)
        val bookmarks =
          if buffer.annotations.bookmarks.contains(cursor) then buffer.annotations.bookmarks.filterNot(_ == cursor)
          else (cursor :: buffer.annotations.bookmarks).distinct.sortBy(position => (position.line, position.column))
        applied(withBuffer(state, buffer.copy(annotations = buffer.annotations.copy(bookmarks = bookmarks))))
      case None =>
        ignored("[CMD] Toggle bookmark requested without an active editor buffer")

  private def addDocumentComment(state: AppState, text: String): NavigationOutcome =
    activeEditorBuffer(state) match
      case Some((_, buffer)) =>
        val normalizedCursor = snapCursorAfterGrapheme(buffer, primaryCursor(buffer))
        val (start, end) = buffer.primarySelection
          .map(selection => normalizedCommentSelectionRange(buffer, selection))
          .getOrElse(normalizedCursor -> normalizedCursor)
        val commentText             = Option(text.trim).filter(_.nonEmpty).getOrElse("Comment")
        val existingComments        = buffer.annotations.documentComments
        val existingCommentAtCursor = existingComments.find(_.contains(normalizedCursor))
        val updatedComment =
          existingCommentAtCursor.fold(DocumentComment(start, end, commentText))(_.copy(text = commentText))
        val comments = (updatedComment :: existingComments.filterNot(existing =>
          existingCommentAtCursor.contains(existing) || (existing.start == start && existing.end == end)
        )).sortBy(existing => (existing.start.line, existing.start.column, existing.text))
        applied(
          withBuffer(
            state,
            buffer.copy(
              annotations = buffer.annotations.copy(documentComments = comments),
              document = buffer.document.copy(isDirty = true)
            )
          )
        )
      case None =>
        ignored("[CMD] Add document comment requested without an active editor buffer")

  private def normalizedCommentSelectionRange(
    buffer: Buffer,
    selection: Selection
  ): (CursorPosition, CursorPosition) =
    val content     = buffer.document.content
    val startOffset = content.lineColumnToOffset(selection.start.line, selection.start.column)
    val endOffset   = content.lineColumnToOffset(selection.end.line, selection.end.column)
    if startOffset >= endOffset then
      val cursor = content.offsetToCursorPosition(content.graphemeBoundaryAfterOrAt(startOffset))
      cursor -> cursor
    else
      content.offsetToCursorPosition(content.graphemeBoundaryBeforeOrAt(startOffset)) ->
        content.offsetToCursorPosition(content.graphemeBoundaryAfterOrAt(endOffset))

  private def snapCursorAfterGrapheme(buffer: Buffer, cursor: CursorPosition): CursorPosition =
    val content = buffer.document.content
    content.offsetToCursorPosition(
      content.graphemeBoundaryAfterOrAt(content.lineColumnToOffset(cursor.line, cursor.column))
    )

  private def deleteDocumentComment(state: AppState): NavigationOutcome =
    activeEditorBuffer(state) match
      case Some((_, buffer)) =>
        val cursor   = primaryCursor(buffer)
        val comments = buffer.annotations.documentComments.filterNot(_.contains(cursor))
        applied(
          withBuffer(
            state,
            buffer.copy(
              annotations = buffer.annotations.copy(documentComments = comments),
              document = buffer.document.copy(
                isDirty = buffer.document.isDirty || comments != buffer.annotations.documentComments
              )
            )
          )
        )
      case None =>
        ignored("[CMD] Delete document comment requested without an active editor buffer")

  private def withBuffer(state: AppState, buffer: Buffer): AppState =
    state.copy(persisted = state.persisted.copy(buffers = state.persisted.buffers + (buffer.id -> buffer)))

  private def primaryCursor(buffer: Buffer): CursorPosition =
    buffer.editing.cursorPositions.headOption.getOrElse(CursorPosition(0, 0))

  private def activeEditorBuffer(state: AppState): Option[(PaneId, Buffer)] =
    for
      paneId   <- state.persisted.layout.activeEditorPaneId
      pane     <- state.persisted.layout.editorPanes.get(paneId)
      bufferId <- pane.bufferId
      buffer   <- state.persisted.buffers.get(bufferId)
    yield (paneId, buffer)
