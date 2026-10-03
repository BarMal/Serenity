package com.serenity.state.manager

import com.serenity.command.{CommentsIntent, NavigationIntent, PlaceholderIntent}
import com.serenity.document.{CommentRendering, DocumentNavigation}
import com.serenity.rope.*
import com.serenity.state.models.*
import com.serenity.state.reducers.{ModalStateReducer, ReducerResult}
import com.serenity.ui.layout.{Symbol, WrappedLineCache}

private[manager] enum NavigationOutcome:
  case Applied(result: ReducerResult)

  /** Nothing to do; `debugLog` is what the shell logs about it, if anything. */
  case Ignored(debugLog: Option[String])

/** Document navigation (bookmarks, symbols, comments, back/forward history) and the comment-lens/annotation mutations
  * that go with it, as pure functions of the current state.
  */
private[manager] object NavigationTransitions:

  private type SymbolChooser = (List[Symbol], CursorPosition) => Option[Symbol]

  def comments(
    intent: CommentsIntent,
    state: AppState,
    wrapCache: WrappedLineCache = WrappedLineCache.Uncached
  ): NavigationOutcome =
    given WrappedLineCache = wrapCache
    intent match
      case CommentsIntent.ToggleCommentLens        => toggleCommentLens(state)
      case CommentsIntent.AddDocumentComment(text) => addDocumentComment(state, text)
      case CommentsIntent.DeleteDocumentComment    => deleteDocumentComment(state)
      case CommentsIntent.NextDocumentComment      => navigateDocumentComment(state, DocumentNavigation.nextSymbol)
      case CommentsIntent.PreviousDocumentComment  => navigateDocumentComment(state, DocumentNavigation.previousSymbol)

  def placeholders(
    intent: PlaceholderIntent,
    state: AppState,
    wrapCache: WrappedLineCache = WrappedLineCache.Uncached
  ): NavigationOutcome =
    given WrappedLineCache = wrapCache
    intent match
      case PlaceholderIntent.AddPlaceholder(note) => addPlaceholder(state, note)
      case PlaceholderIntent.DeletePlaceholder    => deletePlaceholder(state)
      case PlaceholderIntent.NextPlaceholder      => navigatePlaceholder(state, DocumentNavigation.nextSymbol)
      case PlaceholderIntent.PreviousPlaceholder  => navigatePlaceholder(state, DocumentNavigation.previousSymbol)

  def navigation(
    intent: NavigationIntent,
    state: AppState,
    wrapCache: WrappedLineCache = WrappedLineCache.Uncached
  ): NavigationOutcome =
    given WrappedLineCache = wrapCache
    intent match
      case NavigationIntent.OpenGotoLine =>
        NavigationOutcome.Applied(ModalStateReducer.show(Modal.TextPrompt(TextPrompt.gotoLine()), state))
      case NavigationIntent.ToggleBookmark         => toggleBookmark(state)
      case NavigationIntent.NextBookmark           => navigateBookmark(state, DocumentNavigation.nextSymbol)
      case NavigationIntent.PreviousBookmark       => navigateBookmark(state, DocumentNavigation.previousSymbol)
      case NavigationIntent.NextDocumentSymbol     => navigateDocumentSymbol(state, DocumentNavigation.nextSymbol)
      case NavigationIntent.PreviousDocumentSymbol => navigateDocumentSymbol(state, DocumentNavigation.previousSymbol)
      case NavigationIntent.NavigateBack           => navigateHistoryBack(state)
      case NavigationIntent.NavigateForward        => navigateHistoryForward(state)
      case NavigationIntent.GoToBufferLine(bufferId, line) => goToBufferLine(state, bufferId, line)

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

  private def navigateDocumentSymbol(state: AppState, chooseSymbol: SymbolChooser)(using
    WrappedLineCache
  ): NavigationOutcome =
    navigateSymbols(state, PanelSymbolLookup.outlineSymbolsForBuffer, chooseSymbol, "Document symbol")

  private def navigateBookmark(state: AppState, chooseSymbol: SymbolChooser)(using
    WrappedLineCache
  ): NavigationOutcome =
    navigateSymbols(
      state,
      buffer => DocumentNavigation.bookmarkSymbols(buffer.annotations.bookmarks),
      chooseSymbol,
      "Bookmark"
    )

  private def navigateDocumentComment(state: AppState, chooseSymbol: SymbolChooser)(using
    WrappedLineCache
  ): NavigationOutcome =
    navigateSymbols(
      state,
      buffer => DocumentNavigation.commentSymbols(buffer.annotations.documentComments),
      chooseSymbol,
      "Document comment",
      onTargetResolved = Some(CommentRendering.openLensAtCursor)
    )

  private def navigatePlaceholder(state: AppState, chooseSymbol: SymbolChooser)(using
    WrappedLineCache
  ): NavigationOutcome =
    navigateSymbols(
      state,
      buffer => DocumentNavigation.placeholderSymbols(buffer.annotations.placeholders),
      chooseSymbol,
      "Placeholder"
    )

  private def navigateSymbols(
    state: AppState,
    symbolsForBuffer: Buffer => List[Symbol],
    chooseSymbol: SymbolChooser,
    label: String,
    onTargetResolved: Option[AppState => AppState] = None
  )(using WrappedLineCache): NavigationOutcome =
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
          ReducerResult.noEffects(onTargetResolved.fold(moved)(_(moved)))
        )
      case Some(_) =>
        onTargetResolved match
          case Some(transform) => applied(transform(state))
          case None            => ignored(s"[CMD] $label navigation requested for the current location")
      case None =>
        ignored(s"[CMD] $label navigation requested without a target")

  private def navigateHistoryBack(state: AppState)(using WrappedLineCache): NavigationOutcome =
    (state.runtime.navigation.backStack, currentNavigationPoint(state)) match
      case (target :: remaining, Some(point)) =>
        jumpThroughHistory(
          state,
          target,
          backStack = remaining,
          forwardStack = pushNavigationPoint(point, state.runtime.navigation.forwardStack)
        )
      case _ => NavigationOutcome.Ignored(None)

  private def navigateHistoryForward(state: AppState)(using WrappedLineCache): NavigationOutcome =
    (state.runtime.navigation.forwardStack, currentNavigationPoint(state)) match
      case (target :: remaining, Some(point)) =>
        jumpThroughHistory(
          state,
          target,
          backStack = pushNavigationPoint(point, state.runtime.navigation.backStack),
          forwardStack = remaining
        )
      case _ => NavigationOutcome.Ignored(None)

  /** Puts the cursor at the start of `line` (the last line, if past the end) in the pane already showing the buffer,
    * else in the active pane, and focuses that pane. Where it came from goes on the back stack, as for any other jump.
    */
  private def goToBufferLine(state: AppState, bufferId: BufferId, line: Int)(using
    WrappedLineCache
  ): NavigationOutcome =
    val layout = state.persisted.layout
    val showing = (layout.activeEditorPaneId.toList ++ layout.orderedPaneIds)
      .find(paneId => layout.editorPanes.get(paneId).exists(_.bufferId.contains(bufferId)))
    (state.persisted.buffers.get(bufferId), showing.orElse(layout.activeEditorPaneId)) match
      case (None, _) => ignored("[CMD] Go to line requested for a buffer that is no longer open")
      case (_, None) => ignored("[CMD] Go to line requested without an editor pane")
      case (Some(buffer), Some(paneId)) =>
        val target =
          NavigationPoint(paneId, bufferId, CursorPosition(line.max(0).min(buffer.document.content.newlineCount), 0))
        val moved = moveToNavigationPoint(state, target)
        currentNavigationPoint(state).filter(_ != target) match
          case Some(origin) =>
            val recorded =
              withHistory(moved, pushNavigationPoint(origin, state.runtime.navigation.backStack), forwardStack = Nil)
            NavigationOutcome.Applied(
              ReducerResult.noEffects(recorded)
            )
          case None => applied(moved)

  private def jumpThroughHistory(
    state: AppState,
    target: NavigationPoint,
    backStack: List[NavigationPoint],
    forwardStack: List[NavigationPoint]
  )(using WrappedLineCache): NavigationOutcome =
    val moved = withHistory(moveToNavigationPoint(state, target), backStack, forwardStack)
    NavigationOutcome.Applied(ReducerResult.noEffects(moved))

  private def withHistory(
    state: AppState,
    backStack: List[NavigationPoint],
    forwardStack: List[NavigationPoint]
  ): AppState =
    state.copy(runtime = state.runtime.copy(navigation = NavigationHistory(backStack, forwardStack)))

  private def currentNavigationPoint(state: AppState): Option[NavigationPoint] =
    activeEditorBuffer(state).flatMap {
      case (paneId, buffer) =>
        buffer.editing.cursorPositions.headOption.map(cursor => NavigationPoint(paneId, buffer.id, cursor))
    }

  private def pushNavigationPoint(point: NavigationPoint, stack: List[NavigationPoint]): List[NavigationPoint] =
    stack match
      case head :: _ if head == point => stack
      case _                          => point :: stack

  private def moveToNavigationPoint(state: AppState, point: NavigationPoint)(using
    wrapCache: WrappedLineCache
  ): AppState =
    (state.persisted.layout.editorPanes.get(point.paneId), state.persisted.buffers.get(point.bufferId)) match
      case (Some(pane), Some(buffer)) =>
        val viewport = CursorViewport.adjustForCursor(buffer, state, point.cursor, wrapCache = wrapCache)
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

  private def addPlaceholder(state: AppState, note: String): NavigationOutcome =
    activeEditorBuffer(state) match
      case Some((_, buffer)) =>
        val cursor               = snapCursorAfterGrapheme(buffer, primaryCursor(buffer))
        val placeholderNote      = Option(note.trim).filter(_.nonEmpty).getOrElse("TODO")
        val existingPlaceholders = buffer.annotations.placeholders
        val updated =
          (Placeholder(cursor, placeholderNote) :: existingPlaceholders.filterNot(_.position == cursor))
            .sortBy(placeholder => (placeholder.position.line, placeholder.position.column))
        applied(
          withBuffer(
            state,
            buffer.copy(
              annotations = buffer.annotations.copy(placeholders = updated),
              document = buffer.document.copy(isDirty = true)
            )
          )
        )
      case None =>
        ignored("[CMD] Add placeholder requested without an active editor buffer")

  private def deletePlaceholder(state: AppState): NavigationOutcome =
    activeEditorBuffer(state) match
      case Some((_, buffer)) =>
        val cursor       = primaryCursor(buffer)
        val placeholders = buffer.annotations.placeholders.filterNot(_.position == cursor)
        applied(
          withBuffer(
            state,
            buffer.copy(
              annotations = buffer.annotations.copy(placeholders = placeholders),
              document = buffer.document.copy(
                isDirty = buffer.document.isDirty || placeholders != buffer.annotations.placeholders
              )
            )
          )
        )
      case None =>
        ignored("[CMD] Delete placeholder requested without an active editor buffer")

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
