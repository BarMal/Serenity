package com.serenity.state.reducers

import com.serenity.keystroke.events.*
import com.serenity.rope.*
import com.serenity.state.models.*

/** Caret movement, paging, select-all -- the family that repositions cursors/selection without touching document
  * content. Split out of `EditorEventReducer.reduceCursorsEditEvent`'s sibling dispatch when that file grew past its
  * 600-line target.
  */
private[reducers] object EditorNavigationEventReducer:
  import EditorCursorMovement.*
  import EditorCursorSupport.{CursorEventContext, countLines, findLineEnd, homeTarget, endTarget}

  private val OriginCursor = CursorPosition(0, 0)

  /** One path over `buffer.cursorList` (see its doc comment) handles single cursor, multi-cursor and multi-selection
    * alike. Movement (`MoveLeft`/`MoveRight`/`MoveWordLeft`/`MoveWordRight`/`MoveToStart`/`MoveToEnd`/
    * `MoveToStartOfFile`/`PageUp`/`PageDown`) turned out to have no genuine per-cardinality difference at all: the old
    * single-cursor bodies and `applyMultiCursorNavigation`/`applyMultiCursorPageNavigation` compute identically-shaped
    * results for one cursor, so all of them now share the one multi-cursor-shaped implementation (selections collapse
    * to their focus first, exactly as the old multi-selection path already did for the subset of these events it
    * covered). `MoveToEndOfFile` keeps its own single-cursor body: it scrolls the viewport to keep the new cursor
    * visible where the multi-cursor form does not, and untangling whether that asymmetry is deliberate is outside this
    * change's scope.
    */
  def reduce(event: TextEntryEvent, ctx: CursorEventContext): ReducerResult =
    import ctx.*

    def applyBuffer(f: Buffer => Buffer): ReducerResult =
      ReducerResult.noEffects(Focused.replaceBuffer(currentState, f(buffer)))

    def navigateWithAffinity(moveFn: CursorPosition => CursorPosition): ReducerResult =
      applyBuffer(target =>
        applyMultiCursorNavigation(if hasSelection then collapseSelectionsToFocus(target) else target)(moveFn)
      )

    // Landing on the column where one wrapped row ends and the next begins means arriving at the start of the later
    // row -- for every movement but End, which means the end of the row the cursor was already on. Normalising here
    // rather than in each `moveFn` keeps an upstream affinity from outliving the End that set it, whatever route the
    // cursor takes away from that column afterwards.
    def navigate(moveFn: CursorPosition => CursorPosition): ReducerResult =
      navigateWithAffinity(cursor => moveFn(cursor).downstream)

    event match
      case MoveLeft  => navigate(cursor => moveCursorLeft(cursor, buffer.document.content))
      case MoveRight => navigate(cursor => moveCursorRight(cursor, buffer.document.content))
      case MoveWordLeft =>
        navigate(cursor => wordBoundaryFrom(buffer, cursor, (rope, offset) => rope.previousWordBoundary(offset)))
      case MoveWordRight =>
        navigate(cursor => wordBoundaryFrom(buffer, cursor, (rope, offset) => rope.nextWordBoundary(offset)))
      case MoveToStart       => navigate(cursor => homeTarget(currentState, paneId, cursor))
      case MoveToEnd         => navigateWithAffinity(cursor => endTarget(currentState, paneId, buffer, cursor))
      case MoveToStartOfFile => navigate(_ => OriginCursor)

      case PageUp   => applyBuffer(target => pageNavigate(target, ctx, direction = -1))
      case PageDown => applyBuffer(target => pageNavigate(target, ctx, direction = 1))

      case MoveToEndOfFile => reduceMoveToEndOfFile(ctx, applyBuffer, navigate)
      case SelectAll       => reduceSelectAll(ctx)

      case _ =>
        ReducerResult.noEffects(currentState)

  private def pageNavigate(target: Buffer, ctx: CursorEventContext, direction: Int): Buffer =
    import ctx.*
    applyMultiCursorPageNavigation(
      if hasSelection then collapseSelectionsToFocus(target) else target,
      currentState,
      paneId,
      direction
    )

  /** `MoveToEndOfFile` keeps its own single-cursor body: it scrolls the viewport to keep the new cursor visible where
    * the multi-cursor form does not, and untangling whether that asymmetry is deliberate is outside this change's
    * scope.
    */
  private def reduceMoveToEndOfFile(
    ctx: CursorEventContext,
    applyBuffer: (Buffer => Buffer) => ReducerResult,
    navigate: (CursorPosition => CursorPosition) => ReducerResult
  ): ReducerResult =
    import ctx.*
    val totalLines  = countLines(buffer.document.content)
    val lastLine    = totalLines - 1
    val lastLineEnd = findLineEnd(buffer.document.content, lastLine)

    if !hasSelection && !isMulti then
      val newCursor = CursorPosition(lastLine, lastLineEnd)
      // No viewport here either, for the reasons `applyMultiCursorPageNavigation` documents: a top line of
      // `lastLine - visibleLines + 1` counts logical lines against a screen of visual rows, and the effect boundary
      // recomputes it correctly straight afterwards regardless.
      applyBuffer(
        _.copy(
          editing = buffer.editing.copy(
            cursors = List(newCursor),
            preferredColumn = Some(newCursor.column),
            preferredXPx = None
          )
        )
      )
    else navigate(_ => CursorPosition(lastLine, lastLineEnd))

  private def reduceSelectAll(ctx: CursorEventContext): ReducerResult =
    import ctx.*
    ReducerResult.fromTransition(
      currentState,
      Focused.modifyBufferWithId(buffer.id) { current =>
        val lastLine  = math.max(0, countLines(current.document.content) - 1)
        val endCursor = CursorPosition(lastLine, findLineEnd(current.document.content, lastLine))
        current.copy(
          editing = current.editing.copy(
            cursors = List(endCursor),
            selection = Some(Selection(CursorPosition(0, 0), endCursor)),
            selections = Nil,
            preferredColumn = Some(endCursor.column),
            preferredXPx = None
          )
        )
      }
    )

  private def applyMultiCursorNavigation(
    buffer: Buffer
  )(move: CursorPosition => CursorPosition): Buffer =
    val finalCursors = buffer.editing.cursors
      .map(move)
      .distinct
      .sortBy(cursor => (cursor.line, cursor.column))
    val primaryCursor = finalCursors.primaryCursor
    val baseBuffer = buffer.copy(
      editing = buffer.editing.copy(
        cursors = finalCursors,
        selection = None,
        selections = Nil,
        preferredColumn = Some(primaryCursor.column),
        preferredXPx = None,
        multiCursorVerticalStates = Nil
      )
    )
    baseBuffer

  private def applyMultiCursorPageNavigation(
    buffer: Buffer,
    currentState: AppState,
    paneId: PaneId,
    direction: Int
  ): Buffer =
    val move = pageTarget(buffer, currentState, paneId, direction)

    val finalCursors = buffer.editing.cursors
      .map(move)
      .distinct
      .sortBy(cursor => (cursor.line, cursor.column))
    val primaryCursor = finalCursors.primaryCursor
    buffer.copy(
      editing = buffer.editing.copy(
        cursors = finalCursors,
        selection = None,
        selections = Nil,
        preferredColumn = Some(primaryCursor.column),
        preferredXPx = None,
        multiCursorVerticalStates = Nil
      )
    )
