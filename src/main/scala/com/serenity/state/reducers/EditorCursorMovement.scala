package com.serenity.state.reducers

import com.serenity.rope.*
import com.serenity.state.models.*

/** Cursor/selection queries and single-cursor movement targets shared across every family of [[EditorEventReducer]]
  * event handling -- horizontal, page and vertical movement, and selection extension all resolve "where does one cursor
  * land" the same way and then differ only in what they do with the answer.
  */
private[reducers] object EditorCursorMovement:

  /** Replaces the primary (first) cursor while leaving every other live cursor untouched. */
  def replacePrimaryCursor(newPrimary: CursorPosition, cursors: List[CursorPosition]): List[CursorPosition] =
    cursors match
      case _ :: rest => newPrimary :: rest
      case Nil       => List(newPrimary)

  /** Since `#1577`, each cursor carries its own preferred-column/x state directly, so there is nothing left to clear
    * that a fresh primary-cursor write (`EditingState.withPrimary`) doesn't already replace on its own. Kept as a
    * named no-op so call sites that clear stale multi-cursor vertical state ahead of an event that might turn
    * genuinely multi-cursor again don't need to know that distinction disappeared.
    */
  def clearInFlightMultiCursorVerticalState(buffer: Buffer): Buffer = buffer

  /** Where a movement key lands, plus the column and measured x-offset a later vertical move should resume from.
    * Movement and shift-movement compute this identically and differ only in what they do with it.
    */
  final case class CursorTarget(cursor: CursorPosition, preferredColumn: Int, preferredXPx: Option[Float])

  def horizontalTarget(landed: CursorPosition): CursorTarget =
    CursorTarget(landed, landed.column, preferredXPx = None)

  def moveCursorLeft(cursor: CursorPosition, content: Rope): CursorPosition =
    val offset = content.lineColumnToOffset(cursor.line, cursor.column)
    val target = content.previousGraphemeBoundary(offset)
    if target < offset then content.offsetToCursorPosition(target)
    else cursor

  def moveCursorRight(cursor: CursorPosition, content: Rope): CursorPosition =
    val offset = content.lineColumnToOffset(cursor.line, cursor.column)
    val target = content.nextGraphemeBoundary(offset)
    if offset < target then content.offsetToCursorPosition(target)
    else cursor

  def wordBoundaryFrom(buffer: Buffer, from: CursorPosition, boundary: (Rope, Int) => Int): CursorPosition =
    val offset = buffer.document.content.lineColumnToOffset(from.line, from.column)
    buffer.document.content.offsetToCursorPosition(boundary(buffer.document.content, offset))

  def leftTarget(buffer: Buffer, from: CursorPosition): CursorTarget =
    horizontalTarget(moveCursorLeft(from, buffer.document.content))

  def rightTarget(buffer: Buffer, from: CursorPosition): CursorTarget =
    horizontalTarget(moveCursorRight(from, buffer.document.content))

  def wordLeftTarget(buffer: Buffer, from: CursorPosition): CursorTarget =
    horizontalTarget(wordBoundaryFrom(buffer, from, (rope, offset) => rope.previousWordBoundary(offset)))

  def wordRightTarget(buffer: Buffer, from: CursorPosition): CursorTarget =
    horizontalTarget(wordBoundaryFrom(buffer, from, (rope, offset) => rope.nextWordBoundary(offset)))

  /** `from` is passed rather than derived: arrow keys resume from the selection focus so a right-arrow off a selection
    * lands past its end, while Home and End resume from the head cursor.
    */
  def reduceMovement(buffer: Buffer, from: CursorPosition, currentState: AppState)(
    target: (Buffer, CursorPosition) => CursorTarget
  ): ReducerResult =
    ReducerResult.fromTransition(
      currentState,
      Focused.modifyBufferWithId(buffer.id) { current =>
        val landed = target(current, from)
        current.copy(
          editing =
            current.editing.withPrimary(Cursor(landed.cursor, None, Some(landed.preferredColumn), landed.preferredXPx))
        )
      }
    )

  def reduceSelectionExtension(buffer: Buffer, cursor: CursorPosition, currentState: AppState)(
    target: (Buffer, CursorPosition) => CursorTarget
  ): ReducerResult =
    ReducerResult.fromTransition(
      currentState,
      Focused.modifyBufferWithId(buffer.id) { current =>
        val landed = target(current, cursor)
        extendSelection(current, cursor, landed.cursor, Some(landed.preferredColumn), landed.preferredXPx)
      }
    )

  def selectionFocusOrCursor(buffer: Buffer, cursor: CursorPosition): CursorPosition =
    buffer.primarySelection.map(_.focus).getOrElse(cursor)

  def extendSelection(
    buffer: Buffer,
    anchor: CursorPosition,
    focus: CursorPosition,
    preferredColumn: Option[Int],
    preferredXPx: Option[Float]
  ): Buffer =
    val selectionAnchor = buffer.primarySelection.map(_.anchor).getOrElse(anchor)
    buffer.copy(
      editing = buffer.editing.withPrimary(Cursor(focus, Some(selectionAnchor), preferredColumn, preferredXPx))
    )

  def collapseSelectionsToFocus(buffer: Buffer): Buffer =
    val cursors = activeSelections(buffer)
      .map(_.focus)
      .distinct
      .sortBy(cursor => (cursor.line, cursor.column))
    buffer.copy(editing = EditingState(cursors))

  /** A page is a screenful of what the reader can see. Under word wrap that is a screenful of *visual rows*, which is
    * far fewer logical lines than `visibleLines` -- counted in logical lines, one PageDown through wrapped prose jumps
    * several screens, usually straight to the end of the document.
    *
    * The viewport is deliberately not set here. Placing it needs the same text measurement placing the cursor does, and
    * that belongs at the effect boundary (`CursorViewport.ensureVisibleCursors`, which runs after every reduce that
    * moves a cursor) rather than in a reducer. Computing one here was also how a page move could scroll the viewport
    * when it had not moved the cursor at all: the old bottom clamp, `totalLines - visibleLines`, is a logical-line
    * count that goes negative for a document whose wrapped rows outnumber its lines, and snapped the viewport to the
    * top of the file on the second PageDown at a document's end.
    *
    * Where one cursor lands after a page move of `direction`, shared by the plain page keys and their shifted
    * selection-extending forms so both travel the same distance over the same rows (#1292).
    */
  def pageTarget(
    buffer: Buffer,
    currentState: AppState,
    paneId: PaneId,
    direction: Int
  ): CursorPosition => CursorPosition =
    val totalLines  = buffer.document.content.lineCount
    val visibleRows = math.max(1, buffer.viewport.visibleLines)
    // The window has to reach a whole screenful in the direction of travel, which the default geometry only covers
    // downwards.
    val geometry =
      if EditorCursorSupport.useVisualLineNavigation(currentState) then
        com.serenity.state.manager.EditorGeometryProducer
          .forPane(currentState, paneId, rowsAbove = visibleRows)
          .map(_.navigation)
      else None

    def logicalTarget(cursor: CursorPosition): CursorPosition =
      val targetLine =
        if direction < 0 then math.max(0, cursor.line - visibleRows)
        else math.min(cursor.line + visibleRows, totalLines - 1)
      CursorPosition(targetLine, 0)

    // A cursor whose own row the window does not cover -- a secondary cursor far from the primary one the window is
    // anchored on -- falls back to logical lines rather than being left where it is.
    def visualTarget(navigation: NavigationGeometry)(cursor: CursorPosition): Option[CursorPosition] =
      navigation.visualRowIndexFor(cursor).flatMap { row =>
        val lastRow   = navigation.visualLines.length - 1
        val targetRow = math.max(0, math.min(lastRow, row + direction * visibleRows))
        navigation.visualLines.lift(targetRow).map(line => CursorPosition(line.bufferLine, line.startColumn))
      }

    geometry match
      case Some(navigation) => cursor => visualTarget(navigation)(cursor).getOrElse(logicalTarget(cursor))
      case None             => logicalTarget

  /** Column-based document layout (issue #1338, Phase 1): where one cursor lands after a `ColumnLeft`/`ColumnRight`
    * move of `direction` -- fixed/discrete jumps of exactly `visibleLines` visual rows, identical to `pageTarget`'s own
    * distance. Kept as its own named entry point (reusing `pageTarget`'s visual-row-walk branch rather than duplicating
    * it) so the two can diverge later without one masquerading as the other.
    */
  def columnTarget(
    buffer: Buffer,
    currentState: AppState,
    paneId: PaneId,
    direction: Int
  ): CursorPosition => CursorPosition =
    pageTarget(buffer, currentState, paneId, direction)

  def activeSelections(buffer: Buffer): List[Selection] =
    buffer.allSelections.distinct
      .sortBy(selection =>
        (
          selection.start.line,
          selection.start.column,
          selection.end.line,
          selection.end.column
        )
      )

  def selectionStartOffset(selection: Selection, content: Rope): Int =
    content.graphemeBoundaryBeforeOrAt(content.lineColumnToOffset(selection.start.line, selection.start.column))

  def selectionEndOffset(selection: Selection, content: Rope): Int =
    content.graphemeBoundaryAfterOrAt(content.lineColumnToOffset(selection.end.line, selection.end.column))

  def distinctCursorLines(buffer: Buffer): List[Int] =
    buffer.editing.cursorPositions.distinct
      .sortBy(cursor => (cursor.line, cursor.column))
      .map(_.line)
      .distinct

  def selectionLines(buffer: Buffer): List[Int] =
    activeSelections(buffer)
      .flatMap(selection => selection.start.line to selection.end.line)
      .distinct
      .sorted

  final case class CursorEntry(cursor: CursorPosition, offset: Int)

  def multiCursorEntries(buffer: Buffer): List[CursorEntry] =
    buffer.editing.cursorPositions.distinct
      .map(cursor => CursorEntry(cursor, buffer.document.content.lineColumnToOffset(cursor.line, cursor.column)))
      .sortBy(_.offset)

  def mergedActiveSelectionRanges(buffer: Buffer, content: Rope): List[(Int, Int)] =
    val ranges = activeSelections(buffer)
      .map(selection => (selectionStartOffset(selection, content), selectionEndOffset(selection, content)))
      .filter { case (start, end) => start < end }
    mergeOverlappingSelectionRanges(ranges)

  def mergeOverlappingSelectionRanges(ranges: List[(Int, Int)]): List[(Int, Int)] =
    ranges
      .sortBy { case (start, end) => (start, end) }
      .foldLeft(List.empty[(Int, Int)]) {
        case (Nil, range) => range :: Nil
        case ((currentStart, currentEnd) :: rest, (nextStart, nextEnd)) =>
          if nextStart < currentEnd then (currentStart, math.max(currentEnd, nextEnd)) :: rest
          else (nextStart, nextEnd) :: (currentStart, currentEnd) :: rest
      }
      .reverse

  def selectedTexts(buffer: Buffer): List[String] =
    mergedActiveSelectionRanges(buffer, buffer.document.content).map {
      case (start, end) =>
        buffer.document.content.sliceString(start, end)
    }
