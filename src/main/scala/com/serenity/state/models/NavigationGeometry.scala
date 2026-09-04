package com.serenity.state.models

/** Immutable, UI-free text geometry. Produced from a rendered text layout at the effect boundary and consumed by pure
  * navigation logic, so cursor movement never reaches for a font or a Java2D metric.
  */
final case class TextCaretStop(column: Int, xPx: Float)

final case class TextVisualLine(
    bufferLine: Int,
    startColumn: Int,
    endColumn: Int,
    text: String,
    widthPx: Float,
    caretStops: Vector[TextCaretStop],
    xOffsetPx: Float = 0.0f,
    xSortedCaretStops: Vector[TextCaretStop] = Vector.empty
):

  def xForColumn(column: Int): Option[Float] =
    @annotation.tailrec
    def search(low: Int, high: Int, best: Option[TextCaretStop]): Option[Float] =
      if low > high then best.orElse(caretStops.lastOption).map(_.xPx)
      else
        val middle = (low + high) >>> 1
        val stop   = caretStops(middle)
        if stop.column >= column then search(low, middle - 1, Some(stop))
        else search(middle + 1, high, best)

    search(0, caretStops.length - 1, None)

  def nearestColumnForXPx(xPx: Float): Int =
    val stops = if xSortedCaretStops.nonEmpty then xSortedCaretStops else caretStops.sortBy(_.xPx)

    stops.headOption match
      case None => 0
      case Some(firstStop) =>
        @annotation.tailrec
        def search(low: Int, high: Int): TextCaretStop =
          if low > high then
            // stops is non-empty here (headOption matched Some above), so the (None, None) case can only
            // arise if both the low and high probes fell outside the array, which the binary search never
            // does starting from a non-empty range -- firstStop is an unreachable-in-practice fallback.
            (stops.lift(high), stops.lift(low)) match
              case (Some(left), Some(right)) => closer(left, right, xPx)
              case (Some(left), None)        => left
              case (None, Some(right))       => right
              case (None, None)              => firstStop
          else
            val middle = (low + high) >>> 1
            if stops(middle).xPx < xPx then search(middle + 1, high)
            else search(low, middle - 1)

        search(0, stops.length - 1).column

  private def closer(first: TextCaretStop, second: TextCaretStop, xPx: Float): TextCaretStop =
    if math.abs(second.xPx - xPx) < math.abs(first.xPx - xPx) then second else first

/** The immutable geometry a vertical cursor move needs, produced once at the effect boundary and handed to the pure
  * reducer. `charWidthPx` and `panelWidthColumns` back the monospace fallbacks used when the measured layout has no
  * caret stop for a cursor or when word wrap is off.
  */
final case class EditorGeometry(navigation: NavigationGeometry, charWidthPx: Int, panelWidthColumns: Int)

final case class NavigationGeometry(visualLines: Vector[TextVisualLine]):

  /** Index into [[visualLines]] of the visual (wrapped) row `cursor` sits on -- the single containment lookup every
    * cursor-to-row resolution shares ([[visualLineFor]], [[xPxForCursor]], [[moveVertical]]) and, crucially, the one
    * `Renderer`'s caret drawing must also use (`Renderer.calculateCursorVisualPosition`), or the caret is painted on a
    * different wrapped row than vertical navigation moves from -- the "cursor stuck at a wrap boundary until nudged
    * left/right" bug. At a wrap boundary, where one row's `endColumn` equals the next row's `startColumn`, both rows
    * match the column; `.lastOption` resolves it to the later row, so a boundary column belongs to the start of the
    * next visual line.
    */
  def visualRowIndexFor(cursor: CursorPosition): Option[Int] =
    visualLines.zipWithIndex
      .filter {
        case (line, _) =>
          line.bufferLine == cursor.line && cursor.column >= line.startColumn && cursor.column <= line.endColumn
      }
      .lastOption
      .map(_._2)

  /** The visual (wrapped) row `cursor` currently sits on, exposed for callers (Home/End under visual-line navigation,
    * and the renderer's caret placement) that need that row's own `startColumn`/`endColumn`/caret stops rather than a
    * neighbouring row.
    */
  def visualLineFor(cursor: CursorPosition): Option[TextVisualLine] =
    visualRowIndexFor(cursor).map(visualLines)

  def xPxForCursor(cursor: CursorPosition): Option[Float] =
    visualLineFor(cursor).map(line => line.xForColumn(cursor.column).getOrElse(line.widthPx))

  def cursorForVisualRowAndXPx(row: Int, xPx: Float): Option[CursorPosition] =
    visualLines.lift(row).map(line => CursorPosition(line.bufferLine, line.nearestColumnForXPx(xPx)))

  def moveVertical(cursor: CursorPosition, direction: Int, preferredXPx: Float): Option[CursorPosition] =
    visualRowIndexFor(cursor)
      .map(_ + direction)
      .flatMap(targetRow => cursorForVisualRowAndXPx(targetRow, preferredXPx))
