package com.serenity.state.models

/** Immutable, UI-free text geometry. Produced from a rendered text layout at the effect boundary and consumed by pure
  * navigation logic, so cursor movement never reaches for a font or a Java2D metric.
  */
final case class TextCaretStop(column: Int, xPx: Float)

/** A wrapped row's measured geometry, relative to the row's own origin: independent of the buffer line it sits on, the
  * paragraph's alignment and any drop cap inset, which [[TextVisualLine]] applies when the row is read. That makes it
  * the unit a wrap cache can share between every placement of the same text.
  */
final case class MeasuredRow(
    startColumn: Int,
    endColumn: Int,
    text: String,
    widthPx: Float,
    caretStops: Vector[TextCaretStop],
    xSortedCaretStops: Vector[TextCaretStop] = Vector.empty,
    // Per-line measured metrics (measured/proportional layout only): the tallest run on this visual line. 0 means
    // "unset" -- callers fall back to the snapshot's uniform lineHeightPx/ascentPx (cell layout, or non-rich buffers).
    heightPx: Int = 0,
    ascentPx: Int = 0
):

  def xForColumn(column: Int, offsetPx: Float = 0.0f): Option[Float] =
    @annotation.tailrec
    def search(low: Int, high: Int, best: Option[TextCaretStop]): Option[Float] =
      if low > high then best.orElse(caretStops.lastOption).map(_.xPx + offsetPx)
      else
        val middle = (low + high) >>> 1
        val stop   = caretStops(middle)
        if stop.column >= column then search(low, middle - 1, Some(stop))
        else search(middle + 1, high, best)

    search(0, caretStops.length - 1, None)

  def nearestColumnForXPx(xPx: Float, offsetPx: Float = 0.0f): Int =
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
              case (Some(left), Some(right)) => closer(left, right, xPx, offsetPx)
              case (Some(left), None)        => left
              case (None, Some(right))       => right
              case (None, None)              => firstStop
          else
            val middle = (low + high) >>> 1
            if stops(middle).xPx + offsetPx < xPx then search(middle + 1, high)
            else search(low, middle - 1)

        search(0, stops.length - 1).column

  private def closer(first: TextCaretStop, second: TextCaretStop, xPx: Float, offsetPx: Float): TextCaretStop =
    if math.abs(second.xPx + offsetPx - xPx) < math.abs(first.xPx + offsetPx - xPx) then second else first

/** A [[MeasuredRow]] placed on a buffer line and shifted `xOffsetPx` right by its paragraph's alignment and drop cap
  * inset. Placing a cached row allocates only this wrapper: the measured geometry is shared, never copied.
  */
final case class TextVisualLine(bufferLine: Int, row: MeasuredRow, xOffsetPx: Float):
  export row.{ascentPx, endColumn, heightPx, startColumn, text, widthPx}

  /** The row's caret stops in the coordinates of the pane, shifted by [[xOffsetPx]]. */
  lazy val caretStops: Vector[TextCaretStop] = shifted(row.caretStops)

  lazy val xSortedCaretStops: Vector[TextCaretStop] = shifted(row.xSortedCaretStops)

  def xForColumn(column: Int): Option[Float] = row.xForColumn(column, xOffsetPx)

  def nearestColumnForXPx(xPx: Float): Int = row.nearestColumnForXPx(xPx, xOffsetPx)

  private def shifted(stops: Vector[TextCaretStop]): Vector[TextCaretStop] =
    if xOffsetPx == 0.0f then stops else stops.map(stop => stop.copy(xPx = stop.xPx + xOffsetPx))

object TextVisualLine:

  /** A line whose carets are already in pane coordinates, which is the whole geometry when there is no offset. */
  def apply(
    bufferLine: Int,
    startColumn: Int,
    endColumn: Int,
    text: String,
    widthPx: Float,
    caretStops: Vector[TextCaretStop],
    xOffsetPx: Float = 0.0f,
    xSortedCaretStops: Vector[TextCaretStop] = Vector.empty,
    heightPx: Int = 0,
    ascentPx: Int = 0
  ): TextVisualLine =
    TextVisualLine(
      bufferLine,
      MeasuredRow(startColumn, endColumn, text, widthPx, caretStops, xSortedCaretStops, heightPx, ascentPx),
      xOffsetPx
    )

/** The immutable geometry a vertical cursor move needs, produced once at the effect boundary and handed to the pure
  * reducer. `charWidthPx` and `panelWidthColumns` back the monospace fallbacks used when the measured layout has no
  * caret stop for a cursor or when word wrap is off.
  */
final case class EditorGeometry(navigation: NavigationGeometry, charWidthPx: Int, panelWidthColumns: Int)

final case class NavigationGeometry(visualLines: Vector[TextVisualLine]):

  /** Index into [[visualLines]] of the visual (wrapped) row `cursor` sits on -- the single containment lookup every
    * cursor-to-row resolution shares ([[visualLineFor]], [[xPxForCursor]], [[moveVertical]]) and, crucially, the one
    * `Renderer`'s caret drawing must also use (`RendererCursorGlyphs.calculateCursorVisualPosition`), or the caret is
    * painted on a different wrapped row than vertical navigation moves from -- the "cursor stuck at a wrap boundary
    * until nudged left/right" bug. At a wrap boundary, where one row's `endColumn` equals the next row's `startColumn`,
    * both rows match the column and the cursor's own [[RowAffinity]] settles it: downstream (all but End, and the
    * default) reads the column as the start of the later row, upstream as the end of the earlier one.
    */
  def visualRowIndexFor(cursor: CursorPosition): Option[Int] =
    val matching = visualLines.zipWithIndex.filter {
      case (line, _) =>
        line.bufferLine == cursor.line && cursor.column >= line.startColumn && cursor.column <= line.endColumn
    }
    val resolved = cursor.rowAffinity match
      case RowAffinity.Upstream   => matching.headOption
      case RowAffinity.Downstream => matching.lastOption
    resolved.map(_._2)

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
