package com.serenity.state.models

import scala.util.hashing.MurmurHash3

/** A caret position as a value, for callers that want a pair rather than two parallel arrays. Rows do not hold these:
  * see [[MeasuredRow.caretStops]].
  */
final case class TextCaretStop(column: Int, xPx: Float)

/** A wrapped row's measured geometry, relative to the row's own origin: independent of the buffer line it sits on, the
  * paragraph's alignment and any drop cap inset, which [[TextVisualLine]] applies when the row is read. That makes it
  * the unit a wrap cache can share between every placement of the same text.
  *
  * Caret stops are two parallel primitive arrays rather than one object per stop: stop `i` sits at column
  * `startColumn + caretOffsets(i)` and `caretXs(i)` pixels. The `landing` pair holds the same stops ordered by x for
  * hit-testing (minus stops a hidden column makes unlandable); it is the same arrays as the caret pair whenever the
  * stops already ascend. Offsets are relative to `startColumn`, so moving a row to another column is a field change.
  */
final case class MeasuredRow(
    startColumn: Int,
    endColumn: Int,
    text: String,
    widthPx: Float,
    caretXs: IArray[Float],
    caretOffsets: IArray[Int],
    landingXs: IArray[Float],
    landingOffsets: IArray[Int],
    // Per-line measured metrics (measured/proportional layout only): the tallest run on this visual line. 0 means
    // "unset" -- callers fall back to the snapshot's uniform lineHeightPx/ascentPx (cell layout, or non-rich buffers).
    heightPx: Int = 0,
    ascentPx: Int = 0
):

  // Arrays compare by reference, so the generated equals would call two identical measurements different. Row
  // equality drives dirty-row diffing; the `eq` fast path keeps it free for the shared rows a warm cache serves.
  override def equals(other: Any): Boolean = other match
    case that: MeasuredRow =>
      (this eq that) || (startColumn == that.startColumn && endColumn == that.endColumn && text == that.text &&
        widthPx == that.widthPx && heightPx == that.heightPx && ascentPx == that.ascentPx &&
        caretXs.sameElements(that.caretXs) && caretOffsets.sameElements(that.caretOffsets) &&
        landingXs.sameElements(that.landingXs) && landingOffsets.sameElements(that.landingOffsets))
    case _ => false

  override def hashCode: Int =
    MurmurHash3.finalizeHash(
      MurmurHash3.mix(MurmurHash3.mix(startColumn, endColumn), MurmurHash3.mix(text.hashCode, caretXs.length)),
      4
    )

  def caretCount: Int = caretXs.length

  def caretColumn(index: Int): Int = startColumn + caretOffsets(index)

  /** The stops as objects, allocated on each call: for tests and diagnostics, never for layout or painting. */
  def caretStops: Vector[TextCaretStop] = MeasuredRow.stops(startColumn, caretOffsets, caretXs)

  def xSortedCaretStops: Vector[TextCaretStop] = MeasuredRow.stops(startColumn, landingOffsets, landingXs)

  def xForColumn(column: Int, offsetPx: Float = 0.0f): Option[Float] =
    @annotation.tailrec
    def search(low: Int, high: Int, best: Int): Option[Float] =
      if low > high then
        val resolved = if best >= 0 then best else caretXs.length - 1
        Option.when(resolved >= 0)(caretXs(resolved) + offsetPx)
      else
        val middle = (low + high) >>> 1
        if caretColumn(middle) >= column then search(low, middle - 1, middle)
        else search(middle + 1, high, best)

    search(0, caretXs.length - 1, -1)

  def nearestColumnForXPx(xPx: Float, offsetPx: Float = 0.0f): Int =
    val count = landingXs.length

    @annotation.tailrec
    def search(low: Int, high: Int): Int =
      if low > high then
        val left  = Option.when(high >= 0 && high < count)(high)
        val right = Option.when(low >= 0 && low < count)(low)
        (left, right) match
          case (Some(l), Some(r)) => if closer(r, l, xPx, offsetPx) then r else l
          case (Some(l), None)    => l
          case (None, Some(r))    => r
          case (None, None)       => 0
      else
        val middle = (low + high) >>> 1
        if landingXs(middle) + offsetPx < xPx then search(middle + 1, high)
        else search(low, middle - 1)

    if count == 0 then 0 else startColumn + landingOffsets(search(0, count - 1))

  private def closer(candidate: Int, incumbent: Int, xPx: Float, offsetPx: Float): Boolean =
    math.abs(landingXs(candidate) + offsetPx - xPx) < math.abs(landingXs(incumbent) + offsetPx - xPx)

object MeasuredRow:

  /** A row built from stop objects. `xSorted` empty means "order `stops` by x". */
  def fromStops(
    startColumn: Int,
    endColumn: Int,
    text: String,
    widthPx: Float,
    stops: Vector[TextCaretStop],
    xSorted: Vector[TextCaretStop] = Vector.empty,
    heightPx: Int = 0,
    ascentPx: Int = 0
  ): MeasuredRow =
    val landing = if xSorted.nonEmpty then xSorted else stops.sortBy(_.xPx)
    MeasuredRow(
      startColumn,
      endColumn,
      text,
      widthPx,
      IArray.from(stops.map(_.xPx)),
      IArray.from(stops.map(_.column - startColumn)),
      IArray.from(landing.map(_.xPx)),
      IArray.from(landing.map(_.column - startColumn)),
      heightPx,
      ascentPx
    )

  /** `xs` and `offsets` ordered by x, stably, or the arrays themselves when they already ascend. */
  def orderedByX(xs: IArray[Float], offsets: IArray[Int]): (IArray[Float], IArray[Int]) =
    val ascending = (1 until xs.length).forall(index => xs(index - 1) <= xs(index))
    if ascending then (xs, offsets)
    else
      val order = xs.indices.sortBy(xs(_))
      (IArray.from(order.map(xs(_))), IArray.from(order.map(offsets(_))))

  private def stops(startColumn: Int, offsets: IArray[Int], xs: IArray[Float]): Vector[TextCaretStop] =
    Vector.tabulate(xs.length)(index => TextCaretStop(startColumn + offsets(index), xs(index)))

/** A [[MeasuredRow]] placed on a buffer line and shifted `xOffsetPx` right by its paragraph's alignment and drop cap
  * inset. Placing a cached row allocates only this wrapper: the measured geometry is shared, never copied.
  */
final case class TextVisualLine(bufferLine: Int, row: MeasuredRow, xOffsetPx: Float):
  export row.{ascentPx, caretCount, caretColumn, endColumn, heightPx, startColumn, text, widthPx}

  /** Caret x of stop `index` in the coordinates of the pane. */
  def caretX(index: Int): Float = row.caretXs(index) + xOffsetPx

  /** The row's caret stops in the coordinates of the pane, as objects; see [[MeasuredRow.caretStops]]. */
  def caretStops: Vector[TextCaretStop] = shifted(row.caretStops)

  def xSortedCaretStops: Vector[TextCaretStop] = shifted(row.xSortedCaretStops)

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
      MeasuredRow.fromStops(startColumn, endColumn, text, widthPx, caretStops, xSortedCaretStops, heightPx, ascentPx),
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
