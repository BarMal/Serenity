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

    @annotation.tailrec
    def search(low: Int, high: Int): TextCaretStop =
      if low > high then
        (stops.lift(high), stops.lift(low)) match
          case (Some(left), Some(right)) => closer(left, right, xPx)
          case (Some(left), None)        => left
          case (None, Some(right))       => right
          case (None, None)              => stops.head
      else
        val middle = (low + high) >>> 1
        if stops(middle).xPx < xPx then search(middle + 1, high)
        else search(low, middle - 1)

    if stops.isEmpty then 0 else search(0, stops.length - 1).column

  private def closer(first: TextCaretStop, second: TextCaretStop, xPx: Float): TextCaretStop =
    if math.abs(second.xPx - xPx) < math.abs(first.xPx - xPx) then second else first

final case class NavigationGeometry(visualLines: Vector[TextVisualLine]):

  def xPxForCursor(cursor: CursorPosition): Option[Float] =
    visualLines.collectFirst {
      case line
          if line.bufferLine == cursor.line && cursor.column >= line.startColumn && cursor.column <= line.endColumn =>
        line.xForColumn(cursor.column).getOrElse(line.widthPx)
    }

  def cursorForVisualRowAndXPx(row: Int, xPx: Float): Option[CursorPosition] =
    visualLines.lift(row).map(line => CursorPosition(line.bufferLine, line.nearestColumnForXPx(xPx)))

  def moveVertical(cursor: CursorPosition, direction: Int, preferredXPx: Float): Option[CursorPosition] =
    visualLines.zipWithIndex
      .collectFirst {
        case (line, index)
            if line.bufferLine == cursor.line && cursor.column >= line.startColumn && cursor.column <= line.endColumn =>
          index + direction
      }
      .flatMap(targetRow => cursorForVisualRowAndXPx(targetRow, preferredXPx))
