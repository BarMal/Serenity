package com.serenity.ui.layout

import scala.annotation.tailrec

import com.serenity.state.models.TextVisualLine

/** Column-mode paging by pixels. A column is as tall as `visibleLines` base lines, but its rows are not all one line
  * high: headings are larger and have room around them, a drop cap reserves its glyph's height, a raised letter grows
  * its row. So a column takes rows while they stack to no more than it is tall, and where a column starts is a function
  * of the document and its layout alone.
  *
  * Where a pixel rule would otherwise leave a choice: a column always takes at least one row (a row taller than the
  * whole column stands alone and is clipped, rather than leaving a column empty); a column opens and closes flush, so
  * the room above its first row and below its last is dropped; and a drop cap's glyph is not split between columns
  * unless it alone is taller than one.
  */
object ColumnPaging:

  /** The first row of a column: row `row` of the wrapped logical line `line`. */
  final case class ColumnStart(line: Int, row: Int):
    def isAfter(other: ColumnStart): Boolean = line > other.line || (line == other.line && row > other.row)

  object ColumnStart:
    val Origin: ColumnStart = ColumnStart(0, 0)

  /** The pixels a column is filled against: the pane gives a column `visibleLines` rows of its cell grid, which is
    * `columnHeightPx`, however tall the rows the font measures to are. A row is never counted shorter than a base line,
    * so a column holds at most `rowsPerColumn` rows however short its rows measure.
    */
  final case class Frame(columnHeightPx: Int, lineHeightPx: Int, measuredLayout: Boolean):

    def rowsPerColumn: Int = math.max(1, columnHeightPx / math.max(1, lineHeightPx))

    /** Where a row's text and its caret are, which is what a cell grid fixes at one line. */
    def textBoxPx(row: TextVisualLine): Int =
      if measuredLayout then math.max(lineHeightPx, row.heightPx) else lineHeightPx

    /** The room a row stacks to: its text box, what its paragraph reserves below it and a heading's space. */
    def bandPx(row: TextVisualLine): Int =
      row.spaceBeforePx + textBoxPx(row) + (if measuredLayout then row.reservedBelowPx else 0) + row.spaceAfterPx

  /** Rows a drop cap paragraph may wrap beyond those a page can show, so a glyph's span is whole when it is read. */
  private[layout] val GlyphSpanMarginRows = 32

  /** The row after the last of the rows the glyph drawn from `home` covers. */
  private[layout] def glyphSpanEnd(rows: Vector[TextVisualLine], home: Int, frame: Frame): Int =
    val glyphBottomPx = rows(home).dropCapHeightPx
    @tailrec def end(index: Int, topPx: Int): Int =
      if topPx >= glyphBottomPx || index >= rows.length || rows(index).bufferLine != rows(home).bufferLine then index
      else end(index + 1, topPx + (if rows(index).heightPx > 0 then rows(index).heightPx else frame.lineHeightPx))
    end(home, 0)

  /** What `rows(index)` takes to be placed in a column that already holds something: itself, or for a drop cap's home
    * row the whole span of its glyph; either way without the room below the last row, which a column drops.
    */
  private[layout] def pixelsToPlace(rows: Vector[TextVisualLine], index: Int, frame: Frame): Int =
    if rows(index).dropCapHeightPx > 0 then
      val end = glyphSpanEnd(rows, index, frame)
      (index until end).map(row => frame.bandPx(rows(row))).sum - rows(end - 1).spaceAfterPx
    else frame.bandPx(rows(index)) - rows(index).spaceAfterPx

  /** Whether a column holding `usedPx` must close before `rows(index)`. An empty column never does. */
  private[layout] def closesBefore(rows: Vector[TextVisualLine], index: Int, usedPx: Int, frame: Frame): Boolean =
    usedPx > 0 && usedPx + pixelsToPlace(rows, index, frame) > frame.columnHeightPx

  private[layout] def taken(row: TextVisualLine, usedPx: Int, frame: Frame): Int =
    if usedPx == 0 then frame.bandPx(row) - row.spaceBeforePx else frame.bandPx(row)

  private[layout] def flush(column: Vector[TextVisualLine]): Vector[TextVisualLine] =
    val opened = HeadingSpacingLayout.droppingLeadingSpace(column)
    opened.lastOption match
      case Some(last) if last.spaceAfterPx != 0 => opened.updated(opened.length - 1, last.copy(spaceAfterPx = 0))
      case _                                    => opened

  final private[layout] case class Fill(
      done: Vector[Vector[TextVisualLine]],
      current: Vector[TextVisualLine],
      usedPx: Int,
      placed: Int,
      full: Boolean
  )

  final private[layout] case class Open(line: Int, row: Int, rows: Vector[TextVisualLine]):
    def start: ColumnStart = ColumnStart(line, row)

/** The columns of one buffer at one layout: where each starts, and the rows each holds. `place(line, maxRows)` gives
  * the placed rows of a logical line, the first `maxRows` of them at most; `heightIndex` builds, from a line's pixel
  * total, the index that lets whole lines that fit be skipped without being read.
  *
  * Both questions -- where the next column starts, and which rows a column holds -- are answered by the same
  * [[ColumnPaging]] rule, so painting a page and anchoring the viewport to it cannot disagree.
  */
final class ColumnPager(
    val richText: RichTextContext,
    lineCount: Int,
    frame: ColumnPaging.Frame,
    heightIndex: (Int => Int) => VisualRowCounts,
    place: (Int, Int) => Vector[TextVisualLine]
):
  import ColumnPaging.*

  private val heights =
    heightIndex(line => math.max(1, place(line, Int.MaxValue).iterator.map(frame.bandPx).sum))

  /** The start of the column after the one that starts at `start`; `None` when the document ends in that column. */
  def nextStart(start: ColumnStart): Option[ColumnStart] =
    if lineCount <= 0 then None else advance(open(start)).map(_.start)

  /** Every column start from the document's first, through the column that holds `target`. */
  def startsThrough(target: ColumnStart): Vector[ColumnStart] =
    if lineCount <= 0 then Vector(ColumnStart.Origin)
    else
      @tailrec def walk(current: Open, acc: Vector[ColumnStart]): Vector[ColumnStart] =
        advance(current) match
          case Some(next) if !next.start.isAfter(target) => walk(next, acc :+ next.start)
          case _                                         => acc
      val first = open(ColumnStart.Origin)
      walk(first, Vector(first.start))

  /** The rows of the `columnCount` columns from `start`; the last holds fewer when the document ends first. */
  def fill(start: ColumnStart, columnCount: Int): Vector[Vector[TextVisualLine]] =
    val wanted = math.max(1, columnCount)
    val slots  = frame.rowsPerColumn * wanted

    @tailrec def consume(rows: Vector[TextVisualLine], index: Int, state: Fill): Fill =
      if state.full || index >= rows.length then state
      else if closesBefore(rows, index, state.usedPx, frame) then
        val done = state.done :+ flush(state.current)
        consume(rows, index, Fill(done, Vector.empty, 0, state.placed, done.size >= wanted))
      else
        val row = rows(index)
        consume(
          rows,
          index + 1,
          state.copy(
            current = state.current :+ row,
            usedPx = state.usedPx + taken(row, state.usedPx, frame),
            placed = state.placed + 1
          )
        )

    @tailrec def lines(line: Int, row: Int, state: Fill): Fill =
      if state.full || line >= lineCount then state
      else
        val rows = place(line, row + (slots - state.placed) + 1 + GlyphSpanMarginRows)
        if row >= rows.length then lines(line + 1, row - rows.length, state)
        else lines(line + 1, 0, consume(rows, row, state))

    val end =
      lines(math.max(0, start.line), math.max(0, start.row), Fill(Vector.empty, Vector.empty, 0, 0, full = false))
    if end.full || end.current.isEmpty then end.done else end.done :+ flush(end.current)

  /** `start` with a row past its line's last carried into the lines after it. */
  @tailrec private def open(start: ColumnStart): Open =
    val line = math.max(0, math.min(start.line, lineCount - 1))
    val rows = place(line, Int.MaxValue)
    if start.row >= rows.length && line + 1 < lineCount then open(ColumnStart(line + 1, start.row - rows.length))
    else Open(line, math.max(0, math.min(start.row, rows.length - 1)), rows)

  private def advance(from: Open): Option[Open] =
    @tailrec def closeIndex(rows: Vector[TextVisualLine], index: Int, usedPx: Int): Either[Int, Int] =
      if index >= rows.length then Right(usedPx)
      else if closesBefore(rows, index, usedPx, frame) then Left(index)
      else closeIndex(rows, index + 1, usedPx + taken(rows(index), usedPx, frame))

    @tailrec def scan(line: Int, rows: Vector[TextVisualLine], row: Int, usedPx: Int): Option[Open] =
      closeIndex(rows, row, usedPx) match
        case Left(at) => Some(Open(line, at, rows))
        case Right(usedAfter) =>
          val next = line + 1
          if next >= lineCount then None
          else
            heights.rowBelow(next, frame.columnHeightPx - usedAfter) match
              case None => None
              case Some((fits, _)) =>
                scan(fits, place(fits, Int.MaxValue), 0, usedAfter + heights.rowsBetween(next, fits))

    scan(from.line, from.rows, from.row, 0)
