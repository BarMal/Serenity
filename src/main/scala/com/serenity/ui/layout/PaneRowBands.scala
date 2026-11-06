package com.serenity.ui.layout

import java.awt.Font
import java.awt.font.FontRenderContext

import scala.annotation.tailrec

import com.serenity.state.models.{Buffer, TextVisualLine}

/** The room each visual row of a row-mode pane takes as the painter stacks it (`TextRowMetrics`): the space a heading
  * gives it above and below, plus its text box -- the row's measured height and what its paragraph reserves below it,
  * or one grid row on a cell grid. The pane's first row is drawn without its space above
  * (`HeadingSpacingLayout.droppingLeadingSpace`).
  *
  * Placing a viewport against the caret walks these bands, so a caret held a given distance from the pane's top is
  * painted that distance from it, however tall the rows between are. Counting rows instead put a "centred" caret near
  * the bottom of a pane of headings (#2570).
  */
final class PaneRowBands private (
    lineCount: Int,
    rowsOf: Int => Vector[TextVisualLine],
    measuredLayout: Boolean,
    val lineHeightPx: Int
):
  import PaneRowBands.Row

  def textBoxPx(row: Row): Int = textBox(rowsOf(row.line).lift(row.row))

  /** The top that leaves `abovePx` of rows between the pane's top and the top of `caret`'s text box, as near as whole
    * rows allow without exceeding it; the document's first row once there is not that much above the caret.
    */
  def topAbove(caret: Row, abovePx: Int): Row = topWithin(tops(caret), caret, abovePx)

  /** [[topAbove]] read off a walk of [[tops]] already under way, so several limits share one walk. */
  def topWithin(walk: LazyList[(Row, Int)], caret: Row, abovePx: Int): Row =
    walk.takeWhile(_._2 <= math.max(0, abovePx)).lastOption.fold(caret)(_._1)

  /** The top that paints `caret` on grid row `targetRow` of the pane, or the nearest row to it whole rows allow: a
    * heading's space can make the next row up a long step. The painter puts a caret on the grid row its text box's top
    * falls in.
    */
  def topPainting(caret: Row, gridRowPx: Int, targetRow: Int): Row =
    val rowPx               = math.max(1, gridRowPx)
    val targetPx            = targetRow * rowPx + rowPx / 2
    val (reachable, beyond) = tops(caret).span((_, textTopPx) => textTopPx < (targetRow + 1) * rowPx)
    (reachable ++ beyond.take(1))
      .minByOption((_, textTopPx) => (math.abs(textTopPx / rowPx - targetRow), math.abs(textTopPx - targetPx)))
      .fold(caret)(_._1)

  /** Every top from `caret` itself upward, each with how far below the pane's top it paints `caret`'s text box. Lazy
    * and memoised: a caller reading several limits off one walk places each row once.
    */
  def tops(caret: Row): LazyList[(Row, Int)] =
    // A top moving up a row draws its own space above, and the row above it becomes the one whose space is dropped.
    def from(line: Int, rows: Vector[TextVisualLine], index: Int, textTopPx: Int): LazyList[(Row, Int)] =
      val earlier =
        if index > 0 then Some((line, rows, index - 1))
        else if line <= 0 then None
        else
          val above = rowsOf(line - 1)
          Some((line - 1, above, math.max(0, above.length - 1)))
      (Row(line, index), textTopPx) #:: earlier.fold(LazyList.empty) { (earlierLine, earlierRows, earlierIndex) =>
        val earlierRow = earlierRows.lift(earlierIndex)
        from(
          earlierLine,
          earlierRows,
          earlierIndex,
          textTopPx + spaceBefore(rows.lift(index)) + textBox(earlierRow) + spaceAfter(earlierRow)
        )
      }
    from(caret.line, rowsOf(caret.line), caret.row, 0)

  /** The pixels of rows from just below `caret`'s text box to the end of the document, counted no further than
    * `limitPx`.
    */
  def belowPx(caret: Row, limitPx: Int): Int =
    @tailrec def walk(line: Int, rows: Vector[TextVisualLine], index: Int, total: Int): Int =
      if total >= limitPx then limitPx
      else
        val later =
          if index + 1 < rows.length then Some((line, rows, index + 1))
          else if line + 1 >= lineCount then None
          else Some((line + 1, rowsOf(line + 1), 0))
        later match
          case None => total
          case Some((laterLine, laterRows, laterIndex)) =>
            val row = laterRows.lift(laterIndex)
            walk(laterLine, laterRows, laterIndex, total + spaceBefore(row) + textBox(row) + spaceAfter(row))
    val rows = rowsOf(caret.line)
    walk(caret.line, rows, caret.row, spaceAfter(rows.lift(caret.row)))

  private def textBox(row: Option[TextVisualLine]): Int =
    row.fold(lineHeightPx) { line =>
      if measuredLayout then (if line.heightPx > 0 then line.heightPx else lineHeightPx) + line.reservedBelowPx
      else lineHeightPx
    }

  private def spaceBefore(row: Option[TextVisualLine]): Int = row.fold(0)(_.spaceBeforePx)

  private def spaceAfter(row: Option[TextVisualLine]): Int = row.fold(0)(_.spaceAfterPx)

object PaneRowBands:

  /** A visual row: the `row`th row of buffer line `line`. Ordered as the rows are stacked. */
  final case class Row(line: Int, row: Int)

  object Row:
    given Ordering[Row] = Ordering.by(row => (row.line, row.row))

  def forBuffer(
    buffer: Buffer,
    wrapWidthPx: Int,
    font: Font,
    cellMetricsOverride: Option[CellMetrics],
    forceCellLayout: Boolean,
    wordWrapEnabled: Boolean,
    richText: RichTextContext,
    wrapCache: WrappedLineCache,
    fontRenderContext: FontRenderContext = TextLayoutSnapshot.defaultFontRenderContext()
  ): PaneRowBands =
    val cellMetrics = cellMetricsOverride.getOrElse(CellMetrics.fromFont(font))
    val measuredLayout =
      LineLayout.measuredLayoutFor(font, fontRenderContext, forceCellLayout, richText, wrapCache.shapedText.probes)
    val lineHeightPx =
      if measuredLayout then math.max(1, wrapCache.layoutMemos.lineExtents.of(font, fontRenderContext).heightPx)
      else math.max(1, cellMetrics.lineHeight)
    val content = buffer.document.content
    def rowsOf(line: Int): Vector[TextVisualLine] =
      val text = content.getLine(line).getOrElse("")
      if wordWrapEnabled then
        LineLayout.placedWrappedLine(
          line,
          text,
          math.max(1, wrapWidthPx),
          font,
          fontRenderContext,
          measuredLayout,
          cellMetrics,
          Int.MaxValue,
          richText,
          wrapCache
        )
      else
        LineLayout.unwrappedLine(
          line,
          text,
          buffer.viewport,
          math.max(1, wrapWidthPx),
          font,
          fontRenderContext,
          measuredLayout,
          cellMetrics,
          richText,
          wrapCache
        )
    PaneRowBands(content.lineCount, rowsOf, measuredLayout, lineHeightPx)
