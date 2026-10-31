package com.serenity.ui.layout

/** One pane's scroll position, with the row counts it was measured from so a position picked on the scrollbar can be
  * turned back into the line and row the viewport rests on.
  */
final case class PaneScroll(extent: ScrollExtent, rows: VisualRowCounts):

  /** The viewport's `(topLine, topVisualLine)` that shows the document from visual row `firstRow`. */
  def viewportTop(firstRow: Int): (Int, Int) = rows.estimatedLineAtRow(extent.clamped(firstRow))

object PaneScroll:

  /** Documents up to this many lines are measured whole, so their scrollbar is exact. A longer one counts the lines it
    * has not measured as a row each, which keeps the cost of a frame independent of the document's length; the lines on
    * screen are always measured, and every line scrolled past stays so.
    */
  val ExactLineLimit: Int = 2048

  def of(rows: VisualRowCounts, topLine: Int, topVisualLine: Int, visibleRows: Int): PaneScroll =
    val lineCount = rows.lineCount
    val (rowsAbove, totalRows) =
      rows.rowsAboveAndTotal(topLine, (topLine + visibleRows).max(0), exact = lineCount <= ExactLineLimit)
    PaneScroll(
      ScrollExtent(firstRow = rowsAbove + topVisualLine.max(0), visibleRows = visibleRows, totalRows = totalRows),
      rows
    )

  /** The scroll of a document the screen alone does not cover, once every one of its lines is measured: measuring the
    * rest is left to whoever holds the [[PendingScroll]]. A document over [[ExactLineLimit]] has its estimate at once.
    */
  def whenCounted(rows: VisualRowCounts, topLine: Int, topVisualLine: Int, visibleRows: Int): Option[PaneScroll] =
    if rows.lineCount > ExactLineLimit then Some(of(rows, topLine, topVisualLine, visibleRows))
    else
      val _ = rows.rowsAboveAndTotal(topLine, (topLine + visibleRows).max(0), exact = false)
      Option.when(rows.allMeasured)(of(rows, topLine, topVisualLine, visibleRows))

/** A pane's scroll that waits for its lines to be measured: it becomes a [[PaneScroll]] once they are. */
final case class PendingScroll(rows: VisualRowCounts, topLine: Int, topVisualLine: Int, visibleRows: Int):

  def counted: Option[PaneScroll] = PaneScroll.whenCounted(rows, topLine, topVisualLine, visibleRows)
