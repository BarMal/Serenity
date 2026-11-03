package com.serenity.ui.layout

/** The last place a pane can rest after scrolling: the line, and the visual row within it, that sit at the top of a
  * screen whose bottom is the end of the document.
  */
final case class ScrollLimit(line: Int, row: Int):

  /** `(line, row)` itself, or this limit when it lies below it. */
  def clamp(line: Int, row: Int): (Int, Int) =
    if line > this.line || (line == this.line && row > this.row) then (this.line, this.row) else (line, row)

/** How far a pane scrolls, counted in the rows the pane's own font draws. The wheel, cursor following, the cursor's
  * bottom clamp and the scrollbar all ask here, so a document ends on the same row for each of them.
  */
object ScrollLimits:

  /** The whole rows of `rowHeightPx` in a pane `paneHeightPx` tall, at least one. */
  def rowsThatFit(paneHeightPx: Int, rowHeightPx: Int): Int =
    math.max(1, paneHeightPx / math.max(1, rowHeightPx))

  /** The last resting place of a pane that fits `rowsThatFit` rows: the final screen of a document of `rows`. */
  def last(rows: VisualRowCounts, rowsThatFit: Int): ScrollLimit =
    val (line, row) = rows.rowAbove(rows.lineCount, rowsThatFit)
    ScrollLimit(line, row)
