package com.serenity.ui.tui

import java.awt.Color

import com.serenity.ui.theme.TextStyle

/** One rendered frame as the terminal received it: the cell grid, the caret, the terminal-side modes, and the bytes
  * this particular render wrote. Snapshots are immutable, so a spec can hold the screen from before an interaction and
  * compare it against the screen after -- which is how "only the status bar changed" becomes an assertion rather than a
  * hope.
  */
final case class TuiScreen(terminal: TerminalEmulator, emitted: String):

  export terminal.{
    bracketedPasteEnabled,
    cellAt,
    cursor,
    find,
    focusReportingEnabled,
    frame,
    height,
    inAlternateScreen,
    mouseTrackingEnabled,
    render,
    rowText,
    rows,
    rowsContaining,
    width
  }

  /** The whole grid as one newline-joined string, for a coarse `should include` when the exact row does not matter. */
  def text: String = rows.mkString("\n")

  /** Only the rows with something on them, paired with their row index -- the readable form of a mostly-empty screen. */
  def paintedRows: List[(Int, String)] =
    rows.zipWithIndex.collect { case (line, row) if line.strip().nonEmpty => (row, line.stripTrailing()) }.toList

  def containsText(value: String): Boolean = rows.exists(_.contains(value))

  /** The chrome rows the editor always draws: the buffer header across the top, the status bar along the bottom. */
  def titleBar: String  = rowText(0)
  def statusBar: String = rowText(height - 1)

  /** The editor body: everything between the two chrome rows, which is where document content and the gutter live. */
  def bodyRows: Vector[String] = rows.slice(1, height - 1)

  def rowOf(value: String): Option[Int] = terminal.rowsContaining(value).headOption

  /** Where the terminal's own cursor sits, as `(column, row)`. */
  def caret: (Int, Int) = (cursor.col, cursor.row)

  def caretVisible: Boolean = cursor.visible

  /** A vertical slice, one character per row -- the readable way to assert on the gutter or a pane divider. */
  def column(col: Int): String = (0 until height).map(row => cellAt(col, row).text).mkString

  def foregroundAt(col: Int, row: Int): Color = cellAt(col, row).fg
  def backgroundAt(col: Int, row: Int): Color = cellAt(col, row).bg
  def styleAt(col: Int, row: Int): TextStyle  = cellAt(col, row).style
  def textAt(col: Int, row: Int, length: Int): String =
    (col until math.min(col + length, width)).map(x => cellAt(x, row).text).mkString

  /** Every cell whose content, colour or style differs from `previous`. Empty means the two frames are identical, which
    * for a terminal means the second one cost nothing to draw.
    */
  def changedCells(previous: TuiScreen): Set[(Int, Int)] =
    val cells =
      for
        row <- 0 until math.min(height, previous.height)
        col <- 0 until math.min(width, previous.width)
        if cellAt(col, row) != previous.cellAt(col, row)
      yield (col, row)
    cells.toSet

  def changedRows(previous: TuiScreen): Set[Int] = changedCells(previous).map(_._2)

  /** The distinct background colours in use, most-used first -- a cheap way to assert that a highlight, selection or
    * overlay actually painted something distinguishable rather than blending into the body.
    */
  def backgroundHistogram: List[(Color, Int)] =
    val counts =
      for
        row <- 0 until height
        col <- 0 until width
      yield cellAt(col, row).bg
    counts.groupBy(identity).view.mapValues(_.size).toList.sortBy(-_._2)

end TuiScreen
