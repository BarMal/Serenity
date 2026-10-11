package com.serenity.ui.tui

import java.awt.Color

/** An immutable snapshot of a [[TerminalScreenBuffer]]: the value model the compact [[CellGrid]] converts to and from.
  * Flushing never builds one -- it diffs the grids directly -- so a frame is for callers that want to look at the
  * screen, or hand [[TerminalAnsiDiff.emit]] two screens to compare.
  */
final case class TerminalFrame(width: Int, height: Int, cells: Vector[Vector[TerminalCell]]):
  def apply(x: Int, y: Int): TerminalCell = cells(y)(x)

object TerminalFrame:

  def blank(width: Int, height: Int, fg: Color = Color.WHITE, bg: Color = Color.BLACK): TerminalFrame =
    TerminalFrame(width, height, Vector.fill(height)(Vector.fill(width)(TerminalCell.blank(fg, bg))))
