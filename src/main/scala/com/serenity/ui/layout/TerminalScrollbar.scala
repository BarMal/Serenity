package com.serenity.ui.layout

/** What a terminal scrollbar shows in one cell of its column. The thumb is placed to half a row, so a thumb that ends
  * or starts half-way down a cell is drawn with a glyph heavy on the half it occupies.
  */
enum ScrollGlyph(val char: Char):
  case Track extends ScrollGlyph('│')

  /** The thumb fills the cell. */
  case Thumb extends ScrollGlyph('┃')

  /** The thumb fills the cell's upper half and ends there. */
  case ThumbEndsHere extends ScrollGlyph('╿')

  /** The thumb fills the cell's lower half and starts there. */
  case ThumbStartsHere extends ScrollGlyph('╽')

/** The terminal scrollbar's arithmetic as a pure function: a [[ScrollExtent]] over a column of cells becomes the glyph
  * for each cell, using the same thumb [[ScrollbarGeometry]] places for pixels, in half-rows.
  */
object TerminalScrollbar:

  /** A thumb is never shorter than a cell. */
  private val MinThumbHalfRows: Int = 2

  /** The glyph of each of `rows` cells, top to bottom, or `None` when there is nothing to scroll. */
  def glyphs(extent: ScrollExtent, rows: Int): Option[Vector[ScrollGlyph]] =
    ScrollbarGeometry.thumb(extent, rows * 2, MinThumbHalfRows).map { thumb =>
      Vector.tabulate(rows)(row => glyphAt(thumb, row))
    }

  private def glyphAt(thumb: ScrollThumb, row: Int): ScrollGlyph =
    (thumb.contains(row * 2), thumb.contains(row * 2 + 1)) match
      case (true, true)  => ScrollGlyph.Thumb
      case (true, false) => ScrollGlyph.ThumbEndsHere
      case (false, true) => ScrollGlyph.ThumbStartsHere
      case _             => ScrollGlyph.Track
