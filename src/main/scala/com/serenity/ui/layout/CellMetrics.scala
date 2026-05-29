package com.serenity.ui.layout

/** Font-derived cell dimensions. Constant with respect to window size — resizing changes the
 *  cell count, not the cell size. Derived from FontMetrics at startup and font changes only.
 */
case class CellMetrics(charWidth: Int, lineHeight: Int, ascent: Int):
  def toPixelX(col: Int): Int  = col * charWidth
  def toPixelY(row: Int): Int  = row * lineHeight
  def toCol(pixelX: Int): Int  = pixelX / charWidth
  def toRow(pixelY: Int): Int  = pixelY / lineHeight

  /** Derive a TerminalSize in cell units from a pixel viewport.
   *  Fractional cells at the right/bottom edge are excluded — those pixels stay background-filled.
   */
  def terminalSize(pixelWidth: Int, pixelHeight: Int): TerminalSize =
    TerminalSize(pixelWidth / charWidth, pixelHeight / lineHeight)
