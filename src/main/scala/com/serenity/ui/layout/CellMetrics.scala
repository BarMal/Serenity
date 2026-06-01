package com.serenity.ui.layout

/** Font-derived cell dimensions. Constant with respect to window size — resizing changes the cell count, not the cell
  * size. Derived from FontMetrics at startup and font changes only.
  */
case class CellMetrics(charWidth: Int, lineHeight: Int, ascent: Int):
  def toPixelX(col: Int): Int = col * charWidth
  def toPixelY(row: Int): Int = row * lineHeight
  def toCol(pixelX: Int): Int = pixelX / charWidth
  def toRow(pixelY: Int): Int = pixelY / lineHeight

  /** Derive a ViewportSize in cell units from a pixel viewport. Fractional cells at the right/bottom edge are excluded
    * — those pixels stay background-filled.
    */
  def viewportSize(pixelWidth: Int, pixelHeight: Int): ViewportSize =
    ViewportSize(pixelWidth / charWidth, pixelHeight / lineHeight)

object CellMetrics:

  /** Derive cell metrics from a java.awt.Font. Uses M-width as the nominal charWidth (consistent across monospaced and
    * variable-width fonts).
    */
  def fromFont(font: java.awt.Font): CellMetrics =
    val image = new java.awt.image.BufferedImage(1, 1, java.awt.image.BufferedImage.TYPE_INT_RGB)
    val g     = image.createGraphics()
    g.setFont(font)
    val fm     = g.getFontMetrics
    val result = CellMetrics(fm.charWidth('M'), fm.getHeight, fm.getAscent)
    g.dispose()
    result
