package com.serenity.ui.layout

/** Font-derived cell dimensions. Constant with respect to window size — resizing changes the cell count, not the cell
  * size. Derived from FontMetrics at startup and font changes only.
  */
case class CellMetrics(charWidth: Int, lineHeight: Int, ascent: Int):
  def isValid: Boolean        = charWidth > 0 && lineHeight > 0
  def toPixelX(col: Int): Int = col * charWidth
  def toPixelY(row: Int): Int = row * lineHeight
  def toPixelX(col: Double): Double = col * charWidth
  def toPixelY(row: Double): Double = row * lineHeight
  def toCol(pixelX: Int): Int = if charWidth > 0 then pixelX / charWidth else 0
  def toRow(pixelY: Int): Int = if lineHeight > 0 then pixelY / lineHeight else 0

  /** Derive a ViewportSize in cell units from a pixel viewport. Fractional cells at the right/bottom edge are excluded
    * — those pixels stay background-filled.
    */
  def viewportSize(pixelWidth: Int, pixelHeight: Int): ViewportSize =
    ViewportSize(
      if charWidth > 0 then pixelWidth / charWidth else 1,
      if lineHeight > 0 then pixelHeight / lineHeight else 1
    )

object CellMetrics:

  def max(metrics: CellMetrics*): CellMetrics =
    val nonEmpty = metrics.toList
    CellMetrics(
      charWidth = nonEmpty.map(_.charWidth).maxOption.getOrElse(1),
      lineHeight = nonEmpty.map(_.lineHeight).maxOption.getOrElse(1),
      ascent = nonEmpty.map(_.ascent).maxOption.getOrElse(1)
    )

  /** Derive cell metrics from a java.awt.Font. Uses M-width as the nominal charWidth (consistent across monospaced and
    * variable-width fonts).
    */
  def fromFont(font: java.awt.Font): CellMetrics =
    val image = new java.awt.image.BufferedImage(1, 1, java.awt.image.BufferedImage.TYPE_INT_RGB)
    val g     = image.createGraphics()
    g.setFont(font)
    val fm     = g.getFontMetrics
    val result = CellMetrics(math.max(1, fm.charWidth('M')), math.max(1, fm.getHeight), math.max(1, fm.getAscent))
    g.dispose()
    result
