package com.serenity.ui.layout

case class TextRowMetrics(
    contentRect: LayoutRect,
    gridMetrics: CellMetrics,
    rowLineHeightPx: Int,
    usesMeasuredLayout: Boolean
):
  private val contentTopPx: Int =
    gridMetrics.toPixelY(contentRect.y)

  def contentBottomPx: Int =
    gridMetrics.toPixelY(contentRect.bottom)

  def surfaceBottomPx(viewportHeightCells: Int): Int =
    gridMetrics.toPixelY(viewportHeightCells)

  def lineTopPx(visualRow: Int): Int =
    if usesMeasuredLayout then contentTopPx + visualRow * rowLineHeightPx
    else gridMetrics.toPixelY(contentRect.y + visualRow)

  def lineFits(visualRow: Int): Boolean =
    if usesMeasuredLayout then lineTopPx(visualRow) < contentBottomPx
    else visualRow < contentRect.height

  def lineVisible(visualRow: Int, viewportHeightCells: Int): Boolean =
    if usesMeasuredLayout then
      val topPx = lineTopPx(visualRow)
      topPx >= contentTopPx &&
      topPx < contentBottomPx &&
      topPx < surfaceBottomPx(viewportHeightCells)
    else
      val row = contentRect.y + visualRow
      row >= 0 && row < contentRect.bottom && row < viewportHeightCells

  def cursorTopPx(visualRow: Int): Int =
    if usesMeasuredLayout then lineTopPx(visualRow)
    else math.max(contentTopPx, lineTopPx(visualRow) - cursorOpticalLiftPx)

  private def cursorOpticalLiftPx: Int =
    math.max(2, math.round(rowLineHeightPx.toFloat * 0.125f))
