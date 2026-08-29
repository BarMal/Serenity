package com.serenity.ui.layout

final case class TextRowMetrics(
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

  /** A small nudge that lifts a cell-grid cursor's top edge slightly above its row's own top, for a real sub-pixel font
    * (a monospaced code font, say 16px tall) where a couple of spare pixels make the caret look better centred. The
    * `2`-pixel floor below assumes a row several pixels tall to spare; on a grid whose row *is* the pixel unit (TUI's
    * `CellMetricsOne`, `rowLineHeightPx == 1`), that same floor would lift the cursor a full row or more off its real
    * position, so the lift is capped to never exceed half the row -- vanishing entirely once a row has no pixels to
    * spare.
    */
  private def cursorOpticalLiftPx: Int =
    math.min(rowLineHeightPx / 2, math.max(2, math.round(rowLineHeightPx.toFloat * 0.125f)))
