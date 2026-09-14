package com.serenity.ui.layout

final case class TextRowMetrics(
    contentRect: LayoutRect,
    gridMetrics: CellMetrics,
    rowLineHeightPx: Int,
    usesMeasuredLayout: Boolean,
    // Per-visual-row heights (measured/proportional rich text). Empty means every row is `rowLineHeightPx` tall, which
    // reproduces the old uniform stacking exactly. Entries of 0 also fall back to `rowLineHeightPx`.
    rowHeightsPx: Vector[Int] = Vector.empty
):
  private val contentTopPx: Int =
    gridMetrics.toPixelY(contentRect.y)

  private def heightOf(visualRow: Int): Int =
    rowHeightsPx.lift(visualRow).filter(_ > 0).getOrElse(rowLineHeightPx)

  /** Cumulative top pixel of each measured row (index i = top of row i; last entry = bottom of the final row). */
  private val measuredTops: Vector[Int] =
    if usesMeasuredLayout then rowHeightsPx.scanLeft(contentTopPx)((top, height) => top + math.max(1, height))
    else Vector.empty

  /** The measured height of a single visual row (the tallest run on it), or the grid row height in cell layout. */
  def rowHeightPx(visualRow: Int): Int =
    if usesMeasuredLayout then heightOf(visualRow) else gridMetrics.lineHeight

  def contentBottomPx: Int =
    gridMetrics.toPixelY(contentRect.bottom)

  def surfaceBottomPx(viewportHeightCells: Int): Int =
    gridMetrics.toPixelY(viewportHeightCells)

  def lineTopPx(visualRow: Int): Int =
    if usesMeasuredLayout then
      measuredTops
        .lift(visualRow)
        .getOrElse {
          val lastTop = measuredTops.lastOption.getOrElse(contentTopPx)
          lastTop + math.max(0, visualRow - math.max(0, measuredTops.length - 1)) * rowLineHeightPx
        }
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

  /** The visual row that owns an absolute surface pixel Y -- the inverse of [[lineTopPx]]. Used by mouse hit-testing so
    * a click on a tall heading row targets that row rather than a uniform-grid guess. Clamps above the first row and
    * below the last.
    */
  def visualRowAt(pixelY: Int): Int =
    if !usesMeasuredLayout then math.max(0, gridMetrics.toRow(pixelY) - contentRect.y)
    else if rowHeightsPx.isEmpty then math.max(0, (pixelY - contentTopPx) / math.max(1, rowLineHeightPx))
    else
      val firstBelow = measuredTops.indexWhere(_ > pixelY)
      val row        = if firstBelow < 0 then rowHeightsPx.length - 1 else firstBelow - 1
      row.max(0)

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
