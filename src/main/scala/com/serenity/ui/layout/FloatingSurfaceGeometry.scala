package com.serenity.ui.layout

/** A device-independent pixel rectangle used at the floating-surface boundary. */
case class LogicalPixelRect(x: Double, y: Double, width: Double, height: Double):
  def right: Double  = x + width
  def bottom: Double = y + height

  def contains(pixelX: Double, pixelY: Double): Boolean =
    pixelX >= x && pixelX < right && pixelY >= y && pixelY < bottom

/** Authoritative geometry for a floating surface after cell anchors become logical pixels.
  *
  * Text continues to use integer cell baselines; only frame, inset, spacing, and hit regions use this contract.
  */
case class FloatingSurfaceGeometry(
    frame: LogicalPixelRect,
    cellWidthPx: Double,
    lineHeightPx: Double,
    borderXPx: Double = 0.0,
    borderYPx: Double = 0.0,
    itemGapRows: Double = 0.0
):
  private val normalizedBorderXPx = borderXPx.max(0.0)
  private val normalizedBorderYPx = borderYPx.max(0.0)
  private val normalizedGapPx     = itemGapRows.max(0.0) * lineHeightPx.max(0.0)

  def contentRect: LogicalPixelRect =
    LogicalPixelRect(
      frame.x + normalizedBorderXPx,
      frame.y + normalizedBorderYPx,
      (frame.width - normalizedBorderXPx * 2).max(0.0),
      (frame.height - normalizedBorderYPx * 2).max(0.0)
    )

  def itemRect(index: Int, headerRows: Int = 0): LogicalPixelRect =
    val rowHeight = lineHeightPx.max(0.0)
    val offset    = headerRows.max(0) * rowHeight + index.max(0) * (rowHeight + normalizedGapPx)
    LogicalPixelRect(contentRect.x, contentRect.y + offset, contentRect.width, rowHeight)

  def itemIndexAt(pixelX: Double, pixelY: Double, itemCount: Int, headerRows: Int = 0): Option[Int] =
    (0 until itemCount.max(0)).find(index => itemRect(index, headerRows).contains(pixelX, pixelY))

object FloatingSurfaceGeometry:

  /** Cell-layout fallback used only to reserve enough integer viewport rows for a pixel surface. */
  def reservedCellRows(rows: Double): Int =
    math.ceil(rows.max(0.0)).toInt

  def fromCells(
    frame: LayoutRect,
    metrics: CellMetrics,
    borderCells: Int = 0,
    itemGapRows: Double = 0.0
  ): FloatingSurfaceGeometry =
    FloatingSurfaceGeometry(
      frame = LogicalPixelRect(
        metrics.toPixelX(frame.x).toDouble,
        metrics.toPixelY(frame.y).toDouble,
        (frame.width * metrics.charWidth).toDouble,
        (frame.height * metrics.lineHeight).toDouble
      ),
      cellWidthPx = metrics.charWidth.toDouble,
      lineHeightPx = metrics.lineHeight.toDouble,
      borderXPx = borderCells.max(0) * metrics.charWidth.toDouble,
      borderYPx = borderCells.max(0) * metrics.lineHeight.toDouble,
      itemGapRows = itemGapRows
    )
