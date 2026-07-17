package com.serenity.ui.layout

/** A device-independent pixel rectangle used at the floating-surface boundary. */
case class LogicalPixelRect(x: Double, y: Double, width: Double, height: Double):
  def right: Double = x + width
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
    borderPx: Double = 0.0,
    itemGapRows: Double = 0.0
):
  private val normalizedBorderPx = borderPx.max(0.0)
  private val normalizedGapPx = itemGapRows.max(0.0) * lineHeightPx.max(0.0)

  def contentRect: LogicalPixelRect =
    LogicalPixelRect(
      frame.x + normalizedBorderPx,
      frame.y + normalizedBorderPx,
      (frame.width - normalizedBorderPx * 2).max(0.0),
      (frame.height - normalizedBorderPx * 2).max(0.0)
    )

  def itemRect(index: Int, headerRows: Int = 0): LogicalPixelRect =
    val rowHeight = lineHeightPx.max(0.0)
    val offset = headerRows.max(0) * rowHeight + index.max(0) * (rowHeight + normalizedGapPx)
    LogicalPixelRect(contentRect.x, contentRect.y + offset, contentRect.width, rowHeight)

  def itemIndexAt(pixelX: Double, pixelY: Double, itemCount: Int, headerRows: Int = 0): Option[Int] =
    (0 until itemCount.max(0)).find(index => itemRect(index, headerRows).contains(pixelX, pixelY))
