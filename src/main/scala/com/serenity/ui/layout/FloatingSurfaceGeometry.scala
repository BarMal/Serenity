package com.serenity.ui.layout

/** A device-independent pixel rectangle used at the floating-surface boundary. */
case class LogicalPixelRect(x: Double, y: Double, width: Double, height: Double):
  def right: Double  = x + width
  def bottom: Double = y + height

  def contains(pixelX: Double, pixelY: Double): Boolean =
    pixelX >= x && pixelX < right && pixelY >= y && pixelY < bottom

/** A floating frame's cell fallback together with its unrounded logical row origin.
  *
  * The cell rectangle remains available to the editor layout contract, while render and pointer paths use
  * [[logicalFrame]] so a configured sub-row separation is not discarded before pixel conversion.
  */
case class FloatingSurfaceFramePlacement(cellRect: LayoutRect, yOffsetRows: Double = 0.0):
  private val normalizedYOffsetRows = yOffsetRows.max(0.0)

  def logicalFrame(metrics: CellMetrics): LogicalPixelRect =
    LogicalPixelRect(
      metrics.toPixelX(cellRect.x).toDouble,
      metrics.toPixelY(cellRect.y).toDouble + (normalizedYOffsetRows * metrics.lineHeight),
      (cellRect.width * metrics.charWidth).toDouble,
      (cellRect.height * metrics.lineHeight).toDouble
    )

  def geometry(
    metrics: CellMetrics,
    borderCells: Int = 0,
    itemGapRows: Double = 0.0
  ): FloatingSurfaceGeometry =
    FloatingSurfaceGeometry(
      frame = logicalFrame(metrics),
      cellWidthPx = metrics.charWidth.toDouble,
      lineHeightPx = metrics.lineHeight.toDouble,
      borderXPx = borderCells.max(0) * metrics.charWidth.toDouble,
      borderYPx = borderCells.max(0) * metrics.lineHeight.toDouble,
      itemGapRows = itemGapRows
    )

object FloatingSurfaceFramePlacement:

  def atRow(cellRect: LayoutRect, row: Double): FloatingSurfaceFramePlacement =
    val baseRow = math.floor(row).toInt
    FloatingSurfaceFramePlacement(cellRect.copy(y = baseRow), row - baseRow)

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
    FloatingSurfaceFramePlacement(frame).geometry(metrics, borderCells, itemGapRows)
