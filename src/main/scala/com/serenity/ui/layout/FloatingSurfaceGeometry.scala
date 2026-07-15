package com.serenity.ui.layout

/** A logical-pixel rectangle used at the boundary between cell-based editor layout and floating UI. */
case class PixelRect(x: Double, y: Double, width: Double, height: Double):
  def right: Double  = x + width
  def bottom: Double = y + height

  def contains(pixelX: Double, pixelY: Double): Boolean =
    pixelX >= x && pixelX < right && pixelY >= y && pixelY < bottom

  def translated(deltaX: Double, deltaY: Double): PixelRect = copy(x = x + deltaX, y = y + deltaY)

/** The authoritative geometry for a floating surface. Editor anchors remain in cells; all spacing and hit targets
  * within the surface are expressed in logical pixels so fractional rows never alter text cell height.
  */
case class FloatingSurfaceGeometry(
    frame: PixelRect,
    content: PixelRect,
    headerRect: Option[PixelRect],
    itemRects: List[PixelRect],
    footerRect: Option[PixelRect],
    itemWindow: SurfaceItemWindow,
    cellMetrics: CellMetrics
):

  def translated(deltaX: Double, deltaY: Double): FloatingSurfaceGeometry =
    copy(
      frame = frame.translated(deltaX, deltaY),
      content = content.translated(deltaX, deltaY),
      headerRect = headerRect.map(_.translated(deltaX, deltaY)),
      itemRects = itemRects.map(_.translated(deltaX, deltaY)),
      footerRect = footerRect.map(_.translated(deltaX, deltaY))
    )

  def itemIndexAt(pixelX: Double, pixelY: Double): Option[Int] =
    itemRects.zipWithIndex.collectFirst { case (rect, index) if rect.contains(pixelX, pixelY) => index }

  /** Prefer physical event coordinates while retaining synthetic cell-coordinate event compatibility. */
  def itemIndexAt(
    pixelX: Option[Int],
    pixelY: Option[Int],
    cellX: Int,
    cellY: Int
  ): Option[Int] =
    val x = pixelX.fold(cellMetrics.toPixelX(cellX).toDouble)(_.toDouble)
    val y = pixelY.fold(cellMetrics.toPixelY(cellY).toDouble)(_.toDouble)
    itemIndexAt(x, y)

object FloatingSurfaceGeometry:

  def forItems(
    frame: LayoutRect,
    metrics: CellMetrics,
    itemCount: Int,
    itemGapRows: Double,
    borderCells: Int = 0,
    headerRows: Int = 0,
    footerRows: Int = 0,
    reservedContentRows: Int = 0,
    selectedIndex: Int = 0
  ): FloatingSurfaceGeometry =
    val framePx = PixelRect(
      metrics.toPixelX(frame.x).toDouble,
      metrics.toPixelY(frame.y).toDouble,
      metrics.toPixelX(frame.width).toDouble,
      metrics.toPixelY(frame.height).toDouble
    )
    val insetX = metrics.toPixelX(borderCells.max(0)).toDouble
    val insetY = metrics.toPixelY(borderCells.max(0)).toDouble
    val content = PixelRect(
      framePx.x + insetX,
      framePx.y + insetY,
      (framePx.width - (insetX * 2)).max(0.0),
      (framePx.height - (insetY * 2)).max(0.0)
    )
    val itemHeight          = metrics.lineHeight.toDouble.max(0.0)
    val itemStride          = itemHeight + (itemHeight * itemGapRows.max(0.0))
    val firstItemY          = content.y + (itemHeight * headerRows.max(0))
    val chromeHeight        = itemHeight * (headerRows.max(0) + footerRows.max(0) + reservedContentRows.max(0))
    val availableItemHeight = (content.height - chromeHeight).max(0.0)
    val visibleCount =
      if itemHeight <= 0.0 || availableItemHeight < itemHeight then 0
      else math.floor((availableItemHeight + itemStride - itemHeight) / itemStride).toInt.max(0)
    val windowRows = visibleCount.max(1)
    val windowOffset =
      if itemCount <= windowRows then 0
      else
        val half = windowRows / 2
        math.max(0, math.min(selectedIndex - half, itemCount - windowRows))
    val itemWindow = SurfaceItemWindow(windowOffset, math.min(itemCount.max(0), visibleCount))
    val items =
      (0 until itemWindow.rowCount).toList.map(index =>
        PixelRect(content.x, firstItemY + (index * itemStride), content.width, itemHeight)
      )
    val headerRect =
      Option.when(headerRows > 0)(PixelRect(content.x, content.y, content.width, itemHeight * headerRows))
    val footerRect = Option.when(footerRows > 0)(
      PixelRect(content.x, content.bottom - (itemHeight * footerRows), content.width, itemHeight * footerRows)
    )
    FloatingSurfaceGeometry(framePx, content, headerRect, items, footerRect, itemWindow, metrics)
