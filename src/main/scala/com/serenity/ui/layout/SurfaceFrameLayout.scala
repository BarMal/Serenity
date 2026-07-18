package com.serenity.ui.layout

import com.serenity.state.models.SurfaceContent

/** A device-independent rectangle used at the floating-surface boundary. */
case class LogicalPixelRect(x: Double, y: Double, width: Double, height: Double):
  def contains(pixelX: Double, pixelY: Double): Boolean =
    pixelX >= x && pixelX < x + width && pixelY >= y && pixelY < y + height

  def translated(deltaX: Double, deltaY: Double): LogicalPixelRect =
    copy(x = x + deltaX, y = y + deltaY)

/** Shared pixel geometry for a framed floating surface and its selectable rows. */
case class FloatingSurfaceGeometry(
    frame: LogicalPixelRect,
    content: LogicalPixelRect,
    itemRects: List[LogicalPixelRect]
):

  def translated(deltaX: Double, deltaY: Double): FloatingSurfaceGeometry =
    copy(
      frame = frame.translated(deltaX, deltaY),
      content = content.translated(deltaX, deltaY),
      itemRects = itemRects.map(_.translated(deltaX, deltaY))
    )

  def itemIndexAt(pixelX: Double, pixelY: Double): Option[Int] =
    itemRects.zipWithIndex.collectFirst { case (rect, index) if rect.contains(pixelX, pixelY) => index }

object FloatingSurfaceGeometry:

  /** Convert logical row spacing to the pixel coordinate space shared by layout, rendering, and hit testing. */
  def rowOffsetPixels(rows: Double, metrics: CellMetrics): Double =
    math.max(0.0, rows) * metrics.lineHeight

  def signedRowOffsetPixels(rows: Double, metrics: CellMetrics): Double =
    math.copySign(rowOffsetPixels(math.abs(rows), metrics), rows)

  /** Count only rows whose complete interactive rectangle fits in the available height. */
  def visibleItemCount(availableHeight: Double, itemHeight: Double, itemGapRows: Double): Int =
    val safeItemHeight = math.max(0.0, itemHeight)
    val step           = safeItemHeight * (1.0 + math.max(0.0, itemGapRows))
    if safeItemHeight <= 0.0 || step <= 0.0 || availableHeight < safeItemHeight then 0
    else math.floor((availableHeight - safeItemHeight) / step).toInt + 1

  def fromCells(
    frame: LayoutRect,
    metrics: CellMetrics,
    borderCells: Int,
    itemCount: Int,
    hasHeader: Boolean,
    hasFooter: Boolean,
    itemGapRows: Double
  ): FloatingSurfaceGeometry =
    val frameRect = LogicalPixelRect(
      frame.x * metrics.charWidth.toDouble,
      frame.y * metrics.lineHeight.toDouble,
      frame.width * metrics.charWidth.toDouble,
      frame.height * metrics.lineHeight.toDouble
    )
    val inset = math.max(0, borderCells)
    val contentRect = LogicalPixelRect(
      (frame.x + inset) * metrics.charWidth.toDouble,
      (frame.y + inset) * metrics.lineHeight.toDouble,
      math.max(0, frame.width - inset * 2) * metrics.charWidth.toDouble,
      math.max(0, frame.height - inset * 2) * metrics.lineHeight.toDouble
    )
    val itemStart = contentRect.y + (if hasHeader then metrics.lineHeight else 0)
    val usableHeight =
      contentRect.height - (if hasHeader then metrics.lineHeight else 0) - (if hasFooter then metrics.lineHeight else 0)
    val step         = metrics.lineHeight * (1.0 + math.max(0.0, itemGapRows))
    val visibleItems = visibleItemCount(usableHeight, metrics.lineHeight.toDouble, itemGapRows)
    val items =
      (0 until math.min(math.max(0, itemCount), visibleItems)).toList
        .map(index => LogicalPixelRect(contentRect.x, itemStart + index * step, contentRect.width, metrics.lineHeight))
    FloatingSurfaceGeometry(frameRect, contentRect, items)

case class SurfaceFrameLayout(
    frameRect: LayoutRect,
    borderCells: Int = SurfaceFrameLayout.DefaultBorderCells
):
  private val insetCells = math.max(0, borderCells)

  def contentRect: LayoutRect =
    LayoutRect(
      frameRect.x + insetCells,
      frameRect.y + insetCells,
      math.max(0, frameRect.width - (insetCells * 2)),
      math.max(0, frameRect.height - (insetCells * 2))
    )

  def maxContentRows: Int =
    contentRect.height

  def visibleItemRows(
    hasHeader: Boolean,
    hasFooter: Boolean,
    reservedContentRows: Int = 0,
    itemGapRows: Double = 0.0
  ): Int =
    val availableRows =
      math.max(0, maxContentRows - SurfaceFrameLayout.contentChromeRows(hasHeader, hasFooter, reservedContentRows))
    FloatingSurfaceGeometry.visibleItemCount(
      availableRows.toDouble,
      itemHeight = 1.0,
      itemGapRows = itemGapRows
    )

  def itemWindow(
    itemCount: Int,
    selectedIndex: Int,
    hasHeader: Boolean,
    hasFooter: Boolean,
    reservedContentRows: Int = 0,
    itemGapRows: Double = 0.0
  ): SurfaceItemWindow =
    val maxRows = visibleItemRows(hasHeader, hasFooter, reservedContentRows, itemGapRows)
    val offset =
      if itemCount <= maxRows then 0
      else
        val half = maxRows / 2
        math.max(0, math.min(selectedIndex - half, itemCount - maxRows))
    SurfaceItemWindow(offset, math.min(itemCount, maxRows))

  def itemIndexAt(
    row: Int,
    itemCount: Int,
    selectedIndex: Int,
    hasHeader: Boolean,
    hasFooter: Boolean,
    reservedContentRows: Int = 0,
    itemGapRows: Double = 0.0
  ): Option[Int] =
    val window      = itemWindow(itemCount, selectedIndex, hasHeader, hasFooter, reservedContentRows, itemGapRows)
    val itemRowBase = contentRect.y + (if hasHeader then 1 else 0)
    val itemRow     = row - itemRowBase
    val itemHeight  = 1.0 + math.max(0.0, itemGapRows)
    Option
      .when(itemRow >= 0)(math.floor(itemRow.toDouble / itemHeight).toInt)
      .filter(index => math.floor(index * itemHeight).toInt == itemRow)
      .flatMap(window.absoluteIndexAt)

  def contentRowSlots(
    itemCount: Int,
    hasHeader: Boolean,
    hasFooter: Boolean,
    itemGapRows: Double = 0.0
  ): List[SurfaceContentRowSlot] =
    SurfaceFrameLayout.contentRowSlotsFor(contentRect, itemCount, hasHeader, hasFooter, itemGapRows)

case class SurfaceItemWindow(offset: Int, rowCount: Int):
  def slice[A](items: List[A]): List[A] =
    items.slice(offset, offset + rowCount)

  def adjustedSelectedIndex(selectedIndex: Int): Int =
    selectedIndex - offset

  def absoluteIndexAt(displayedRow: Int): Option[Int] =
    Option.when(displayedRow >= 0 && displayedRow < rowCount)(offset + displayedRow)

enum SurfaceContentRowKind:
  case Header
  case Item(index: Int)
  case Footer

case class SurfaceContentRowSlot(kind: SurfaceContentRowKind, y: Int)

object SurfaceFrameLayout:
  val DefaultBorderCells: Int        = 1
  val CommandSurfaceBorderCells: Int = 0

  def contentRowSlotsFor(
    content: LayoutRect,
    itemCount: Int,
    hasHeader: Boolean,
    hasFooter: Boolean,
    itemGapRows: Double = 0.0
  ): List[SurfaceContentRowSlot] =
    if content.height <= 0 then Nil
    else
      val headerRows = if hasHeader then 1 else 0
      val footerRows = if hasFooter then 1 else 0
      val itemRows   = math.max(0, content.height - headerRows - footerRows)
      val itemHeight = 1.0 + math.max(0.0, itemGapRows)
      val visibleItems = FloatingSurfaceGeometry.visibleItemCount(
        itemRows.toDouble,
        itemHeight = 1.0,
        itemGapRows = itemGapRows
      )
      val itemSlots =
        (0 until math.min(itemCount, visibleItems)).toList.map { index =>
          SurfaceContentRowSlot(
            SurfaceContentRowKind.Item(index),
            content.y + headerRows + math.floor(index * itemHeight).toInt
          )
        }
      val headerSlots =
        if hasHeader then List(SurfaceContentRowSlot(SurfaceContentRowKind.Header, content.y))
        else Nil
      val footerSlots =
        if hasFooter && content.height > headerRows then
          List(SurfaceContentRowSlot(SurfaceContentRowKind.Footer, content.bottom - 1))
        else Nil

      headerSlots ++ itemSlots ++ footerSlots

  def borderCellsFor(content: SurfaceContent): Int =
    content match
      case SurfaceContent.CommandPalette(_) | SurfaceContent.CommandPaletteSubmenu(_, _, _) => CommandSurfaceBorderCells
      case _                                                                                => DefaultBorderCells

  def forContent(frameRect: LayoutRect, content: SurfaceContent): SurfaceFrameLayout =
    SurfaceFrameLayout(frameRect, borderCellsFor(content))

  def contentChromeRows(
    hasHeader: Boolean,
    hasFooter: Boolean,
    reservedContentRows: Int = 0
  ): Int =
    (if hasHeader then 1 else 0) +
      (if hasFooter then 1 else 0) +
      math.max(0, reservedContentRows)

  def frameChromeRows(
    hasHeader: Boolean,
    hasFooter: Boolean,
    reservedContentRows: Int = 0,
    borderCells: Int = DefaultBorderCells
  ): Int =
    (math.max(0, borderCells) * 2) + contentChromeRows(hasHeader, hasFooter, reservedContentRows)

  def frameHeightForItemRows(
    itemRows: Int,
    hasHeader: Boolean,
    hasFooter: Boolean,
    reservedContentRows: Int = 0,
    borderCells: Int = DefaultBorderCells,
    itemGapRows: Double = 0.0
  ): Int =
    val rows = math.max(0, itemRows)
    val gaps = math.ceil(math.max(0.0, rows - 1) * math.max(0.0, itemGapRows)).toInt
    rows + gaps + frameChromeRows(hasHeader, hasFooter, reservedContentRows, borderCells)
