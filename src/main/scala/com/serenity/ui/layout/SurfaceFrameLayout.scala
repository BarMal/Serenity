package com.serenity.ui.layout

import com.serenity.state.models.SurfaceContent

/** A device-independent rectangle used at the floating-surface boundary. */
case class LogicalPixelRect(x: Double, y: Double, width: Double, height: Double):
  def contains(pixelX: Double, pixelY: Double): Boolean =
    pixelX >= x && pixelX < x + width && pixelY >= y && pixelY < y + height

/** Shared pixel geometry for a framed floating surface and its selectable rows. */
case class FloatingSurfaceGeometry(
    frame: LogicalPixelRect,
    content: LogicalPixelRect,
    itemRects: List[LogicalPixelRect]
):
  def itemIndexAt(pixelX: Double, pixelY: Double): Option[Int] =
    itemRects.zipWithIndex.collectFirst { case (rect, index) if rect.contains(pixelX, pixelY) => index }

object FloatingSurfaceGeometry:

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
    val step = metrics.lineHeight * (1.0 + math.max(0.0, itemGapRows))
    val items =
      (0 until math.max(0, itemCount)).toList
        .map(index => LogicalPixelRect(contentRect.x, itemStart + index * step, contentRect.width, metrics.lineHeight))
        .takeWhile(rect => rect.y + rect.height <= itemStart + math.max(0.0, usableHeight))
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
    val itemHeight = 1.0 + math.max(0.0, itemGapRows)
    math.floor(availableRows.toDouble / itemHeight).toInt

  def itemWindow(
    itemCount: Int,
    selectedIndex: Int,
    hasHeader: Boolean,
    hasFooter: Boolean,
    reservedContentRows: Int = 0,
    itemGapRows: Double = 0.0
  ): SurfaceItemWindow =
    val maxRows = math.max(1, visibleItemRows(hasHeader, hasFooter, reservedContentRows, itemGapRows))
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
      val headerRows   = if hasHeader then 1 else 0
      val footerRows   = if hasFooter then 1 else 0
      val itemRows     = math.max(0, content.height - headerRows - footerRows)
      val itemHeight   = 1.0 + math.max(0.0, itemGapRows)
      val visibleItems = math.floor(itemRows.toDouble / itemHeight).toInt
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
