package com.serenity.ui.layout

import com.serenity.state.models.SurfaceContent

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
    val itemHeight = itemGapRows.max(0.0) + 1.0
    if availableRows == 0 then 0 else math.floor((availableRows + itemHeight - 1.0) / itemHeight).toInt

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
    val window         = itemWindow(itemCount, selectedIndex, hasHeader, hasFooter, reservedContentRows, itemGapRows)
    val itemRowBase    = contentRect.y + (if hasHeader then 1 else 0)
    val itemRow        = row - itemRowBase
    val itemHeight     = itemGapRows.max(0.0) + 1.0
    val displayedIndex = math.floor(itemRow / itemHeight).toInt
    val itemStart      = displayedIndex * itemHeight
    Option
      .when(itemRow >= 0 && itemRow >= itemStart && itemRow < itemStart + 1.0)(displayedIndex)
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
      val itemHeight   = itemGapRows.max(0.0) + 1.0
      val visibleItems = if itemRows == 0 then 0 else math.floor((itemRows + itemHeight - 1.0) / itemHeight).toInt
      val itemSlots =
        (0 until math.min(itemCount, visibleItems)).toList.map { index =>
          SurfaceContentRowSlot(
            SurfaceContentRowKind.Item(index),
            content.y + headerRows + math.round(index * itemHeight).toInt
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
    val gaps = math.max(0, rows - 1) * itemGapRows.max(0.0)
    math.round(rows + gaps + frameChromeRows(hasHeader, hasFooter, reservedContentRows, borderCells)).toInt
