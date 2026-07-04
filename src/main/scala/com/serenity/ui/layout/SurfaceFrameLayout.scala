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
    reservedContentRows: Int = 0
  ): Int =
    math.max(
      0,
      maxContentRows - SurfaceFrameLayout.contentChromeRows(hasHeader, hasFooter, reservedContentRows)
    )

  def itemWindow(
    itemCount: Int,
    selectedIndex: Int,
    hasHeader: Boolean,
    hasFooter: Boolean,
    reservedContentRows: Int = 0
  ): SurfaceItemWindow =
    val maxRows = math.max(1, visibleItemRows(hasHeader, hasFooter, reservedContentRows))
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
    reservedContentRows: Int = 0
  ): Option[Int] =
    val window      = itemWindow(itemCount, selectedIndex, hasHeader, hasFooter, reservedContentRows)
    val itemRowBase = contentRect.y + (if hasHeader then 1 else 0)
    val itemRow     = row - itemRowBase
    window.absoluteIndexAt(itemRow)

  def contentRowSlots(
    itemCount: Int,
    hasHeader: Boolean,
    hasFooter: Boolean
  ): List[SurfaceContentRowSlot] =
    val content = contentRect
    if content.height <= 0 then Nil
    else
      val headerRows = if hasHeader then 1 else 0
      val footerRows = if hasFooter then 1 else 0
      val itemRows   = math.max(0, content.height - headerRows - footerRows)
      val itemSlots =
        (0 until math.min(itemCount, itemRows)).toList.map { index =>
          SurfaceContentRowSlot(SurfaceContentRowKind.Item(index), content.y + headerRows + index)
        }
      val headerSlots =
        if hasHeader then List(SurfaceContentRowSlot(SurfaceContentRowKind.Header, content.y))
        else Nil
      val footerSlots =
        if hasFooter && content.height > headerRows then
          List(SurfaceContentRowSlot(SurfaceContentRowKind.Footer, content.bottom - 1))
        else Nil

      headerSlots ++ itemSlots ++ footerSlots

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
    borderCells: Int = DefaultBorderCells
  ): Int =
    math.max(0, itemRows) + frameChromeRows(hasHeader, hasFooter, reservedContentRows, borderCells)
