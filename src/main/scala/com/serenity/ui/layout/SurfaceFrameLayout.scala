package com.serenity.ui.layout

import com.serenity.config.InterfaceDensity
import com.serenity.state.models.SurfaceContent

/** A device-independent rectangle used at the floating-surface boundary. */
final case class LogicalPixelRect(x: Double, y: Double, width: Double, height: Double):
  /** Exclusive right edge. */
  def right: Double = x + width

  /** Exclusive bottom edge. */
  def bottom: Double = y + height

  /** Whether the supplied logical pixel is inside this rectangle. */
  def contains(pixelX: Double, pixelY: Double): Boolean =
    pixelX >= x && pixelX < right && pixelY >= y && pixelY < bottom

  /** Whether this rectangle fully contains another rectangle. */
  def containsRect(rect: LogicalPixelRect): Boolean =
    rect.x >= x && rect.y >= y && rect.right <= right && rect.bottom <= bottom

  /** The positive-area overlap with another rectangle. */
  def intersection(other: LogicalPixelRect): Option[LogicalPixelRect] =
    val left   = math.max(x, other.x)
    val top    = math.max(y, other.y)
    val right  = math.min(this.right, other.right)
    val bottom = math.min(this.bottom, other.bottom)
    Option.when(right > left && bottom > top)(LogicalPixelRect(left, top, right - left, bottom - top))

  /** Translate this rectangle without changing its size. */
  def translated(deltaX: Double, deltaY: Double): LogicalPixelRect =
    copy(x = x + deltaX, y = y + deltaY)

/** Shared pixel geometry for a framed floating surface and its selectable rows. */
final case class FloatingSurfaceGeometry(
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
    val step           = safeItemHeight + math.max(0.0, itemGapRows)
    if safeItemHeight <= 0.0 || step <= 0.0 || availableHeight < safeItemHeight then 0
    else math.floor((availableHeight - safeItemHeight) / step).toInt + 1

  def fromCells(
    frame: LayoutRect,
    metrics: CellMetrics,
    borderCells: Int,
    itemCount: Int,
    hasHeader: Boolean,
    hasFooter: Boolean,
    itemGapRows: Double,
    itemTargetRows: Int = 1,
    hasKeyHint: Boolean = false
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
      contentRect.height - (if hasHeader then metrics.lineHeight else 0) - (if hasFooter then metrics.lineHeight
                                                                            else 0) -
        (if hasKeyHint then metrics.lineHeight else 0)
    val targetHeight = math.max(1, itemTargetRows) * metrics.lineHeight.toDouble
    val step         = targetHeight + rowOffsetPixels(itemGapRows, metrics)
    val visibleItems = visibleItemCount(usableHeight, targetHeight, rowOffsetPixels(itemGapRows, metrics))
    val items =
      (0 until math.min(math.max(0, itemCount), visibleItems)).toList
        .map(index => LogicalPixelRect(contentRect.x, itemStart + index * step, contentRect.width, targetHeight))
    FloatingSurfaceGeometry(frameRect, contentRect, items)

final case class SurfaceFrameLayout(
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
    itemGapRows: Double = 0.0,
    itemTargetRows: Int = 1,
    hasKeyHint: Boolean = false
  ): Int =
    val availableRows = math.max(
      0,
      maxContentRows - SurfaceFrameLayout.contentChromeRows(hasHeader, hasFooter, reservedContentRows, hasKeyHint)
    )
    FloatingSurfaceGeometry.visibleItemCount(
      availableRows.toDouble,
      itemHeight = math.max(1, itemTargetRows).toDouble,
      itemGapRows = itemGapRows
    )

  def itemWindow(
    itemCount: Int,
    selectedIndex: Int,
    hasHeader: Boolean,
    hasFooter: Boolean,
    reservedContentRows: Int = 0,
    itemGapRows: Double = 0.0,
    itemTargetRows: Int = 1,
    hasKeyHint: Boolean = false
  ): SurfaceItemWindow =
    // `reservedContentRows` (the selected item's own expand-in-place preview, e.g. a settings group's capped child
    // list) shares the same row budget sibling items compete for -- at high density/preview-row counts that budget
    // can hit zero. Sibling rows are the ones allowed to disappear to make room for the preview; the selected item's
    // own row must not, so a positive `itemCount` always keeps at least one row in the window.
    val maxRows =
      if itemCount <= 0 then 0
      else
        math.max(1, visibleItemRows(hasHeader, hasFooter, reservedContentRows, itemGapRows, itemTargetRows, hasKeyHint))
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
    itemGapRows: Double = 0.0,
    itemTargetRows: Int = 1,
    hasKeyHint: Boolean = false
  ): Option[Int] =
    val window = itemWindow(
      itemCount,
      selectedIndex,
      hasHeader,
      hasFooter,
      reservedContentRows,
      itemGapRows,
      itemTargetRows,
      hasKeyHint
    )
    val itemRowBase = contentRect.y + (if hasHeader then 1 else 0)
    val itemRow     = row - itemRowBase
    val targetRows  = math.max(1, itemTargetRows)
    val itemHeight  = targetRows + math.max(0.0, itemGapRows)
    Option
      .when(itemRow >= 0)(math.floor(itemRow.toDouble / itemHeight).toInt)
      .filter(index => itemRow - math.floor(index * itemHeight).toInt < targetRows)
      .flatMap(window.absoluteIndexAt)

  def contentRowSlots(
    itemCount: Int,
    hasHeader: Boolean,
    hasFooter: Boolean,
    itemGapRows: Double = 0.0,
    itemTargetRows: Int = 1,
    hasKeyHint: Boolean = false
  ): List[SurfaceContentRowSlot] =
    SurfaceFrameLayout.contentRowSlotsFor(
      contentRect,
      itemCount,
      hasHeader,
      hasFooter,
      itemGapRows,
      itemTargetRows,
      hasKeyHint
    )

final case class SurfaceItemWindow(offset: Int, rowCount: Int):
  def slice[A](items: List[A]): List[A] =
    items.slice(offset, offset + rowCount)

  def adjustedSelectedIndex(selectedIndex: Int): Int =
    selectedIndex - offset

  def absoluteIndexAt(displayedRow: Int): Option[Int] =
    Option.when(displayedRow >= 0 && displayedRow < rowCount)(offset + displayedRow)

enum SurfaceContentRowKind:
  case Header
  case Item(index: Int)
  case KeyHint
  case Footer

final case class SurfaceContentRowSlot(kind: SurfaceContentRowKind, y: Int)

object SurfaceFrameLayout:
  val DefaultBorderCells: Int        = 1
  val CommandSurfaceBorderCells: Int = DefaultBorderCells

  def contentRowSlotsFor(
    content: LayoutRect,
    itemCount: Int,
    hasHeader: Boolean,
    hasFooter: Boolean,
    itemGapRows: Double = 0.0,
    itemTargetRows: Int = 1,
    hasKeyHint: Boolean = false
  ): List[SurfaceContentRowSlot] =
    if content.height <= 0 then Nil
    else
      val headerRows  = if hasHeader then 1 else 0
      val footerRows  = if hasFooter then 1 else 0
      val keyHintRows = if hasKeyHint then 1 else 0
      val itemRows    = math.max(0, content.height - headerRows - footerRows - keyHintRows)
      val targetRows  = math.max(1, itemTargetRows)
      val itemHeight  = targetRows + math.max(0.0, itemGapRows)
      val visibleItems = FloatingSurfaceGeometry.visibleItemCount(
        itemRows.toDouble,
        itemHeight = targetRows.toDouble,
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
      // The key-hint row sits directly above the footer (or at the very bottom when there is no footer) -- it is
      // persistent chrome, distinct from the transient status-message footer slot (issue #931, Stage 3).
      val keyHintSlots =
        if hasKeyHint && content.height > headerRows then
          List(SurfaceContentRowSlot(SurfaceContentRowKind.KeyHint, content.bottom - 1 - footerRows))
        else Nil
      val footerSlots =
        if hasFooter && content.height > headerRows then
          List(SurfaceContentRowSlot(SurfaceContentRowKind.Footer, content.bottom - 1))
        else Nil

      headerSlots ++ itemSlots ++ keyHintSlots ++ footerSlots

  def borderCellsFor(content: SurfaceContent): Int =
    content match
      case SurfaceContent.CommandPalette(_) | SurfaceContent.CommandRunnerPeek(_) => CommandSurfaceBorderCells
      case _                                                                      => DefaultBorderCells

  def forContent(frameRect: LayoutRect, content: SurfaceContent): SurfaceFrameLayout =
    SurfaceFrameLayout(frameRect, borderCellsFor(content))

  def contentChromeRows(
    hasHeader: Boolean,
    hasFooter: Boolean,
    reservedContentRows: Int = 0,
    hasKeyHint: Boolean = false
  ): Int =
    (if hasHeader then 1 else 0) +
      (if hasFooter then 1 else 0) +
      (if hasKeyHint then 1 else 0) +
      math.max(0, reservedContentRows)

  def frameChromeRows(
    hasHeader: Boolean,
    hasFooter: Boolean,
    reservedContentRows: Int = 0,
    borderCells: Int = DefaultBorderCells,
    hasKeyHint: Boolean = false
  ): Int =
    (math.max(0, borderCells) * 2) + contentChromeRows(hasHeader, hasFooter, reservedContentRows, hasKeyHint)

  def frameHeightForItemRows(
    itemRows: Int,
    hasHeader: Boolean,
    hasFooter: Boolean,
    reservedContentRows: Int = 0,
    borderCells: Int = DefaultBorderCells,
    itemGapRows: Double = 0.0,
    itemTargetRows: Int = 1,
    hasKeyHint: Boolean = false
  ): Int =
    val rows = math.max(0, itemRows)
    val gaps = math.ceil(math.max(0.0, rows - 1) * math.max(0.0, itemGapRows)).toInt
    (rows * math.max(1, itemTargetRows)) + gaps + frameChromeRows(
      hasHeader,
      hasFooter,
      reservedContentRows,
      borderCells,
      hasKeyHint
    )

  /** Minimum physical height for pointer-operable surface controls at the selected density. */
  def itemTargetRowsFor(content: SurfaceContent, density: InterfaceDensity): Int =
    content match
      case SurfaceContent.CommandPalette(_) | SurfaceContent.CommandRunnerPeek(_) | SurfaceContent.ContextMenu(_) |
          SurfaceContent.ContextualToolbar(_) =>
        minimumTargetRows(density)
      case _ => 1

  def minimumTargetRows(density: InterfaceDensity): Int =
    density match
      case InterfaceDensity.Compact                                 => 1
      case InterfaceDensity.Comfortable | InterfaceDensity.Spacious => 2
