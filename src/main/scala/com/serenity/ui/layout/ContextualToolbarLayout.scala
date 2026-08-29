package com.serenity.ui.layout

import com.serenity.command.CommandOption
import com.serenity.config.ToolbarDisplayMode
import com.serenity.state.models.*

/** Cell-space geometry and hit-testing for the contextual toolbar: row wrapping, column widths, padding, and the
  * translation between a pixel/cell click and the [[ContextualToolbarHit]] it lands on. Pure functions over
  * [[ContextualToolbarState]] and the items it displays -- no mutable state of its own.
  */
object ContextualToolbarLayout:

  private val maxDisplayTextWidth         = 18
  private val compactPaneWidthNumerator   = 2L
  private val compactPaneWidthDenominator = 3L

  /** Cell widths for a toolbar row, excluding inter-item and group-separator gutters. */
  def itemCellWidths(
    items: List[ContextualToolbarItem],
    contentWidth: Int,
    mode: ToolbarDisplayMode
  ): List[Int] =
    val preferredWidths = items.map(item => displayTextWidth(ContextualToolbar.displayText(item, mode)) + 2)
    val gutters         = rowGutterWidth(items)
    val availableWidth  = (contentWidth - gutters).max(0)
    if preferredWidths.sum <= availableWidth then preferredWidths
    else distributeProportionally(preferredWidths, availableWidth)

  /** Leading blank cells that center a compact toolbar row within its frame. */
  def rowLeadingPadding(
    items: List[ContextualToolbarItem],
    contentWidth: Int,
    mode: ToolbarDisplayMode
  ): Int =
    val occupiedWidth = itemCellWidths(items, contentWidth, mode).sum + rowGutterWidth(items)
    ((contentWidth - occupiedWidth).max(0)) / 2

  def rowGroups(
    items: List[ContextualToolbarItem],
    contentWidth: Int,
    mode: ToolbarDisplayMode
  ): List[List[ContextualToolbarItem]] =
    if items.isEmpty || contentWidth <= 0 then Nil
    else if estimatedRowWidth(items, mode) <= contentWidth then List(items)
    else if proseItemSegments(items).exists(_.length > 1) then
      packSegments(proseItemSegments(items), contentWidth, mode)
    else packItems(items, contentWidth, mode)

  def compactContentWidth(toolbarState: ContextualToolbarState, state: AppState, maxWidth: Int): Int =
    val items          = ContextualToolbar.itemsFor(state)
    val intrinsicWidth = estimatedRowWidth(items, toolbarState.displayMode)
    val largestGroupWidth = proseItemSegments(items)
      .map(estimatedRowWidth(_, toolbarState.displayMode))
      .maxOption
      .getOrElse(1)
    val compactRowLimit =
      toolbarState.displayMode match
        case ToolbarDisplayMode.IconOnly => maxWidth.max(1)
        case _ =>
          ((maxWidth.max(1).toLong * compactPaneWidthNumerator) / compactPaneWidthDenominator).toInt.max(1)
    val balancedWidth =
      if intrinsicWidth <= compactRowLimit then intrinsicWidth
      else
        balancedTwoRowWidth(proseItemSegments(items), toolbarState.displayMode)
          .getOrElse((intrinsicWidth + 1) / 2)
          .max(largestGroupWidth)
    balancedWidth
      .max(1)
      .min(compactRowLimit)
      .min(maxWidth.max(1))

  def rowCount(
    toolbarState: ContextualToolbarState,
    state: AppState,
    contentWidth: Int
  ): Int =
    val items    = ContextualToolbar.itemsFor(state)
    val topLevel = rowGroups(items, contentWidth, toolbarState.displayMode).length
    val detailCount =
      toolbarState.normalized(items).detailState match
        case Some(_: ContextualToolbarDetailState.Dropdown) =>
          detailRowGroups(toolbarState, items, contentWidth).length
        case Some(_: ContextualToolbarDetailState.Input) =>
          1
        case None =>
          0
    (topLevel + detailCount).max(1)

  def detailRowGroups(
    toolbarState: ContextualToolbarState,
    items: List[ContextualToolbarItem],
    contentWidth: Int
  ): List[List[CommandOption]] =
    toolbarState.normalized(items).detailState match
      case Some(ContextualToolbarDetailState.Dropdown(itemId, _)) =>
        ContextualToolbar
          .dropdownItem(itemId, items)
          .map(_.optionItem.options)
          .map(options => optionRowGroups(options, contentWidth))
          .getOrElse(Nil)
      case _ =>
        Nil

  def hitAt(
    rowIndex: Int,
    columnOffset: Int,
    contentWidth: Int,
    toolbarState: ContextualToolbarState,
    state: AppState
  ): Option[ContextualToolbarHit] =
    val items        = ContextualToolbar.itemsFor(state)
    val topLevelRows = rowGroups(items, contentWidth, toolbarState.displayMode)
    topLevelRows.lift(rowIndex) match
      case Some(rowItems) =>
        topLevelItemIndexAt(rowItems, columnOffset, contentWidth, toolbarState.displayMode).map { localIndex =>
          val offset = topLevelRows.take(rowIndex).map(_.length).sum
          ContextualToolbarHit.TopLevelItem(offset + localIndex)
        }
      case None =>
        val detailRowIndex = rowIndex - topLevelRows.length
        toolbarState.normalized(items).detailState match
          case Some(ContextualToolbarDetailState.Dropdown(itemId, _)) =>
            val optionGroups = detailRowGroups(toolbarState, items, contentWidth)
            optionGroups.lift(detailRowIndex).flatMap { rowOptions =>
              Option.when(rowOptions.nonEmpty) {
                val offset = optionGroups.take(detailRowIndex).map(_.length).sum
                val localIndex =
                  ((columnOffset.max(0) * rowOptions.length) / contentWidth.max(1))
                    .max(0)
                    .min(rowOptions.length - 1)
                ContextualToolbarHit.DropdownOption(itemId, offset + localIndex)
              }
            }
          case Some(ContextualToolbarDetailState.Input(itemId, _)) if detailRowIndex == 0 =>
            Some(ContextualToolbarHit.InputDetail(itemId))
          case _ =>
            None

  /** The focused top-level item index after moving `deltaRows` between wrapped rows, or `None` when there is no
    * well-defined row layout to move within (e.g. no items).
    */
  def focusedIndexAfterVerticalMove(
    toolbarState: ContextualToolbarState,
    items: List[ContextualToolbarItem],
    contentWidth: Int,
    deltaRows: Int
  ): Option[Int] =
    if items.isEmpty || contentWidth <= 0 then None
    else
      Some(
        moveVerticalIndex(
          rowGroups(items, contentWidth, toolbarState.displayMode),
          toolbarState.normalized(items).focusedIndex,
          deltaRows
        )
      )

  /** The open dropdown detail's item id and selected option index after moving `deltaRows` between wrapped option rows,
    * or `None` when no dropdown detail is open.
    */
  def detailSelectionAfterVerticalMove(
    toolbarState: ContextualToolbarState,
    items: List[ContextualToolbarItem],
    contentWidth: Int,
    deltaRows: Int
  ): Option[(String, Int)] =
    toolbarState.normalized(items).detailState match
      case Some(ContextualToolbarDetailState.Dropdown(itemId, selectedIndex)) if contentWidth > 0 =>
        val groups = detailRowGroups(toolbarState, items, contentWidth)
        Some(itemId -> moveVerticalIndex(groups, selectedIndex, deltaRows))
      case _ =>
        None

  private def topLevelItemIndexAt(
    items: List[ContextualToolbarItem],
    columnOffset: Int,
    contentWidth: Int,
    mode: ToolbarDisplayMode
  ): Option[Int] =
    val widths      = itemCellWidths(items, contentWidth, mode)
    val localColumn = columnOffset - rowLeadingPadding(items, contentWidth, mode)
    items
      .zip(widths)
      .zipWithIndex
      .foldLeft((0, Option.empty[Int])) {
        case ((cursor, found), ((item, width), index)) =>
          val cellEnd = cursor + width
          val hit     = Option.when(localColumn >= cursor && localColumn < cellEnd)(index)
          val separatorWidth =
            Option.when(ContextualToolbar.hasTrailingGroupSeparator(item, items.lift(index + 1)))(1).getOrElse(0)
          val gapWidth = Option.when(index < items.length - 1)(1).getOrElse(0)
          (cellEnd + separatorWidth + gapWidth, found.orElse(hit))
      }
      ._2

  private def rowGutterWidth(items: List[ContextualToolbarItem]): Int =
    items.drop(1).length + items.zip(items.drop(1)).count {
      case (item, nextItem) =>
        ContextualToolbar.hasTrailingGroupSeparator(item, Some(nextItem))
    }

  private def proseItemSegments(items: List[ContextualToolbarItem]): List[List[ContextualToolbarItem]] =
    items.foldLeft(List.empty[List[ContextualToolbarItem]]) { (segments, item) =>
      val nextGroupId = ContextualToolbar.formattingGroupId(item)
      segments match
        case init :+ last
            if last.headOption.exists(head => ContextualToolbar.formattingGroupId(head) == nextGroupId) &&
              nextGroupId.nonEmpty =>
          init :+ (last :+ item)
        case _ =>
          segments :+ List(item)
    }

  private def packSegments(
    segments: List[List[ContextualToolbarItem]],
    contentWidth: Int,
    mode: ToolbarDisplayMode
  ): List[List[ContextualToolbarItem]] =
    val packableSegments = segments.flatMap { segment =>
      if estimatedRowWidth(segment, mode) > contentWidth then packItems(segment, contentWidth, mode)
      else List(segment)
    }
    val (currentRow, rows) =
      packableSegments.foldLeft((List.empty[ContextualToolbarItem], List.empty[List[ContextualToolbarItem]])) {
        case ((currentRow, acc), segment) =>
          val nextRow = currentRow ++ segment
          if currentRow.nonEmpty && estimatedRowWidth(nextRow, mode) > contentWidth then (segment, acc :+ currentRow)
          else (nextRow, acc)
      }
    if currentRow.nonEmpty then rows :+ currentRow else rows

  private def balancedTwoRowWidth(
    segments: List[List[ContextualToolbarItem]],
    mode: ToolbarDisplayMode
  ): Option[Int] =
    (1 until segments.length).iterator.map { splitIndex =>
      estimatedRowWidth(segments.take(splitIndex).flatten, mode)
        .max(estimatedRowWidth(segments.drop(splitIndex).flatten, mode))
    }.minOption

  private def packItems(
    items: List[ContextualToolbarItem],
    contentWidth: Int,
    mode: ToolbarDisplayMode
  ): List[List[ContextualToolbarItem]] =
    val (currentRow, rows) =
      items.foldLeft((List.empty[ContextualToolbarItem], List.empty[List[ContextualToolbarItem]])) {
        case ((currentRow, acc), item) =>
          val nextWidth = estimatedRowWidth(currentRow :+ item, mode)
          if currentRow.nonEmpty && nextWidth > contentWidth then (List(item), acc :+ currentRow)
          else (currentRow :+ item, acc)
      }
    rows :+ currentRow

  private def estimatedRowWidth(items: List[ContextualToolbarItem], mode: ToolbarDisplayMode): Int =
    items.map(item => displayTextWidth(ContextualToolbar.displayText(item, mode)) + 2).sum +
      items.drop(1).length +
      items.zip(items.drop(1)).count {
        case (item, nextItem) =>
          ContextualToolbar.hasTrailingGroupSeparator(item, Some(nextItem))
      }

  private def distributeEvenly(itemCount: Int, availableWidth: Int): List[Int] =
    if itemCount == 0 then Nil
    else
      List.tabulate(itemCount) { index =>
        (availableWidth / itemCount) + Option.when(index < availableWidth % itemCount)(1).getOrElse(0)
      }

  private def distributeProportionally(preferredWidths: List[Int], availableWidth: Int): List[Int] =
    if preferredWidths.isEmpty then Nil
    else if availableWidth < preferredWidths.length then distributeEvenly(preferredWidths.length, availableWidth)
    else
      val remainingWidth = availableWidth - preferredWidths.length
      val weights        = preferredWidths.map(_ - 1)
      val totalWeight    = weights.sum.toLong
      val weightedShares = weights.map(weight => remainingWidth.toLong * weight)
      val allocated      = weightedShares.map(_ / totalWeight)
      val remainingCells = remainingWidth - allocated.sum.toInt
      val extraCells = weightedShares.zipWithIndex
        .sortBy { case (share, index) => (-(share % totalWeight), index) }
        .take(remainingCells)
        .map(_._2)
        .toSet
      List.tabulate(preferredWidths.length) { index =>
        1 + allocated(index).toInt + Option.when(extraCells.contains(index))(1).getOrElse(0)
      }

  private def displayTextWidth(text: String): Int =
    text
      .codePoints()
      .toArray
      .count { codePoint =>
        val category = Character.getType(codePoint)
        category != Character.NON_SPACING_MARK &&
        category != Character.COMBINING_SPACING_MARK &&
        category != Character.ENCLOSING_MARK
      }
      .min(maxDisplayTextWidth)

  private def optionRowGroups(options: List[CommandOption], contentWidth: Int): List[List[CommandOption]] =
    if options.isEmpty || contentWidth <= 0 then Nil
    else
      val (currentRow, rows) =
        options.foldLeft((List.empty[CommandOption], List.empty[List[CommandOption]])) {
          case ((currentRow, acc), option) =>
            val nextWidth = currentRow.map(_.label.length + 2).sum + option.label.length + 2 + currentRow.length
            if currentRow.nonEmpty && nextWidth > contentWidth then (List(option), acc :+ currentRow)
            else (currentRow :+ option, acc)
        }
      rows :+ currentRow

  private def moveVerticalIndex[A](
    rowGroups: List[List[A]],
    currentIndex: Int,
    deltaRows: Int
  ): Int =
    if rowGroups.isEmpty || deltaRows == 0 then currentIndex
    else
      val rowLengths = rowGroups.map(_.length)
      val totalItems = rowLengths.sum
      if totalItems == 0 then currentIndex
      else
        val step = deltaRows.sign
        Iterator
          .fill(deltaRows.abs)(step)
          .foldLeft(currentIndex.max(0).min(totalItems - 1)) { (index, rowDelta) =>
            val (rowIndex, localIndex) = rowAndLocalIndex(rowLengths, index)
            val targetRowIndex         = (rowIndex + rowDelta).max(0).min(rowLengths.length - 1)
            if targetRowIndex == rowIndex then index
            else
              val targetLength = rowLengths(targetRowIndex)
              rowLengths.take(targetRowIndex).sum +
                proportionalIndex(localIndex, rowLengths(rowIndex), targetLength)
          }

  private def rowAndLocalIndex(rowLengths: List[Int], globalIndex: Int): (Int, Int) =
    rowLengths
      .foldLeft((0, 0, Option.empty[(Int, Int)])) {
        case ((offset, rowIndex, found), rowLength) =>
          found match
            case some @ Some(_) =>
              (offset + rowLength, rowIndex + 1, some)
            case None if globalIndex < offset + rowLength =>
              (offset + rowLength, rowIndex + 1, Some((rowIndex, globalIndex - offset)))
            case None =>
              (offset + rowLength, rowIndex + 1, None)
      }
      ._3
      .getOrElse {
        val lastRowIndex  = (rowLengths.length - 1).max(0)
        val lastRowLength = rowLengths.lift(lastRowIndex).getOrElse(1).max(1)
        (lastRowIndex, lastRowLength - 1)
      }

  private def proportionalIndex(currentIndex: Int, currentRowLength: Int, targetRowLength: Int): Int =
    (((currentIndex + 0.5d) * targetRowLength) / currentRowLength.max(1)).toInt
      .max(0)
      .min(targetRowLength - 1)
