package com.serenity

import cats.effect.unsafe.implicits.global
import com.serenity.config.ToolbarDisplayMode
import com.serenity.keystroke.events.*
import com.serenity.richtext.*
import com.serenity.state.models.*
import com.serenity.ui.fonts.FontLoader
import com.serenity.ui.layout.*
import org.scalatest.matchers.should.Matchers

/** Shared fixtures for [[ContextualToolbarSpec]] (state and interaction behaviour) and [[ContextualToolbarLayoutSpec]]
  * (cell-space geometry and hit-testing). Both drive the toolbar through a real `StateManager` because the item list
  * itself is state-derived; the split between the two specs is which half of
  * `ContextualToolbar`/`ContextualToolbarLayout` each test exercises directly.
  */
trait ContextualToolbarTestSupport extends Matchers with StateManagerTestSupport:
  self: org.scalatest.Assertions =>

  final protected case class Point(x: Int, y: Int, pixelX: Int = 0, pixelY: Int = 0)

  protected def seedToolbarDocument(
    stateManager: com.serenity.state.manager.StateManager,
    fontFamily: String = "A"
  ): Unit =
    stateManager
      .updateState { state =>
        val bufferId  = state.focusedBufferId.getOrElse(fail("Expected focused buffer"))
        val selection = Selection(CursorPosition(0, 6), CursorPosition(0, 10))
        val range     = RichTextRange(RichTextPosition(0, 0), RichTextPosition(0, 10))
        val document = RichTextDocument
          .fromPlainText("alpha beta")
          .applyMark(range, InlineMark.Bold)
          .setFontFamily(range, fontFamily)
          .setFontSize(range, 18.0f)
          .setColor(range, "#336699")
          .setParagraphRole(range, ParagraphRole.Body)
          .setParagraphAlignment(range, ParagraphAlignment.Left)
          .normalized
        val nextBuffer = state.persisted
          .buffers(bufferId)
          .copy(
            document = state.persisted.buffers(bufferId).document.copy(content = com.serenity.rope.Rope("alpha beta")),
            editing = state.persisted
              .buffers(bufferId)
              .editing
              .copy(selection = Some(selection), cursors = List(selection.focus)),
            richText = state.persisted.buffers(bufferId).richText.copy(richTextDocument = Some(document))
          )
        state.copy(persisted = state.persisted.copy(buffers = state.persisted.buffers.updated(bufferId, nextBuffer)))
      }
      .unsafeRunSync()

  protected def toolbarItemPoint(state: AppState, itemId: String): Point =
    val itemIndex    = toolbarItemIndex(state, itemId)
    val rect         = toolbarRect(state)
    val toolbarState = toolbarStateFrom(state)
    val contentRect  = SurfaceFrameLayout.forContent(rect, SurfaceContent.ContextualToolbar(toolbarState)).contentRect
    val rowGroups = ContextualToolbarLayout
      .rowGroups(ContextualToolbar.itemsFor(state), contentRect.width.max(1), toolbarState.displayMode)
    val (rowIndex, localIndex) = rowGroups.zipWithIndex
      .collectFirst {
        case (row, currentRowIndex) if itemIndex < row.length + rowGroups.take(currentRowIndex).map(_.length).sum =>
          val offset = rowGroups.take(currentRowIndex).map(_.length).sum
          (currentRowIndex, itemIndex - offset)
      }
      .getOrElse(fail(s"Expected toolbar item index $itemIndex"))
    val rowItems    = rowGroups.lift(rowIndex).getOrElse(fail(s"Expected toolbar row $rowIndex"))
    val cellRegions = renderedToolbarCellRegions(rowItems, contentRect.width, toolbarState.displayMode)
    val (cellStart, cellWidth) = cellRegions
      .lift(localIndex)
      .getOrElse(fail(s"Expected toolbar cell $localIndex"))
    Point(
      x = contentRect.x + cellStart + (cellWidth / 2),
      y = toolbarRowY(state, rowIndex)
    )

  protected def toolbarSeparatorPoint(state: AppState, separatorIndex: Int): Point =
    val rect         = toolbarRect(state)
    val toolbarState = toolbarStateFrom(state)
    val contentRect  = SurfaceFrameLayout.forContent(rect, SurfaceContent.ContextualToolbar(toolbarState)).contentRect
    val rowItems = ContextualToolbarLayout
      .rowGroups(ContextualToolbar.itemsFor(state), contentRect.width.max(1), toolbarState.displayMode)
      .headOption
      .getOrElse(fail("Expected toolbar row"))
    val separatorOffset = renderedToolbarSeparatorOffsets(rowItems, contentRect.width, toolbarState.displayMode)
      .lift(separatorIndex)
      .getOrElse(fail(s"Expected toolbar separator $separatorIndex"))
    Point(contentRect.x + separatorOffset, toolbarRowY(state, 0))

  protected def toolbarDetailPoint(state: AppState, itemId: String, optionLabel: String): Point =
    val rect         = toolbarRect(state)
    val toolbarState = toolbarStateFrom(state)
    val contentRect  = SurfaceFrameLayout.forContent(rect, SurfaceContent.ContextualToolbar(toolbarState)).contentRect
    val rowGroups = ContextualToolbarLayout
      .rowGroups(ContextualToolbar.itemsFor(state), contentRect.width.max(1), toolbarState.displayMode)
    val detailRows =
      ContextualToolbarLayout.detailRowGroups(toolbarState, ContextualToolbar.itemsFor(state), contentRect.width.max(1))
    val optionIndex = ContextualToolbar
      .dropdownItem(itemId, ContextualToolbar.itemsFor(state))
      .map(_.optionItem.options.indexWhere(_.label == optionLabel))
      .filter(_ >= 0)
      .getOrElse(fail(s"Expected toolbar detail option $optionLabel for $itemId"))
    val (rowIndex, localIndex) = detailRows.zipWithIndex
      .collectFirst {
        case (rowOptions, currentRowIndex)
            if optionIndex < rowOptions.length + detailRows.take(currentRowIndex).map(_.length).sum =>
          val offset = detailRows.take(currentRowIndex).map(_.length).sum
          (currentRowIndex, optionIndex - offset)
      }
      .getOrElse(fail(s"Expected toolbar detail option $optionIndex"))
    val rowOptions = detailRows.lift(rowIndex).getOrElse(fail(s"Expected toolbar detail row $rowIndex"))
    Point(
      x = contentRect.x + hitColumnCenter(localIndex, rowOptions.length, contentRect.width),
      y = toolbarRowY(state, rowGroups.length + rowIndex)
    )

  protected def toolbarRowY(state: AppState, displayedRowIndex: Int): Int =
    val viewport = state.runtime.viewportSize.getOrElse(fail("Expected viewport size"))
    val surface  = state.contextualToolbarSurface.getOrElse(fail("Expected contextual toolbar surface"))
    val contract = EditorLayoutContract.from(state, viewport, LayoutEngine.calculateLayoutWithUI(state, viewport))
    contract.floatingOverlayRowSlots
      .getOrElse(surface.id, Nil)
      .collectFirst { case SurfaceContentRowSlot(SurfaceContentRowKind.Item(`displayedRowIndex`), y) => y }
      .getOrElse(fail(s"Expected toolbar content row $displayedRowIndex"))

  protected def fractionalToolbarPoint(state: AppState, point: Point): Point =
    val viewport = state.runtime.viewportSize.getOrElse(fail("Expected viewport size"))
    val surface  = state.contextualToolbarSurface.getOrElse(fail("Expected contextual toolbar surface"))
    val layout   = LayoutEngine.calculateLayoutWithUI(state, viewport)
    val metrics  = CellMetrics.fromFont(FontLoader.previewCodeFont(state.persisted.config.editorConfig.fontConfig))
    val offsetPx = FloatingSurfaceGeometry.signedRowOffsetPixels(
      layout.floatingOverlayOffsetRows.getOrElse(surface.id, 0.0),
      metrics
    )
    Point(
      x = point.x,
      y = point.y,
      pixelX = point.x * metrics.charWidth + metrics.charWidth / 2,
      pixelY = math.round(point.y * metrics.lineHeight + offsetPx + metrics.lineHeight / 2.0).toInt
    )

  protected def toolbarRect(state: AppState) =
    val viewport = state.runtime.viewportSize.getOrElse(fail("Expected viewport size"))
    val surface  = state.contextualToolbarSurface.getOrElse(fail("Expected contextual toolbar surface"))
    val contract = EditorLayoutContract.from(state, viewport, LayoutEngine.calculateLayoutWithUI(state, viewport))
    contract
      .overlayRect(surface.id)
      .getOrElse(fail("Expected toolbar overlay rect"))

  protected def toolbarContentWidth(state: AppState): Int =
    val toolbarState = toolbarStateFrom(state)
    SurfaceFrameLayout
      .forContent(toolbarRect(state), SurfaceContent.ContextualToolbar(toolbarState))
      .contentRect
      .width
      .max(1)

  protected def toolbarStateFrom(state: AppState): ContextualToolbarState =
    state.contextualToolbarSurface
      .flatMap {
        _.content match
          case SurfaceContent.ContextualToolbar(toolbarState) => Some(toolbarState)
          case _                                              => None
      }
      .getOrElse(fail("Expected contextual toolbar state"))

  protected def toolbarButton(state: AppState, itemId: String): ContextualToolbarItem.Button =
    ContextualToolbar
      .itemsFor(state)
      .collectFirst {
        case item: ContextualToolbarItem.Button if item.id == itemId => item
      }
      .getOrElse(fail(s"Expected toolbar button $itemId"))

  protected def toolbarInput(state: AppState, itemId: String): ContextualToolbarItem.Input =
    ContextualToolbar
      .itemsFor(state)
      .collectFirst {
        case item: ContextualToolbarItem.Input if item.id == itemId => item
      }
      .getOrElse(fail(s"Expected toolbar input $itemId"))

  protected def toolbarDropdown(state: AppState, itemId: String): ContextualToolbarItem.Dropdown =
    ContextualToolbar
      .itemsFor(state)
      .collectFirst {
        case item: ContextualToolbarItem.Dropdown if item.id == itemId => item
      }
      .getOrElse(fail(s"Expected toolbar dropdown $itemId"))

  protected def focusedToolbarItemId(state: AppState): String =
    val items = ContextualToolbar.itemsFor(state)
    toolbarStateFrom(state)
      .normalized(items)
      .focusedItem(items)
      .map(_.id)
      .getOrElse(fail("Expected focused toolbar item"))

  protected def verticalTopLevelPair(rowGroups: List[List[ContextualToolbarItem]]): (String, String) =
    rowGroups.zipWithIndex
      .collectFirst {
        case (rowItems, rowIndex)
            if rowIndex < rowGroups.length - 1 && rowItems.nonEmpty && rowGroups(rowIndex + 1).nonEmpty =>
          val localIndex = rowItems.length - 1
          (
            rowItems(localIndex).id,
            rowGroups(rowIndex + 1)(verticalTargetIndex(localIndex, rowItems.length, rowGroups(rowIndex + 1).length)).id
          )
      }
      .getOrElse(fail("Expected wrapped toolbar rows with multiple items"))

  protected def verticalTargetIndex(currentIndex: Int, currentRowLength: Int, targetRowLength: Int): Int =
    (((currentIndex + 0.5d) * targetRowLength) / currentRowLength).toInt
      .max(0)
      .min(targetRowLength - 1)

  protected def hitColumnCenter(localIndex: Int, itemCount: Int, contentWidth: Int): Int =
    val start = ((localIndex * contentWidth) + itemCount - 1) / itemCount
    val end   = ((((localIndex + 1) * contentWidth) + itemCount - 1) / itemCount) - 1
    start + math.max(0, (end - start) / 2)

  protected def renderedToolbarCellRegions(
    items: List[ContextualToolbarItem],
    contentWidth: Int,
    mode: ToolbarDisplayMode
  ): List[(Int, Int)] =
    val widths         = ContextualToolbarLayout.itemCellWidths(items, contentWidth, mode)
    val leadingPadding = ContextualToolbarLayout.rowLeadingPadding(items, contentWidth, mode)
    items
      .zip(widths)
      .zipWithIndex
      .foldLeft((leadingPadding, List.empty[(Int, Int)])) {
        case ((cursor, regions), ((item, width), index)) =>
          val separatorWidth = Option
            .when(ContextualToolbar.hasTrailingGroupSeparator(item, items.lift(index + 1)))(1)
            .getOrElse(0)
          val gapWidth = Option.when(index < items.length - 1)(1).getOrElse(0)
          (cursor + width + separatorWidth + gapWidth, regions :+ (cursor -> width))
      }
      ._2

  protected def renderedToolbarSeparatorOffsets(
    items: List[ContextualToolbarItem],
    contentWidth: Int,
    mode: ToolbarDisplayMode
  ): List[Int] =
    renderedToolbarCellRegions(items, contentWidth, mode)
      .zip(items)
      .zipWithIndex
      .collect {
        case (((start, width), item), index)
            if ContextualToolbar.hasTrailingGroupSeparator(item, items.lift(index + 1)) =>
          start + width
      }

  protected def moveToolbarFocusTo(stateManager: com.serenity.state.manager.StateManager, itemId: String): Unit =
    focusToolbar(stateManager)
    val state     = stateManager.getCurrentState.unsafeRunSync()
    val target    = toolbarItemIndex(state, itemId)
    val toolbar   = toolbarStateFrom(state)
    val itemCount = ContextualToolbar.itemsFor(state).length
    val delta     = (target - toolbar.focusedIndex + itemCount) % itemCount
    (0 until delta).foreach(_ => stateManager.applyEvent(MoveRight).unsafeRunSync())

  protected def focusToolbar(stateManager: com.serenity.state.manager.StateManager): Unit =
    stateManager
      .updateState { state =>
        val toolbarId = state.contextualToolbarSurface
          .map(_.id)
          .getOrElse(fail("Expected contextual toolbar surface"))
        state.pushFocus(Focus.Surface(toolbarId))
      }
      .unsafeRunSync()

  protected def toolbarItemIndex(state: AppState, itemId: String): Int =
    ContextualToolbar.itemsFor(state).indexWhere(_.id == itemId) match
      case -1    => fail(s"Expected toolbar item $itemId")
      case index => index

  protected def activeBufferId(state: AppState): BufferId =
    state.persisted.layout.activeEditorPaneId
      .flatMap(state.persisted.layout.editorPanes.get)
      .flatMap(_.bufferId)
      .getOrElse(fail("Expected active buffer"))
