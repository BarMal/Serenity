package com.serenity

import cats.effect.unsafe.implicits.global
import com.serenity.keystroke.events.*
import com.serenity.rope.Balance
import com.serenity.state.models.*
import com.serenity.ui.layout.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ContextualToolbarUiScenarioSpec extends AnyFlatSpec with Matchers:
  given Balance = Balance.default

  "Contextual toolbar UI scenario" should "summon, follow the caret, and return focus on close" in {
    val driver = UiScenarioDriver
      .create("contextual-toolbar", UiScenarioEnvironment(viewport = com.serenity.ui.layout.ViewportSize(60, 18)))
      .unsafeRunSync()
    driver.dispatch(ToggleContextualToolbar).unsafeRunSync()
    val opened    = driver.renderFrame("opened").unsafeRunSync()
    val surfaceId = opened.evidence.surfaceRects.keys.headOption.getOrElse(fail("Expected toolbar rectangle"))
    val initial   = opened.evidence.surfaceRects(surfaceId)
    initial.width should be < driver.environment.viewport.width
    opened.evidence.focus shouldBe Focus.EditorPane(PaneId(0))

    driver.stateManager.setCursorPosition(PaneId(0), 0, 1).unsafeRunSync()
    driver.dispatch(InsertChar('x')).unsafeRunSync()
    driver.dispatch(ToggleContextualToolbar).unsafeRunSync()
    val closed = driver.renderFrame("closed").unsafeRunSync()

    closed.evidence.surfaceRects shouldBe empty
    closed.evidence.focus shouldBe Focus.EditorPane(PaneId(0))
  }

  it should "render a wide default toolbar as two balanced rows with a material width margin" in {
    val driver = UiScenarioDriver
      .create(
        "contextual-toolbar-wide-balanced",
        UiScenarioEnvironment(viewport = com.serenity.ui.layout.ViewportSize(215, 30))
      )
      .unsafeRunSync()
    driver
      .updateState { state =>
        val bufferId  = state.focusedBufferId.getOrElse(BufferId(0))
        val selection = Selection(CursorPosition(0, 0), CursorPosition(0, 5))
        val buffer = state
          .buffers(bufferId)
          .copy(
            content = com.serenity.rope.Rope("alpha beta"),
            selection = Some(selection),
            cursors = List(selection.focus)
          )
        state.copy(buffers = state.buffers.updated(bufferId, buffer))
      }
      .unsafeRunSync()
    driver.dispatch(ToggleContextualToolbar).unsafeRunSync()

    val rendered  = driver.renderFrame("wide-balanced").unsafeRunSync()
    val surfaceId = rendered.evidence.surfaceRects.keys.headOption.getOrElse(fail("Expected toolbar"))
    val toolbar   = rendered.evidence.surfaceRects(surfaceId)
    val itemRows  = rendered.evidence.itemRects(surfaceId)

    toolbar.width should be <= (driver.environment.viewport.width * 2 / 3)
    toolbarRows(driver) should have size 2
    itemRows should have size 2
    itemRows.foreach(itemRow => toolbar.containsRect(itemRow) shouldBe true)
  }

  it should "exercise button, dropdown, input, and wrapped narrow navigation" in {
    val driver = UiScenarioDriver
      .create(
        "contextual-toolbar-controls",
        UiScenarioEnvironment(viewport = com.serenity.ui.layout.ViewportSize(42, 18))
      )
      .unsafeRunSync()
    driver
      .updateState { state =>
        val bufferId  = state.focusedBufferId.getOrElse(BufferId(0))
        val selection = Selection(CursorPosition(0, 0), CursorPosition(0, 5))
        val buffer = state
          .buffers(bufferId)
          .copy(
            content = com.serenity.rope.Rope("alpha beta"),
            selection = Some(selection),
            cursors = List(selection.focus)
          )
        state.copy(buffers = state.buffers.updated(bufferId, buffer))
      }
      .unsafeRunSync()
    driver.dispatch(ToggleContextualToolbar).unsafeRunSync()
    val opened    = driver.renderFrame("wrapped").unsafeRunSync()
    val surfaceId = opened.evidence.surfaceRects.keys.headOption.getOrElse(fail("Expected toolbar"))
    val rowGroups = toolbarRows(driver)
    val content   = toolbarContentRect(driver)
    opened.evidence.surfaceRects(surfaceId).width should be < driver.environment.viewport.width
    rowGroups.length should be > 1
    opened.evidence.itemRects(surfaceId) should have size rowGroups.length
    opened.evidence.surfaceRects(surfaceId).height should be > content.height

    val dropdownPoint = toolbarItemPoint(driver, "paragraph-role")
    driver.dispatch(MouseClick(dropdownPoint._1, dropdownPoint._2)).unsafeRunSync()
    driver.dispatch(Escape).unsafeRunSync()
    val beforeVertical = toolbarState(driver).focusedIndex
    val beforeRow      = toolbarRowOf(driver, beforeVertical)
    driver.dispatch(MoveUp).unsafeRunSync()
    val afterVertical = toolbarState(driver).focusedIndex
    toolbarRowOf(driver, afterVertical) should not be beforeRow

    moveToToolbarItem(driver, "bold")
    driver.dispatch(Enter).unsafeRunSync()
    driver.state.unsafeRunSync().buffers.values.flatMap(_.richTextDocument).toList should not be empty

    val dropdownAfterButton = toolbarItemPoint(driver, "paragraph-role")
    driver.dispatch(MouseClick(dropdownAfterButton._1, dropdownAfterButton._2)).unsafeRunSync()
    toolbarState(driver).detailState.getOrElse(fail("Expected dropdown detail")) shouldBe
      a[ContextualToolbarDetailState.Dropdown]
    driver.dispatch(MoveRight).unsafeRunSync()
    driver.dispatch(Enter).unsafeRunSync()
    driver.state.unsafeRunSync().focus shouldBe Focus.EditorPane(PaneId(0))

    val inputPoint = toolbarItemPoint(driver, "font-size")
    driver.dispatch(MouseClick(inputPoint._1, inputPoint._2)).unsafeRunSync()
    toolbarState(driver).detailState.getOrElse(fail("Expected input detail")) shouldBe
      a[ContextualToolbarDetailState.Input]
    driver.dispatch(DeleteBackward).unsafeRunSync()
    driver.dispatch(DeleteBackward).unsafeRunSync()
    driver.dispatch(InsertChar('2')).unsafeRunSync()
    driver.dispatch(InsertChar('0')).unsafeRunSync()
    driver.dispatch(Enter).unsafeRunSync()
    driver.state.unsafeRunSync().focus shouldBe Focus.EditorPane(PaneId(0))
  }

  private def moveToToolbarItem(driver: UiScenarioDriver, itemId: String): Unit =
    val state  = driver.state.unsafeRunSync()
    val items  = ContextualToolbar.itemsFor(state)
    val target = items.indexWhere(_.id == itemId)
    val delta  = (target - toolbarState(driver).focusedIndex + items.length) % items.length
    (0 until delta).foreach(_ => driver.dispatch(MoveRight).unsafeRunSync())

  private def toolbarItemPoint(driver: UiScenarioDriver, itemId: String): (Int, Int) =
    val state       = driver.state.unsafeRunSync()
    val items       = ContextualToolbar.itemsFor(state)
    val itemIndex   = items.indexWhere(_.id == itemId)
    val rowGroups   = toolbarRows(driver)
    val contentRect = toolbarContentRect(driver)
    val (rowIndex, localIndex) = rowGroups.zipWithIndex
      .collectFirst {
        case (row, currentRowIndex) if itemIndex < row.length + rowGroups.take(currentRowIndex).map(_.length).sum =>
          val precedingItemCount = rowGroups.take(currentRowIndex).map(_.length).sum
          currentRowIndex -> (itemIndex - precedingItemCount)
      }
      .getOrElse(fail(s"Expected toolbar item $itemId"))
    val regions = renderedToolbarCellRegions(
      rowGroups(rowIndex),
      contentRect.width,
      toolbarState(driver).displayMode
    )
    val (start, width) = regions.lift(localIndex).getOrElse(fail(s"Expected toolbar cell $itemId"))
    val surface        = state.contextualToolbarSurface.getOrElse(fail("Expected toolbar"))
    val viewport       = state.viewportSize.getOrElse(fail("Expected viewport"))
    val contract       = EditorLayoutContract.from(state, viewport, LayoutEngine.calculateLayoutWithUI(state, viewport))
    val y = contract
      .overlayRowSlots(surface.id)
      .collectFirst { case SurfaceContentRowSlot(SurfaceContentRowKind.Item(`rowIndex`), rowY) => rowY }
      .getOrElse(fail(s"Expected toolbar row $rowIndex"))
    contentRect.x + start + width / 2 -> y

  private def toolbarRows(driver: UiScenarioDriver): List[List[ContextualToolbarItem]] =
    val state = driver.state.unsafeRunSync()
    ContextualToolbar.rowGroups(
      ContextualToolbar.itemsFor(state),
      toolbarContentRect(driver).width.max(1),
      toolbarState(driver).displayMode
    )

  private def toolbarContentRect(driver: UiScenarioDriver): LayoutRect =
    val state    = driver.state.unsafeRunSync()
    val viewport = state.viewportSize.getOrElse(fail("Expected viewport"))
    val surface  = state.contextualToolbarSurface.getOrElse(fail("Expected toolbar"))
    val contract = EditorLayoutContract.from(state, viewport, LayoutEngine.calculateLayoutWithUI(state, viewport))
    val rect     = contract.overlayRect(surface.id).getOrElse(fail("Expected toolbar rectangle"))
    SurfaceFrameLayout.forContent(rect, surface.content).contentRect

  private def toolbarRowOf(driver: UiScenarioDriver, itemIndex: Int): Int =
    toolbarRows(driver).zipWithIndex
      .collectFirst {
        case (row, rowIndex) if itemIndex < row.length + toolbarRows(driver).take(rowIndex).map(_.length).sum =>
          rowIndex
      }
      .getOrElse(fail(s"Expected toolbar row for item $itemIndex"))

  private def renderedToolbarCellRegions(
    items: List[ContextualToolbarItem],
    contentWidth: Int,
    mode: com.serenity.config.ToolbarDisplayMode
  ): List[(Int, Int)] =
    val widths         = ContextualToolbar.itemCellWidths(items, contentWidth, mode)
    val leadingPadding = ContextualToolbar.rowLeadingPadding(items, contentWidth, mode)
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

  private def toolbarState(driver: UiScenarioDriver): ContextualToolbarState =
    driver.state.unsafeRunSync().contextualToolbarSurface.getOrElse(fail("Expected toolbar")).content match
      case SurfaceContent.ContextualToolbar(value) => value
      case _                                       => fail("Expected toolbar content")
