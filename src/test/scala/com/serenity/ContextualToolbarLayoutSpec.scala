package com.serenity

import cats.effect.unsafe.implicits.global
import com.serenity.command.*
import com.serenity.config.ToolbarDisplayMode
import com.serenity.keystroke.events.*
import com.serenity.richtext.*
import com.serenity.state.models.*
import com.serenity.ui.layout.*
import com.serenity.ui.renderer.{SurfaceContentResolver, SurfaceRenderMode}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Cell-space geometry and hit-testing: row wrapping, compact-width balancing, cell widths/padding, and pixel-click to
  * [[ContextualToolbarHit]] resolution. Behaviour tests that exercise focus/detail state and toolbar interaction end to
  * end live in [[ContextualToolbarSpec]].
  */
class ContextualToolbarLayoutSpec extends AnyFlatSpec with Matchers with ContextualToolbarTestSupport:

  it should "compact and balance the default formatting toolbar when its intrinsic width exceeds the pane" in {
    val stateManager = createStateManager("ContextualToolbarSpec-default-constrained-pane")

    stateManager.applyEvent(ResizeEvent(ViewportSize(140, 30))).unsafeRunSync()
    seedToolbarDocument(stateManager)
    stateManager.applyEvent(ToggleContextualToolbar).unsafeRunSync()

    val state        = stateManager.getCurrentState.unsafeRunSync()
    val toolbarState = toolbarStateFrom(state)
    val items        = ContextualToolbar.itemsFor(state)
    val viewport     = state.runtime.viewportSize.getOrElse(fail("Expected viewport size"))
    val layout       = LayoutEngine.calculateLayoutWithUI(state, viewport)
    val toolbar      = toolbarRect(state)
    val editorWidth = LayoutEngine
      .calculateEditorWorkspaceLayout(state, layout)
      .activeContentRect(state)
      .map(_.width)
      .getOrElse(fail("Expected active content rect"))
    val intrinsicWidth = ContextualToolbarLayout.compactContentWidth(toolbarState, state, Int.MaxValue)
    val rowGroups = ContextualToolbarLayout.rowGroups(
      items,
      toolbarContentWidth(state),
      toolbarState.displayMode
    )

    toolbarState.displayMode shouldBe ToolbarDisplayMode.IconAndText
    intrinsicWidth should be > editorWidth
    toolbar.width should be < (editorWidth * 3 / 4)
    rowGroups.map(_.map(_.id)) shouldBe List(
      List("bold", "italic", "underline", "font-family", "font-family-text", "font-size"),
      List("color", "color-hex", "paragraph-role", "align-left", "align-center", "align-right", "align-justify")
    )
  }

  it should "center short wrapped rows and leave their surrounding padding inactive" in {
    val stateManager = createStateManager("ContextualToolbarSpec-centered-wrapped-rows")

    stateManager.applyEvent(ResizeEvent(ViewportSize(140, 30))).unsafeRunSync()
    seedToolbarDocument(stateManager)
    stateManager.applyEvent(ToggleContextualToolbar).unsafeRunSync()

    val state        = stateManager.getCurrentState.unsafeRunSync()
    val toolbarState = toolbarStateFrom(state)
    val contentWidth = toolbarContentWidth(state)
    val rowGroups = ContextualToolbarLayout.rowGroups(
      ContextualToolbar.itemsFor(state),
      contentWidth,
      toolbarState.displayMode
    )
    val resolvedRows = SurfaceContentResolver
      .resolveContextualToolbar(toolbarState, state, toolbarRect(state), SurfaceRenderMode.Floating)
      .rows
    val centeredRowIndex = rowGroups.zipWithIndex
      .collectFirst {
        case (row, index)
            if ContextualToolbarLayout.rowLeadingPadding(row, contentWidth, toolbarState.displayMode) > 0 =>
          index
      }
      .getOrElse(fail("Expected a short toolbar row"))
    val centeredPadding = ContextualToolbarLayout.rowLeadingPadding(
      rowGroups(centeredRowIndex),
      contentWidth,
      toolbarState.displayMode
    )

    resolvedRows(centeredRowIndex).leadingPadding shouldBe centeredPadding
    ContextualToolbarLayout.hitAt(
      centeredRowIndex,
      centeredPadding - 1,
      contentWidth,
      toolbarState,
      state
    ) shouldBe None
  }

  it should "never exceed its compact width cap when balanced groups are wider" in {
    val stateManager = createStateManager("ContextualToolbarSpec-absolute-compact-cap")

    stateManager.applyEvent(ResizeEvent(ViewportSize(100, 30))).unsafeRunSync()
    seedToolbarDocument(stateManager)
    stateManager.applyEvent(ToggleContextualToolbar).unsafeRunSync()

    val state    = stateManager.getCurrentState.unsafeRunSync()
    val viewport = state.runtime.viewportSize.getOrElse(fail("Expected viewport size"))
    val layout   = LayoutEngine.calculateLayoutWithUI(state, viewport)
    val editorWidth = LayoutEngine
      .calculateEditorWorkspaceLayout(state, layout)
      .activeContentRect(state)
      .map(_.width)
      .getOrElse(fail("Expected active content rect"))

    toolbarRect(state).width should be <= ((editorWidth.toLong * 2) / 3).toInt + 2
  }

  it should "wrap a fitting toolbar before it consumes most of the active editor pane" in {
    val stateManager = createStateManager("ContextualToolbarSpec-near-full-width-regression")

    stateManager.applyEvent(ResizeEvent(ViewportSize(200, 30))).unsafeRunSync()
    seedToolbarDocument(stateManager)

    val unopened       = stateManager.getCurrentState.unsafeRunSync()
    val intrinsicWidth = ContextualToolbarLayout.compactContentWidth(ContextualToolbarState(), unopened, Int.MaxValue)
    stateManager.applyEvent(ResizeEvent(ViewportSize(intrinsicWidth + 20, 30))).unsafeRunSync()
    stateManager.applyEvent(ToggleContextualToolbar).unsafeRunSync()

    val state    = stateManager.getCurrentState.unsafeRunSync()
    val viewport = state.runtime.viewportSize.getOrElse(fail("Expected viewport size"))
    val layout   = LayoutEngine.calculateLayoutWithUI(state, viewport)
    val editorWidth = LayoutEngine
      .calculateEditorWorkspaceLayout(state, layout)
      .activeContentRect(state)
      .map(_.width)
      .getOrElse(fail("Expected active content rect"))
    val toolbar = toolbarRect(state)

    intrinsicWidth should be <= editorWidth
    toolbar.width should be <= (editorWidth * 3 / 4)
  }

  it should "use the fewest balanced rows without becoming a wide panel" in {
    val stateManager = createStateManager("ContextualToolbarSpec-wide-palette-regression")

    stateManager.applyEvent(ResizeEvent(ViewportSize(215, 30))).unsafeRunSync()
    seedToolbarDocument(stateManager)
    stateManager.applyEvent(ToggleContextualToolbar).unsafeRunSync()

    val state    = stateManager.getCurrentState.unsafeRunSync()
    val viewport = state.runtime.viewportSize.getOrElse(fail("Expected viewport size"))
    val layout   = LayoutEngine.calculateLayoutWithUI(state, viewport)
    val editorWidth = LayoutEngine
      .calculateEditorWorkspaceLayout(state, layout)
      .activeContentRect(state)
      .map(_.width)
      .getOrElse(fail("Expected active content rect"))

    val toolbarState = toolbarStateFrom(state)
    val rowCount = ContextualToolbarLayout.rowCount(
      toolbarState,
      state,
      toolbarContentWidth(state)
    )

    toolbarRect(state).width should be <= (editorWidth * 2 / 3) + 2
    rowCount shouldBe 2
  }

  it should "keep a long font family from widening the compact toolbar to the pane" in {
    val stateManager = createStateManager("ContextualToolbarSpec-long-font-family")

    stateManager.applyEvent(ResizeEvent(ViewportSize(160, 30))).unsafeRunSync()
    seedToolbarDocument(stateManager, fontFamily = "A deliberately long font family name for compact toolbar coverage")

    val state = stateManager.getCurrentState.unsafeRunSync()
    val width = ContextualToolbarLayout.compactContentWidth(
      ContextualToolbarState(),
      state,
      maxWidth = 120
    )

    width should be <= 90
    ContextualToolbarLayout.rowCount(ContextualToolbarState(), state, width) should be > 1
  }

  it should "give constrained toolbar rows more room for descriptive controls" in {
    val items = List(
      ContextualToolbarItem.Button("bold", "Bold", "bold", "b"),
      ContextualToolbarItem.Button("font-family", "Font family", "font-family", "f"),
      ContextualToolbarItem.Button("size", "Size", "size", "s")
    )

    val widths = ContextualToolbarLayout.itemCellWidths(items, contentWidth = 22, ToolbarDisplayMode.TextOnly)

    widths.sum shouldBe 19
    widths.foreach(_ should be > 0)
    widths(1) should be > widths(0)
    widths(1) should be > widths(2)
  }

  it should "map each rendered compact toolbar cell and leave separator gutters inert" in {
    val stateManager = createStateManager("ContextualToolbarSpec-variable-width-hit-regions")

    stateManager.applyEvent(ResizeEvent(ViewportSize(78, 30))).unsafeRunSync()
    seedToolbarDocument(stateManager)
    stateManager.applyEvent(ToggleContextualToolbar).unsafeRunSync()

    val state        = stateManager.getCurrentState.unsafeRunSync()
    val toolbarState = toolbarStateFrom(state)
    val contentWidth = toolbarContentWidth(state)
    val rowItems = ContextualToolbarLayout
      .rowGroups(ContextualToolbar.itemsFor(state), contentWidth, toolbarState.displayMode)
      .head

    renderedToolbarCellRegions(rowItems, contentWidth, toolbarState.displayMode).zipWithIndex.foreach {
      case ((start, width), index) =>
        ContextualToolbarLayout.hitAt(0, start + (width / 2), contentWidth, toolbarState, state) shouldBe
          Some(ContextualToolbarHit.TopLevelItem(index))
    }

    renderedToolbarSeparatorOffsets(rowItems, contentWidth, toolbarState.displayMode).foreach { offset =>
      ContextualToolbarLayout.hitAt(0, offset, contentWidth, toolbarState, state) shouldBe None
    }
  }

  it should "keep prose formatting controls in semantic clusters when rows wrap" in {
    val stateManager = createStateManager("ContextualToolbarSpec-clustered-rows")

    stateManager.applyEvent(ResizeEvent(ViewportSize(120, 30))).unsafeRunSync()
    seedToolbarDocument(stateManager)

    val rowGroups = ContextualToolbarLayout
      .rowGroups(
        ContextualToolbar.itemsFor(stateManager.getCurrentState.unsafeRunSync()),
        24,
        ToolbarDisplayMode.IconOnly
      )
      .map(_.map(_.id))

    rowGroups shouldBe List(
      List("bold", "italic", "underline", "font-family", "font-family-text", "font-size"),
      List("color", "color-hex", "paragraph-role"),
      List("align-left", "align-center", "align-right", "align-justify")
    )
  }

  it should "use one compact row when the editor has room for all formatting controls" in {
    val stateManager = createStateManager("ContextualToolbarSpec-compact-row")

    stateManager.applyEvent(ResizeEvent(ViewportSize(120, 30))).unsafeRunSync()
    seedToolbarDocument(stateManager)

    val state        = stateManager.getCurrentState.unsafeRunSync()
    val toolbarState = ContextualToolbarState(displayMode = ToolbarDisplayMode.IconOnly)
    val width        = ContextualToolbarLayout.compactContentWidth(toolbarState, state, maxWidth = 120)

    width shouldBe 55
    ContextualToolbarLayout.rowCount(toolbarState, state, width) shouldBe 1
  }

  it should "move focus vertically between wrapped toolbar rows" in {
    val stateManager = createStateManager("ContextualToolbarSpec-vertical-top-level")

    stateManager.applyEvent(ResizeEvent(ViewportSize(26, 30))).unsafeRunSync()
    seedToolbarDocument(stateManager)
    stateManager.applyEvent(ToggleContextualToolbar).unsafeRunSync()

    val before       = stateManager.getCurrentState.unsafeRunSync()
    val toolbarState = toolbarStateFrom(before)
    val contentWidth = toolbarContentWidth(before)
    val rowGroups =
      ContextualToolbarLayout.rowGroups(ContextualToolbar.itemsFor(before), contentWidth, toolbarState.displayMode)
    rowGroups.length should be > 1

    val (startItemId, expectedDownItemId) = verticalTopLevelPair(rowGroups)
    moveToolbarFocusTo(stateManager, startItemId)
    stateManager.applyEvent(MoveDown).unsafeRunSync()
    focusedToolbarItemId(stateManager.getCurrentState.unsafeRunSync()) shouldBe expectedDownItemId

    stateManager.applyEvent(MoveUp).unsafeRunSync()
    focusedToolbarItemId(stateManager.getCurrentState.unsafeRunSync()) shouldBe startItemId
  }

  it should "move dropdown selection vertically between wrapped option rows" in {
    val items = List(
      ContextualToolbarItem.Dropdown(
        id = "paragraph-role",
        label = "Role",
        icon = "P",
        optionItem = CommandSurfaceItem.OptionItem(
          id = "paragraph-role",
          label = "Role",
          options = List(
            com.serenity.command.CommandOption("Body", CommandIntent.SetRichTextParagraphRole(ParagraphRole.Body)),
            com.serenity.command.CommandOption("H1", CommandIntent.SetRichTextParagraphRole(ParagraphRole.Heading(1))),
            com.serenity.command.CommandOption("H2", CommandIntent.SetRichTextParagraphRole(ParagraphRole.Heading(2))),
            com.serenity.command.CommandOption("H3", CommandIntent.SetRichTextParagraphRole(ParagraphRole.Heading(3)))
          ),
          selectedIndex = 1,
          category = CommandCategory.Edit
        )
      )
    )
    val toolbarState =
      ContextualToolbarState(detailState = Some(ContextualToolbarDetailState.Dropdown("paragraph-role", 1)))

    val (downItemId, downIndex) =
      ContextualToolbarLayout
        .detailSelectionAfterVerticalMove(toolbarState, items, contentWidth = 12, deltaRows = 1)
        .getOrElse(fail("Expected a moved dropdown selection"))
    val movedDown = toolbarState.withDetailSelectionIndex(downItemId, downIndex)
    movedDown.detailState shouldBe Some(ContextualToolbarDetailState.Dropdown("paragraph-role", 3))

    val (upItemId, upIndex) =
      ContextualToolbarLayout
        .detailSelectionAfterVerticalMove(movedDown, items, contentWidth = 12, deltaRows = -1)
        .getOrElse(fail("Expected a moved dropdown selection"))
    val movedUp = movedDown.withDetailSelectionIndex(upItemId, upIndex)
    movedUp.detailState shouldBe Some(ContextualToolbarDetailState.Dropdown("paragraph-role", 1))
  }
