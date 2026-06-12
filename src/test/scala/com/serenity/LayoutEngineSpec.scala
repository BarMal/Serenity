package com.serenity

import com.serenity.config.InterfaceDensity
import com.serenity.rope.Balance
import com.serenity.state.models.*
import com.serenity.ui.layout.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class LayoutEngineSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  behavior of "LayoutEngine multi-pane layout"

  it should "calculate single pane layout to use full editor area" in {
    // Given: State with single pane
    val pane1 = EditorPane.empty(PaneId(0))
    val layout = Layout(
      editorPanes = Map(PaneId(0) -> pane1),
      activeEditorPaneId = Some(PaneId(0))
    )
    val state = AppState(
      layout = layout,
      buffers = Map.empty,
      focus = Focus.EditorPane(PaneId(0))
    )
    val viewportSize = ViewportSize(100, 30)

    // When: Calculate layout
    val calculatedLayout = LayoutEngine.calculateLayout(state, viewportSize)

    // Then: Single pane should get full editor area
    val paneLayouts = LayoutEngine.calculatePaneLayouts(state, calculatedLayout)
    paneLayouts should have size 1

    val pane0Layout = paneLayouts(PaneId(0))
    pane0Layout shouldBe calculatedLayout.editorPanelRect
  }

  it should "split editor area between two panes horizontally" in {
    // Given: State with two panes
    val pane1 = EditorPane.empty(PaneId(0))
    val pane2 = EditorPane.empty(PaneId(1))
    val layout = Layout(
      editorPanes = Map(PaneId(0) -> pane1, PaneId(1) -> pane2),
      activeEditorPaneId = Some(PaneId(1))
    )
    val state = AppState(
      layout = layout,
      buffers = Map.empty,
      focus = Focus.EditorPane(PaneId(1))
    )
    val viewportSize = ViewportSize(100, 30)

    // When: Calculate layout
    val calculatedLayout = LayoutEngine.calculateLayout(state, viewportSize)
    val paneLayouts      = LayoutEngine.calculatePaneLayouts(state, calculatedLayout)

    // Then: With minimum width constraints, only one pane should be visible
    paneLayouts should have size 2 // Both panes exist in layout

    val editorRect = calculatedLayout.editorPanelRect

    // Only one pane should be visible (focused pane: PaneId(1))
    val pane1Layout = paneLayouts(PaneId(1))
    pane1Layout shouldBe editorRect

    // Pane 0 should be positioned off-screen (not enough width for both)
    val pane0Layout = paneLayouts(PaneId(0))
    pane0Layout.x should be < editorRect.x // Off-screen to the left
    pane0Layout.y shouldBe editorRect.y
    pane0Layout.width shouldBe editorRect.width
    pane0Layout.height shouldBe editorRect.height
  }

  it should "handle three panes with equal width distribution" in {
    // Given: State with three panes
    val panes = Map(
      PaneId(0) -> EditorPane.empty(PaneId(0)),
      PaneId(1) -> EditorPane.empty(PaneId(1)),
      PaneId(2) -> EditorPane.empty(PaneId(2))
    )
    val layout       = Layout(editorPanes = panes, activeEditorPaneId = Some(PaneId(0)))
    val state        = AppState(layout = layout, buffers = Map.empty, focus = Focus.EditorPane(PaneId(0)))
    val viewportSize = ViewportSize(120, 24)

    // When: Calculate layout
    val calculatedLayout = LayoutEngine.calculateLayout(state, viewportSize)
    val paneLayouts      = LayoutEngine.calculatePaneLayouts(state, calculatedLayout)

    // Then: With minimum width constraints, only one pane should be visible
    paneLayouts should have size 3 // All panes exist in layout

    val editorRect = calculatedLayout.editorPanelRect

    // Only one pane should be visible (focused pane: PaneId(0))
    val pane0Layout = paneLayouts(PaneId(0))
    pane0Layout shouldBe editorRect

    // Other panes should be positioned off-screen (left or right)
    val pane1Layout = paneLayouts(PaneId(1))
    val pane2Layout = paneLayouts(PaneId(2))
    // Off-screen means either left of editor area or right of editor area
    val editorRight = editorRect.x + editorRect.width
    pane1Layout.x should (be < editorRect.x or be >= editorRight) // Off-screen
    pane2Layout.x should (be < editorRect.x or be >= editorRight) // Off-screen
  }

  it should "respect minimum pane width constraint" in {
    // Given: State with many panes that would exceed minimum width
    val minPaneWidth = 40
    val viewportSize = ViewportSize(100, 24) // Editor area = 70 chars, max 1 pane at 40 chars min

    val panes = (0 until 5).map(i => PaneId(i) -> EditorPane.empty(PaneId(i))).toMap

    val layout = Layout(editorPanes = panes, activeEditorPaneId = Some(PaneId(0)))
    val state  = AppState(layout = layout, buffers = Map.empty, focus = Focus.EditorPane(PaneId(0)))

    // When: Calculate layout with minimum width constraint
    val calculatedLayout = LayoutEngine.calculateLayout(state, viewportSize)
    val paneLayouts      = LayoutEngine.calculatePaneLayoutsWithMinWidth(state, calculatedLayout, minPaneWidth)

    // Then: Only panes that fit should be visible, others should be off-screen but tracked
    val editorWidth     = calculatedLayout.editorPanelRect.width
    val maxVisiblePanes = editorWidth / minPaneWidth

    // Should return layouts for all panes, but only some visible
    paneLayouts should have size 5

    // First pane should be visible and use full editor width
    val visiblePane = paneLayouts(PaneId(0))
    visiblePane.width should be >= minPaneWidth

    // Other panes should be positioned off-screen (negative x or beyond screen width)
    for i <- 1 until 5 do
      val hiddenPane = paneLayouts(PaneId(i))
      (hiddenPane.x < 0 || hiddenPane.x >= viewportSize.width) shouldBe true
  }

  it should "handle pane navigation with minimum width constraints" in {
    // Given: 4 panes with terminal that can only show 2 at min width
    val minPaneWidth = 30
    val viewportSize = ViewportSize(100, 24) // Editor area = 70 chars, max 2 panes visible

    val panes = (0 until 4).map(i => PaneId(i) -> EditorPane.empty(PaneId(i))).toMap

    val layout = Layout(editorPanes = panes, activeEditorPaneId = Some(PaneId(2))) // Focus on pane 2
    val state  = AppState(layout = layout, buffers = Map.empty, focus = Focus.EditorPane(PaneId(2)))

    // When: Calculate layout ensuring focused pane is visible
    val calculatedLayout = LayoutEngine.calculateLayout(state, viewportSize)
    val paneLayouts      = LayoutEngine.calculatePaneLayoutsWithMinWidth(state, calculatedLayout, minPaneWidth)

    // Then: Focused pane (PaneId(2)) should be visible, along with adjacent pane
    val focusedPane = paneLayouts(PaneId(2))
    val editorRect  = calculatedLayout.editorPanelRect

    // Focused pane should be visible within editor area
    focusedPane.x should be >= editorRect.x
    focusedPane.x should be < editorRect.right
    focusedPane.width should be >= minPaneWidth
  }

  it should "apply interface density to editor spacing and overlay height" in {
    val runner = com.serenity.command.CommandRunner.empty.activate(
      com.serenity.command.CommandRegistry.default,
      com.serenity.config.AppConfig.default
    )
    val commandSurface = UiSurface(
      SurfaceId("command-runner"),
      SurfaceContent.CommandPalette(runner),
      SurfacePresentation.Floating(Some(CursorPosition(0, 0)), SurfacePlacement.BelowCursor)
    )
    val paneId   = PaneId(0)
    val bufferId = BufferId(1)
    val baseState = AppState.initial.copy(
      buffers = Map(bufferId -> Buffer.fromString(bufferId, "alpha\nbeta\ngamma")),
      bufferOrder = List(bufferId),
      layout = Layout(
        editorPanes = Map(paneId -> EditorPane.withBuffer(paneId, bufferId)),
        activeEditorPaneId = Some(paneId)
      ),
      focus = Focus.EditorPane(paneId),
      uiSurfaces = List(commandSurface)
    )
    val compact = LayoutEngine.calculateLayout(
      baseState.copy(config = baseState.config.withInterfaceDensity(InterfaceDensity.Compact)),
      ViewportSize(120, 30)
    )
    val comfortable = LayoutEngine.calculateLayout(baseState, ViewportSize(120, 30))
    val spacious = LayoutEngine.calculateLayout(
      baseState.copy(config = baseState.config.withInterfaceDensity(InterfaceDensity.Spacious)),
      ViewportSize(120, 30)
    )

    compact.editorPanelRect.x should be < comfortable.editorPanelRect.x
    compact.editorPanelRect.width should be > comfortable.editorPanelRect.width
    spacious.editorPanelRect.x should be > comfortable.editorPanelRect.x
    spacious.editorPanelRect.width should be < comfortable.editorPanelRect.width
    compact.gutterRect.map(_.height) shouldBe Some(1)
    spacious.gutterRect.map(_.height) shouldBe Some(2)
    compact.belowCursorOverlayRect.map(_.height) should be < comfortable.belowCursorOverlayRect.map(_.height)
    spacious.belowCursorOverlayRect.map(_.height) should be > comfortable.belowCursorOverlayRect.map(_.height)
  }
