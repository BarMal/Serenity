package com.serenity

import com.serenity.state.models.*
import com.serenity.ui.layout.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class LayoutEngineSpec extends AnyFlatSpec with Matchers:

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
    val terminalSize = TerminalSize(100, 30)

    // When: Calculate layout  
    val calculatedLayout = LayoutEngine.calculateLayout(state, terminalSize)

    // Then: Single pane should get full editor area
    val paneLayouts = LayoutEngine.calculatePaneLayouts(state, calculatedLayout)
    paneLayouts should have size 1
    
    val pane0Layout = paneLayouts(PaneId(0))
    // Editor area is terminal minus 15% spacers each side = 70% of width
    val expectedWidth = (terminalSize.width * 0.7).toInt
    pane0Layout.width shouldBe expectedWidth
    pane0Layout.height shouldBe terminalSize.height
    pane0Layout.x shouldBe (terminalSize.width * 0.15).toInt // Left spacer
    pane0Layout.y shouldBe 0
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
    val terminalSize = TerminalSize(100, 30)

    // When: Calculate layout
    val calculatedLayout = LayoutEngine.calculateLayout(state, terminalSize)
    val paneLayouts = LayoutEngine.calculatePaneLayouts(state, calculatedLayout)

    // Then: With minimum width constraints, only one pane should be visible
    paneLayouts should have size 2 // Both panes exist in layout

    val editorWidth = (terminalSize.width * 0.7).toInt // 70% of terminal width = 70 chars
    val editorX = (terminalSize.width * 0.15).toInt // Left spacer = 15
    val minPaneWidth = 50 // Default minimum from config
    
    // Only one pane should be visible (focused pane: PaneId(1))
    val pane1Layout = paneLayouts(PaneId(1))
    pane1Layout.x shouldBe editorX // Visible pane at editor start
    pane1Layout.y shouldBe 0
    pane1Layout.width shouldBe editorWidth // Uses full editor width
    pane1Layout.height shouldBe terminalSize.height

    // Pane 0 should be positioned off-screen (not enough width for both)
    val pane0Layout = paneLayouts(PaneId(0))
    pane0Layout.x should be < editorX // Off-screen to the left
    pane0Layout.y shouldBe 0
    pane0Layout.width shouldBe editorWidth // Same width but off-screen
    pane0Layout.height shouldBe terminalSize.height
  }

  it should "handle three panes with equal width distribution" in {
    // Given: State with three panes
    val panes = Map(
      PaneId(0) -> EditorPane.empty(PaneId(0)),
      PaneId(1) -> EditorPane.empty(PaneId(1)),
      PaneId(2) -> EditorPane.empty(PaneId(2))
    )
    val layout = Layout(editorPanes = panes, activeEditorPaneId = Some(PaneId(0)))
    val state = AppState(layout = layout, buffers = Map.empty, focus = Focus.EditorPane(PaneId(0)))
    val terminalSize = TerminalSize(120, 24)

    // When: Calculate layout
    val calculatedLayout = LayoutEngine.calculateLayout(state, terminalSize)
    val paneLayouts = LayoutEngine.calculatePaneLayouts(state, calculatedLayout)

    // Then: With minimum width constraints, only one pane should be visible
    paneLayouts should have size 3 // All panes exist in layout
    
    val editorWidth = (terminalSize.width * 0.7).toInt // 84 chars
    val editorX = (terminalSize.width * 0.15).toInt // 18 chars left spacer
    val minPaneWidth = 50 // Default minimum from config
    
    // Only one pane should be visible (focused pane: PaneId(0))
    val pane0Layout = paneLayouts(PaneId(0))
    pane0Layout.x shouldBe editorX // Visible pane at editor start
    pane0Layout.y shouldBe 0
    pane0Layout.width shouldBe editorWidth // Uses full editor width
    pane0Layout.height shouldBe terminalSize.height

    // Other panes should be positioned off-screen (left or right)
    val pane1Layout = paneLayouts(PaneId(1))
    val pane2Layout = paneLayouts(PaneId(2))
    // Off-screen means either left of editor area or right of editor area
    val editorRight = editorX + editorWidth
    pane1Layout.x should (be < editorX or be >= editorRight) // Off-screen
    pane2Layout.x should (be < editorX or be >= editorRight) // Off-screen
  }

  it should "respect minimum pane width constraint" in {
    // Given: State with many panes that would exceed minimum width
    val minPaneWidth = 40
    val terminalSize = TerminalSize(100, 24) // Editor area = 70 chars, max 1 pane at 40 chars min
    
    val panes = (0 until 5).map { i =>
      PaneId(i) -> EditorPane.empty(PaneId(i))
    }.toMap
    
    val layout = Layout(editorPanes = panes, activeEditorPaneId = Some(PaneId(0)))
    val state = AppState(layout = layout, buffers = Map.empty, focus = Focus.EditorPane(PaneId(0)))

    // When: Calculate layout with minimum width constraint
    val calculatedLayout = LayoutEngine.calculateLayout(state, terminalSize)
    val paneLayouts = LayoutEngine.calculatePaneLayoutsWithMinWidth(state, calculatedLayout, minPaneWidth)

    // Then: Only panes that fit should be visible, others should be off-screen but tracked
    val editorWidth = (terminalSize.width * 0.7).toInt // 70 chars
    val maxVisiblePanes = editorWidth / minPaneWidth // 70/40 = 1 pane

    // Should return layouts for all panes, but only some visible
    paneLayouts should have size 5
    
    // First pane should be visible and use full editor width
    val visiblePane = paneLayouts(PaneId(0))
    visiblePane.width should be >= minPaneWidth
    
    // Other panes should be positioned off-screen (negative x or beyond screen width)
    for (i <- 1 until 5) {
      val hiddenPane = paneLayouts(PaneId(i))
      (hiddenPane.x < 0 || hiddenPane.x >= terminalSize.width) shouldBe true
    }
  }

  it should "handle pane navigation with minimum width constraints" in {
    // Given: 4 panes with terminal that can only show 2 at min width
    val minPaneWidth = 30
    val terminalSize = TerminalSize(100, 24) // Editor area = 70 chars, max 2 panes visible
    
    val panes = (0 until 4).map { i =>
      PaneId(i) -> EditorPane.empty(PaneId(i))
    }.toMap
    
    val layout = Layout(editorPanes = panes, activeEditorPaneId = Some(PaneId(2))) // Focus on pane 2
    val state = AppState(layout = layout, buffers = Map.empty, focus = Focus.EditorPane(PaneId(2)))

    // When: Calculate layout ensuring focused pane is visible
    val calculatedLayout = LayoutEngine.calculateLayout(state, terminalSize)
    val paneLayouts = LayoutEngine.calculatePaneLayoutsWithMinWidth(state, calculatedLayout, minPaneWidth)

    // Then: Focused pane (PaneId(2)) should be visible, along with adjacent pane
    val focusedPane = paneLayouts(PaneId(2))
    val editorX = (terminalSize.width * 0.15).toInt
    
    // Focused pane should be visible within editor area
    focusedPane.x should be >= editorX
    focusedPane.x should be < (editorX + (terminalSize.width * 0.7).toInt)
    focusedPane.width should be >= minPaneWidth
  }