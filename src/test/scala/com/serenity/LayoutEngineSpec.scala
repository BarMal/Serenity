package com.serenity

import com.serenity.config.{AppConfig, InterfaceDensity, TextAreaInsets}
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

  it should "use the full workspace width when no explicit text measure is configured" in {
    val state        = AppState.initial
    val viewportSize = ViewportSize(100, 30)

    val calculatedLayout = LayoutEngine.calculateLayout(state, viewportSize)

    calculatedLayout.leftSpacerRect.width shouldBe 0
    calculatedLayout.rightSpacerRect.width shouldBe 0
    calculatedLayout.lineNumberRect.map(_.width) shouldBe Some(3)
    calculatedLayout.editorPanelRect shouldBe LayoutRect(3, 0, 97, 29)
  }

  it should "remove single-pane chrome when the selected configuration disables pane headers" in {
    val state  = AppState.initial.copy(config = AppConfig.default.withPaneHeaders(false))
    val layout = LayoutEngine.calculateLayout(state, ViewportSize(100, 30))
    val pane   = LayoutEngine.calculateEditorPaneLayouts(state, layout)(PaneId(0))

    pane.headerRect.height shouldBe 0
    pane.contentRect.y shouldBe layout.editorPanelRect.y
    pane.contentRect.height shouldBe layout.editorPanelRect.height
  }

  it should "expose owned child rectangles for editor pane chrome and content" in {
    val state        = AppState.initial
    val viewportSize = ViewportSize(100, 30)

    val calculatedLayout = LayoutEngine.calculateLayout(state, viewportSize)
    val paneLayout       = LayoutEngine.calculateEditorPaneLayouts(state, calculatedLayout)(PaneId(0))

    paneLayout.paneRect shouldBe calculatedLayout.editorPanelRect
    paneLayout.headerRect shouldBe LayoutRect(0, 0, 100, 1)
    paneLayout.titleRect shouldBe paneLayout.headerRect
    paneLayout.contentRect shouldBe LayoutRect(3, 1, 97, 28)
  }

  it should "expose a single editor workspace contract for panes, line numbers, and gutter" in {
    val state = AppState.initial.copy(
      config = AppConfig.default
        .withLineNumbers(true)
        .withGutter(true)
        .copy(textAreaInsets = TextAreaInsets(left = 0.10, right = 0.20))
    )
    val viewportSize     = ViewportSize(100, 30)
    val calculatedLayout = LayoutEngine.calculateLayout(state, viewportSize)
    val workspaceLayout  = LayoutEngine.calculateEditorWorkspaceLayout(state, calculatedLayout)

    workspaceLayout.editorPanelRect shouldBe calculatedLayout.editorPanelRect
    workspaceLayout.lineNumberRect shouldBe calculatedLayout.lineNumberRect
    workspaceLayout.gutterRect shouldBe calculatedLayout.gutterRect
    workspaceLayout.paneLayouts shouldBe LayoutEngine.calculateEditorPaneLayouts(state, calculatedLayout)

    val activeHeader  = workspaceLayout.activeHeaderRect(state).getOrElse(fail("expected active header"))
    val activeContent = workspaceLayout.activeContentRect(state).getOrElse(fail("expected active content"))
    val lineNumbers   = workspaceLayout.lineNumberRect.getOrElse(fail("expected line numbers"))
    val gutter        = workspaceLayout.gutterRect.getOrElse(fail("expected gutter"))

    activeHeader.y shouldBe calculatedLayout.editorPanelRect.y
    activeHeader.height shouldBe 1
    activeContent.y shouldBe lineNumbers.y
    activeContent.bottom should be <= gutter.y
    lineNumbers.bottom should be <= gutter.y
    gutter.y shouldBe viewportSize.height - gutter.height
    gutter.bottom shouldBe viewportSize.height
  }

  it should "derive reusable line-number row slots from the shared workspace contract" in {
    val buffer = Buffer.fromString(BufferId(1), "alpha\nbeta\ngamma\ndelta")
    val state = AppState.initial.copy(
      buffers = Map(buffer.id -> buffer),
      bufferOrder = List(buffer.id),
      layout = AppState.initial.layout.copy(
        editorPanes = Map(PaneId(0) -> EditorPane.withBuffer(PaneId(0), buffer.id)),
        activeEditorPaneId = Some(PaneId(0)),
        paneOrder = List(PaneId(0))
      ),
      config = AppConfig.default.withLineNumbers(true)
    )
    val calculatedLayout = LayoutEngine.calculateLayout(state, ViewportSize(100, 30))
    val workspaceLayout  = LayoutEngine.calculateEditorWorkspaceLayout(state, calculatedLayout)

    workspaceLayout
      .lineNumberRowSlots(itemCount = 4)
      .map(slot => slot.kind -> slot.y)
      .shouldBe(
        List(
          SurfaceContentRowKind.Item(0) -> 1,
          SurfaceContentRowKind.Item(1) -> 2,
          SurfaceContentRowKind.Item(2) -> 3,
          SurfaceContentRowKind.Item(3) -> 4
        )
      )
  }

  it should "place cursors using the pane content rectangle owned by editor pane layout" in {
    val buffer = Buffer.fromString(BufferId(0), "abc\ndef").copy(cursors = List(CursorPosition(1, 2)))
    val state = AppState.initial.copy(
      buffers = Map(buffer.id -> buffer),
      bufferOrder = List(buffer.id),
      layout = AppState.initial.layout.copy(
        editorPanes = Map(PaneId(0) -> EditorPane.withBuffer(PaneId(0), buffer.id)),
        activeEditorPaneId = Some(PaneId(0))
      )
    )
    val calculatedLayout = LayoutEngine.calculateLayout(state, ViewportSize(100, 30))
    val paneLayout       = LayoutEngine.calculateEditorPaneLayouts(state, calculatedLayout)(PaneId(0))

    val cursorPosition = CursorLayout.calculateScreenPositionInContent(
      CursorPosition(1, 2),
      buffer.content,
      paneLayout.contentRect,
      buffer.viewport
    )

    cursorPosition shouldBe Some(ScreenPosition(paneLayout.contentRect.x + 2, paneLayout.contentRect.y + 1))
  }

  it should "delegate legacy LayoutManager layout calculation to the real layout engine" in {
    val state        = AppState.initial
    val viewportSize = ViewportSize(100, 30)

    LayoutManager.calculateLayout(state, viewportSize) shouldBe LayoutEngine.calculateLayout(state, viewportSize)
  }

  it should "apply text area insets inside the workspace without resizing pinned panels or line numbers" in {
    val buffer = Buffer.fromString(BufferId(0), "one\ntwo\nthree")
    val state = AppState.initial.copy(
      buffers = Map(buffer.id -> buffer),
      config = AppConfig.default.copy(
        textAreaInsets = TextAreaInsets(left = 0.10, right = 0.20, top = 0.10, bottom = 0.15)
      ),
      uiSurfaces = List(
        UiSurface(
          SurfaceId("left-panel"),
          SurfaceContent.Outline(Nil),
          SurfacePresentation.Pinned(PanelPosition.Left, 10)
        ),
        UiSurface(
          SurfaceId("right-panel"),
          SurfaceContent.Diagnostics(Nil),
          SurfacePresentation.Pinned(PanelPosition.Right, 20)
        )
      )
    )

    val calculatedLayout = LayoutEngine.calculateLayout(state, ViewportSize(100, 30))
    val paneLayouts      = LayoutEngine.calculatePaneLayouts(state, calculatedLayout)

    calculatedLayout.pinnedPanelRects(PanelPosition.Left).width shouldBe 10
    calculatedLayout.pinnedPanelRects(PanelPosition.Right).width shouldBe 20
    calculatedLayout.leftSpacerRect shouldBe LayoutRect(10, 0, 7, 29)
    calculatedLayout.rightSpacerRect shouldBe LayoutRect(66, 0, 14, 29)
    calculatedLayout.topSpacerRect shouldBe LayoutRect(17, 1, 49, 2)
    calculatedLayout.bottomSpacerRect shouldBe LayoutRect(17, 25, 49, 4)
    calculatedLayout.lineNumberRect shouldBe Some(LayoutRect(17, 3, 3, 22))
    calculatedLayout.editorPanelRect shouldBe LayoutRect(20, 0, 46, 29)
    LayoutEngine.calculateEditorPaneLayouts(state, calculatedLayout)(PaneId(0)).headerRect shouldBe
      LayoutRect(10, 0, 70, 1)
    LayoutEngine.calculateEditorPaneLayouts(state, calculatedLayout)(PaneId(0)).topSpacerRect shouldBe
      LayoutRect(20, 1, 46, 2)
    LayoutEngine.calculateEditorPaneLayouts(state, calculatedLayout)(PaneId(0)).contentRect shouldBe
      LayoutRect(20, 3, 46, 22)
    paneLayouts(PaneId(0)) shouldBe calculatedLayout.editorPanelRect
  }

  it should "apply configured gaps between pinned panels and the editor workspace" in {
    val state = AppState.initial.copy(
      config = AppConfig.default
        .copy(
          showLineNumbers = false,
          textAreaInsets = TextAreaInsets(left = 0.0, right = 0.0)
        )
        .withUiElementGap(2),
      uiSurfaces = List(
        UiSurface(
          SurfaceId("left-panel"),
          SurfaceContent.Outline(Nil),
          SurfacePresentation.Pinned(PanelPosition.Left, 10)
        ),
        UiSurface(
          SurfaceId("right-panel"),
          SurfaceContent.Diagnostics(Nil),
          SurfacePresentation.Pinned(PanelPosition.Right, 20)
        ),
        UiSurface(
          SurfaceId("top-panel"),
          SurfaceContent.Terminal("Build", 0),
          SurfacePresentation.Pinned(PanelPosition.Top, 3)
        ),
        UiSurface(
          SurfaceId("bottom-panel"),
          SurfaceContent.Diagnostics(Nil),
          SurfacePresentation.Pinned(PanelPosition.Bottom, 4)
        )
      )
    )

    val calculatedLayout = LayoutEngine.calculateLayout(state, ViewportSize(100, 30))

    calculatedLayout.pinnedPanelRects(PanelPosition.Left) shouldBe LayoutRect(0, 3, 10, 22)
    calculatedLayout.pinnedPanelRects(PanelPosition.Right) shouldBe LayoutRect(80, 3, 20, 22)
    calculatedLayout.pinnedPanelRects(PanelPosition.Top) shouldBe LayoutRect(0, 0, 100, 3)
    calculatedLayout.pinnedPanelRects(PanelPosition.Bottom) shouldBe LayoutRect(0, 25, 100, 4)
    calculatedLayout.editorPanelRect.x shouldBe 12
    calculatedLayout.editorPanelRect.y shouldBe 5
    calculatedLayout.editorPanelRect.height shouldBe 18
    calculatedLayout.editorPanelRect.right shouldBe 78
    calculatedLayout.editorPanelRect.bottom shouldBe 23
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

  it should "split editor area between panes vertically when the layout requests vertical splits" in {
    val pane1 = EditorPane.empty(PaneId(0))
    val pane2 = EditorPane.empty(PaneId(1))
    val layout = Layout(
      editorPanes = Map(PaneId(0) -> pane1, PaneId(1) -> pane2),
      activeEditorPaneId = Some(PaneId(0)),
      paneOrder = List(PaneId(0), PaneId(1)),
      splitDirection = PaneSplitDirection.Vertical
    )
    val state = AppState(
      layout = layout,
      buffers = Map.empty,
      focus = Focus.EditorPane(PaneId(0))
    )

    val calculatedLayout = LayoutEngine.calculateLayout(state, ViewportSize(100, 30))
    val paneLayouts      = LayoutEngine.calculatePaneLayouts(state, calculatedLayout)
    val editorRect       = calculatedLayout.editorPanelRect

    paneLayouts(PaneId(0)) shouldBe LayoutRect(editorRect.x, editorRect.y, editorRect.width, 15)
    paneLayouts(PaneId(1)) shouldBe LayoutRect(editorRect.x, editorRect.y + 15, editorRect.width, 14)
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

    // Then: With minimum width constraints, as many panes as fit should be visible
    paneLayouts should have size 3 // All panes exist in layout

    val editorRect = calculatedLayout.editorPanelRect

    val pane0Layout = paneLayouts(PaneId(0))
    val pane1Layout = paneLayouts(PaneId(1))
    val pane2Layout = paneLayouts(PaneId(2))

    pane0Layout shouldBe LayoutRect(editorRect.x, editorRect.y, editorRect.width / 2, editorRect.height)
    pane1Layout.x shouldBe editorRect.x + editorRect.width / 2
    pane1Layout.y shouldBe editorRect.y
    pane1Layout.height shouldBe editorRect.height

    // Other panes should be positioned off-screen (left or right)
    val editorRight = editorRect.x + editorRect.width
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
    val editorWidth = calculatedLayout.editorPanelRect.width
    editorWidth / minPaneWidth

    // Should return layouts for all panes, but only some visible
    paneLayouts should have size 5

    // First pane should be visible and use full editor width
    val visiblePane = paneLayouts(PaneId(0))
    visiblePane.width should be >= minPaneWidth

    // Panes beyond the visible capacity should be positioned off-screen.
    for i <- 2 until 5 do
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

    compact.editorPanelRect shouldBe comfortable.editorPanelRect
    spacious.editorPanelRect.x shouldBe comfortable.editorPanelRect.x
    spacious.editorPanelRect.width shouldBe comfortable.editorPanelRect.width
    compact.gutterRect.map(_.height) shouldBe Some(1)
    spacious.gutterRect.map(_.height) shouldBe Some(2)
    compact.belowCursorOverlayRect.map(_.height) should be < comfortable.belowCursorOverlayRect.map(_.height)
    spacious.belowCursorOverlayRect.map(_.height) should be > comfortable.belowCursorOverlayRect.map(_.height)
  }

  it should "keep the command palette compact while clamping it to a narrow viewport" in {
    val runner = com.serenity.command.CommandRunner.empty.activate(
      com.serenity.command.CommandRegistry.default,
      com.serenity.config.AppConfig.default
    )
    val surface = UiSurface(
      SurfaceId("command-runner"),
      SurfaceContent.CommandPalette(runner),
      SurfacePresentation.Floating(Some(CursorPosition(0, 0)), SurfacePlacement.BelowCursor)
    )
    val bufferId = BufferId(1)
    val state = AppState.initial.copy(
      buffers = Map(bufferId -> Buffer.fromString(bufferId, "palette")),
      bufferOrder = List(bufferId),
      layout = Layout(
        editorPanes = Map(PaneId(0) -> EditorPane.withBuffer(PaneId(0), bufferId)),
        activeEditorPaneId = Some(PaneId(0))
      ),
      uiSurfaces = List(surface)
    )

    LayoutEngine.calculateLayout(state, ViewportSize(100, 30)).belowCursorOverlayRect.map(_.width) shouldBe Some(72)
    LayoutEngine.calculateLayout(state, ViewportSize(40, 30)).belowCursorOverlayRect.map(_.width) shouldBe Some(37)
  }

  it should "leave non-runner floating surfaces at their available width" in {
    val bufferId = BufferId(1)
    val surface = UiSurface(
      SurfaceId("file-search"),
      SurfaceContent.FileSearch(FileSearchState("", Nil, selectedIndex = 0)),
      SurfacePresentation.Floating(Some(CursorPosition(0, 0)), SurfacePlacement.BelowCursor)
    )
    val state = AppState.initial.copy(
      buffers = Map(bufferId -> Buffer.fromString(bufferId, "search")),
      bufferOrder = List(bufferId),
      layout = Layout(
        editorPanes = Map(PaneId(0) -> EditorPane.withBuffer(PaneId(0), bufferId)),
        activeEditorPaneId = Some(PaneId(0))
      ),
      uiSurfaces = List(surface)
    )

    LayoutEngine.calculateLayout(state, ViewportSize(100, 30)).belowCursorOverlayRect.map(_.width) shouldBe Some(97)
  }
