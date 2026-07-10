package com.serenity

import com.serenity.command.{CommandRegistry, CommandRunner}
import com.serenity.config.{AppConfig, InterfaceDensity, TextAreaInsets}
import com.serenity.rope.Balance
import com.serenity.state.models.*
import com.serenity.ui.layout.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class LayoutContractSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private val viewport = ViewportSize(120, 36)

  private def viewportRect: LayoutRect =
    LayoutRect(0, 0, viewport.width, viewport.height)

  private def contentAreaFor(layout: CalculatedLayout): LayoutRect =
    layout.gutterRect match
      case Some(gutter) => LayoutRect(0, 0, viewport.width, gutter.y)
      case None         => viewportRect

  private def assertInside(owner: LayoutRect, child: LayoutRect, clue: String): Unit =
    withClue(clue) {
      owner.containsRect(child) shouldBe true
    }

  "LayoutRect" should "define explicit containment semantics for cell-space rectangles" in {
    val outer = LayoutRect(2, 3, 10, 5)

    outer.contains(2, 3) shouldBe true
    outer.contains(11, 7) shouldBe true
    outer.contains(12, 7) shouldBe false
    outer.contains(11, 8) shouldBe false
    outer.containsRect(LayoutRect(4, 4, 2, 2)) shouldBe true
    outer.containsRect(LayoutRect(4, 4, 9, 2)) shouldBe false
  }

  it should "keep editor, panel, line-number, and gutter rectangles within their owned viewport regions" in
    List(InterfaceDensity.Compact, InterfaceDensity.Comfortable, InterfaceDensity.Spacious).foreach { density =>
      val state = AppState.initial.copy(
        config = AppConfig.default
          .withInterfaceDensity(density)
          .withLineNumbers(true)
          .withGutter(true)
          .copy(
            textAreaInsets = TextAreaInsets(left = 0.05, right = 0.10)
          )
          .withUiElementGap(2),
        uiSurfaces = List(
          UiSurface(
            SurfaceId("left-panel"),
            SurfaceContent.Outline(Nil),
            SurfacePresentation.Pinned(PanelPosition.Left, 14)
          ),
          UiSurface(
            SurfaceId("right-panel"),
            SurfaceContent.Diagnostics(Nil),
            SurfacePresentation.Pinned(PanelPosition.Right, 18)
          ),
          UiSurface(
            SurfaceId("top-panel"),
            SurfaceContent.Terminal("Build", 0),
            SurfacePresentation.Pinned(PanelPosition.Top, 4)
          ),
          UiSurface(
            SurfaceId("bottom-panel"),
            SurfaceContent.Diagnostics(Nil),
            SurfacePresentation.Pinned(PanelPosition.Bottom, 5)
          )
        )
      )
      val layout          = LayoutEngine.calculateLayout(state, viewport)
      val workspaceLayout = LayoutEngine.calculateEditorWorkspaceLayout(state, layout)
      val contentArea     = contentAreaFor(layout)

      layout.gutterRect.foreach { gutter =>
        withClue(s"density=$density gutter") {
          gutter.x shouldBe 0
          gutter.width shouldBe viewport.width
          gutter.bottom shouldBe viewport.height
        }
      }

      List(
        "editor panel" -> layout.editorPanelRect,
        "left spacer"  -> layout.leftSpacerRect,
        "right spacer" -> layout.rightSpacerRect
      ).foreach {
        case (name, rect) =>
          assertInside(contentArea, rect, s"density=$density $name")
      }

      layout.lineNumberRect.foreach(rect => assertInside(contentArea, rect, s"density=$density line numbers"))
      layout.pinnedPanelRects.foreach {
        case (position, rect) =>
          assertInside(contentArea, rect, s"density=$density pinned panel $position")
      }
      layout.pinnedSurfaceRects.foreach {
        case (surfaceId, rect) =>
          assertInside(contentArea, rect, s"density=$density pinned surface $surfaceId")
      }

      val activePane = workspaceLayout.activePaneLayout(state).getOrElse(fail("expected active pane layout"))
      assertInside(layout.editorPanelRect, activePane.paneRect, s"density=$density active pane")
      assertInside(activePane.paneRect, activePane.contentRect, s"density=$density active content")
      assertInside(contentArea, activePane.headerRect, s"density=$density active header")
      activePane.headerRect.bottom shouldBe activePane.contentRect.y
      layout.gutterRect.foreach(gutter => activePane.contentRect.bottom should be <= gutter.y)
    }

  it should "keep below-cursor overlays inside the active editor content rectangle" in {
    val buffer = Buffer
      .fromString(BufferId(1), "alpha\nbeta\ngamma\ndelta")
      .copy(cursors = List(CursorPosition(1, 2)))
    val runner = CommandRunner.empty.activate(CommandRegistry.default, AppConfig.default)
    val state = AppState.initial.copy(
      buffers = Map(buffer.id -> buffer),
      bufferOrder = List(buffer.id),
      layout = AppState.initial.layout.copy(
        editorPanes = Map(PaneId(0) -> EditorPane.withBuffer(PaneId(0), buffer.id)),
        activeEditorPaneId = Some(PaneId(0)),
        paneOrder = List(PaneId(0))
      ),
      focus = Focus.Surface(SurfaceId("command-runner")),
      uiSurfaces = List(
        UiSurface(
          SurfaceId("command-runner"),
          SurfaceContent.CommandPalette(runner),
          SurfacePresentation.Floating(Some(CursorPosition(1, 2)), SurfacePlacement.BelowCursor)
        )
      )
    )
    val layout       = LayoutEngine.calculateLayout(state, viewport)
    val contentRect  = LayoutEngine.calculateEditorWorkspaceLayout(state, layout).activeContentRect(state).get
    val overlayRects = layout.belowCursorOverlayStack.map(_._2)

    overlayRects should not be empty
    overlayRects.foreach(rect => assertInside(contentRect, rect, s"overlay $rect"))
  }

  it should "keep above-cursor overlays inside the active editor content rectangle" in {
    val buffer = Buffer
      .fromString(BufferId(1), "alpha\nbeta\ngamma\ndelta")
      .copy(cursors = List(CursorPosition(1, 2)))
    val state = AppState.initial.copy(
      buffers = Map(buffer.id -> buffer),
      bufferOrder = List(buffer.id),
      layout = AppState.initial.layout.copy(
        editorPanes = Map(PaneId(0) -> EditorPane.withBuffer(PaneId(0), buffer.id)),
        activeEditorPaneId = Some(PaneId(0)),
        paneOrder = List(PaneId(0))
      ),
      focus = Focus.Surface(SurfaceId("quick-info")),
      uiSurfaces = List(
        UiSurface(
          SurfaceId("quick-info"),
          SurfaceContent.QuickInfo("List.map(f)"),
          SurfacePresentation.Floating(Some(CursorPosition(1, 2)), SurfacePlacement.AboveCursor)
        )
      )
    )
    val layout      = LayoutEngine.calculateLayout(state, viewport)
    val contentRect = LayoutEngine.calculateEditorWorkspaceLayout(state, layout).activeContentRect(state).get
    val overlayRect = layout.aboveCursorOverlayRect.getOrElse(fail("expected above-cursor overlay"))

    assertInside(contentRect, overlayRect, s"above-cursor overlay $overlayRect")
  }

  it should "keep stacked below-cursor command surfaces inside tiny active editor content rectangles" in {
    val tinyViewport = ViewportSize(32, 6)
    val cursor       = CursorPosition(1, 2)
    val buffer = Buffer
      .fromString(BufferId(1), "alpha\nbeta\ngamma\ndelta")
      .copy(cursors = List(cursor))
    val runner = CommandRunner.empty.activate(CommandRegistry.default, AppConfig.default)
    val state = AppState.initial.copy(
      config = AppConfig.default.copy(
        showLineNumbers = false,
        showGutter = false,
        textAreaInsets = TextAreaInsets()
      ),
      buffers = Map(buffer.id -> buffer),
      bufferOrder = List(buffer.id),
      layout = AppState.initial.layout.copy(
        editorPanes = Map(PaneId(0) -> EditorPane.withBuffer(PaneId(0), buffer.id)),
        activeEditorPaneId = Some(PaneId(0)),
        paneOrder = List(PaneId(0))
      ),
      focus = Focus.Surface(SurfaceId("command-runner-submenu")),
      uiSurfaces = List(
        UiSurface(
          SurfaceId("command-runner"),
          SurfaceContent.CommandPalette(runner),
          SurfacePresentation.Floating(Some(cursor), SurfacePlacement.BelowCursor)
        ),
        UiSurface(
          SurfaceId("command-runner-submenu"),
          SurfaceContent.CommandPaletteSubmenu(runner, "settings-animation", previewOnly = false),
          SurfacePresentation.Floating(Some(cursor), SurfacePlacement.BelowCursor)
        )
      )
    )

    val layout      = LayoutEngine.calculateLayout(state, tinyViewport)
    val contentRect = LayoutEngine.calculateEditorWorkspaceLayout(state, layout).activeContentRect(state).get
    val stack       = layout.belowCursorOverlayStack

    stack.map(_._1) shouldBe List(SurfaceId("command-runner"), SurfaceId("command-runner-submenu"))
    stack.foreach { case (surfaceId, rect) => assertInside(contentRect, rect, s"$surfaceId") }
    val stackById   = stack.toMap
    val runnerRect  = stackById(SurfaceId("command-runner"))
    val submenuRect = stackById(SurfaceId("command-runner-submenu"))
    runnerRect.bottom should be <= submenuRect.y
  }

  it should "reserve configured gaps before clamping oversized pinned side panels" in {
    val constrainedViewport = ViewportSize(20, 8)
    val gap                 = 2
    val state = AppState.initial.copy(
      config = AppConfig.default
        .copy(
          showLineNumbers = false,
          showGutter = false,
          textAreaInsets = TextAreaInsets(left = 0.0, right = 0.0)
        )
        .withUiElementGap(gap),
      uiSurfaces = List(
        UiSurface(
          SurfaceId("left-panel"),
          SurfaceContent.Outline(Nil),
          SurfacePresentation.Pinned(PanelPosition.Left, 15)
        ),
        UiSurface(
          SurfaceId("right-panel"),
          SurfaceContent.Diagnostics(Nil),
          SurfacePresentation.Pinned(PanelPosition.Right, 10)
        )
      )
    )

    val layout       = LayoutEngine.calculateLayout(state, constrainedViewport)
    val viewportRect = LayoutRect(0, 0, constrainedViewport.width, constrainedViewport.height)
    val leftPanel    = layout.pinnedPanelRects(PanelPosition.Left)
    val rightPanel   = layout.pinnedPanelRects(PanelPosition.Right)

    assertInside(viewportRect, leftPanel, "left panel")
    assertInside(viewportRect, rightPanel, "right panel")
    assertInside(viewportRect, layout.editorPanelRect, "editor panel")
    leftPanel.right + gap should be <= layout.editorPanelRect.x
    layout.editorPanelRect.right + gap should be <= rightPanel.x
  }

  it should "reserve configured gaps before clamping oversized pinned top and bottom panels" in {
    val constrainedViewport = ViewportSize(18, 9)
    val gap                 = 2
    val state = AppState.initial.copy(
      config = AppConfig.default
        .copy(
          showLineNumbers = false,
          showGutter = false,
          textAreaInsets = TextAreaInsets(left = 0.0, right = 0.0, top = 0.0, bottom = 0.0)
        )
        .withUiElementGap(gap),
      uiSurfaces = List(
        UiSurface(
          SurfaceId("top-panel"),
          SurfaceContent.Terminal("Build", 0),
          SurfacePresentation.Pinned(PanelPosition.Top, 6)
        ),
        UiSurface(
          SurfaceId("bottom-panel"),
          SurfaceContent.Diagnostics(Nil),
          SurfacePresentation.Pinned(PanelPosition.Bottom, 5)
        )
      )
    )

    val layout       = LayoutEngine.calculateLayout(state, constrainedViewport)
    val viewportRect = LayoutRect(0, 0, constrainedViewport.width, constrainedViewport.height)
    val topPanel     = layout.pinnedPanelRects(PanelPosition.Top)
    val bottomPanel  = layout.pinnedPanelRects(PanelPosition.Bottom)

    assertInside(viewportRect, topPanel, "top panel")
    assertInside(viewportRect, bottomPanel, "bottom panel")
    assertInside(viewportRect, layout.editorPanelRect, "editor panel")
    topPanel.bottom + gap should be <= layout.editorPanelRect.y
    layout.editorPanelRect.bottom + gap should be <= bottomPanel.y
  }

  it should "apply horizontal spacer overrides consistently to workspace and active pane bounds" in {
    val state = AppState.initial.copy(
      config = AppConfig.default.copy(
        showLineNumbers = false,
        showGutter = false,
        textAreaInsets = TextAreaInsets()
      )
    )

    val spacerPercentage = 0.25
    val layout           = LayoutEngine.calculateLayout(state, viewport, spacerPercentage)
    val paneLayout = LayoutEngine
      .calculateEditorWorkspaceLayout(state, layout)
      .activePaneLayout(state)
      .getOrElse(fail("expected active pane layout"))

    layout.leftSpacerRect.width shouldBe (viewport.width * spacerPercentage).toInt
    layout.rightSpacerRect.width shouldBe (viewport.width * spacerPercentage).toInt
    paneLayout.paneRect shouldBe layout.editorPanelRect
    paneLayout.headerRect.x shouldBe layout.leftSpacerRect.x
    paneLayout.headerRect.right shouldBe layout.rightSpacerRect.right
    paneLayout.contentRect.x shouldBe layout.editorPanelRect.x
    paneLayout.contentRect.width shouldBe layout.editorPanelRect.width
  }

  it should "expose reusable contract violations for editor, pane, panel, gutter, and overlay ownership" in {
    val cursor = CursorPosition(1, 2)
    val buffer = Buffer
      .fromString(BufferId(1), "alpha\nbeta\ngamma\ndelta")
      .copy(cursors = List(cursor))
    val runner = CommandRunner.empty.activate(CommandRegistry.default, AppConfig.default)
    val state = AppState.initial.copy(
      config = AppConfig.default
        .withLineNumbers(true)
        .withGutter(true)
        .copy(textAreaInsets = TextAreaInsets(left = 0.05, right = 0.05))
        .withUiElementGap(1),
      buffers = Map(buffer.id -> buffer),
      bufferOrder = List(buffer.id),
      layout = AppState.initial.layout.copy(
        editorPanes = Map(PaneId(0) -> EditorPane.withBuffer(PaneId(0), buffer.id)),
        activeEditorPaneId = Some(PaneId(0)),
        paneOrder = List(PaneId(0))
      ),
      focus = Focus.Surface(SurfaceId("command-runner")),
      uiSurfaces = List(
        UiSurface(
          SurfaceId("left-panel"),
          SurfaceContent.Outline(Nil),
          SurfacePresentation.Pinned(PanelPosition.Left, 16)
        ),
        UiSurface(
          SurfaceId("command-runner"),
          SurfaceContent.CommandPalette(runner),
          SurfacePresentation.Floating(Some(cursor), SurfacePlacement.BelowCursor)
        )
      )
    )
    val calculatedLayout = LayoutEngine.calculateLayout(state, viewport)
    val contract         = EditorLayoutContract.from(state, viewport, calculatedLayout)

    contract.viewportRect shouldBe viewportRect
    contract.contentAreaRect.bottom shouldBe calculatedLayout.gutterRect.map(_.y).getOrElse(viewport.height)
    contract.workspace.paneLayouts shouldBe LayoutEngine.calculateEditorPaneLayouts(state, calculatedLayout)
    contract.violations shouldBe Nil
  }

  it should "report contract violations with the owning rectangle names" in {
    val badLayout = CalculatedLayout(
      editorPanelRect = LayoutRect(0, 0, viewport.width + 1, viewport.height),
      leftSpacerRect = LayoutRect(0, 0, 0, viewport.height),
      rightSpacerRect = LayoutRect(viewport.width, 0, 0, viewport.height),
      gutterRect = Some(LayoutRect(0, viewport.height - 1, viewport.width - 1, 1))
    )

    val violations = EditorLayoutContract.from(AppState.initial, viewport, badLayout).violations

    violations.map(violation => violation.ownerName -> violation.childName) should contain allOf (
      "content area" -> "editor panel",
      "viewport"     -> "gutter"
    )
  }
end LayoutContractSpec
