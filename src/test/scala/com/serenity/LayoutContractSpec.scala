package com.serenity

import com.serenity.command.{CommandRegistry, CommandRunner, FileIntent}
import com.serenity.config.{AppConfig, InterfaceDensity, TextAreaInsets}
import com.serenity.rope.Balance
import com.serenity.state.models.*
import com.serenity.ui.layout.*
import com.serenity.ui.renderer.{OverlayViewModel, PinnedPanelViewModel}
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
        persisted = AppState.initial.persisted.copy(
          config = AppConfig.default
            .withInterfaceDensity(density)
            .withLineNumbers(true)
            .withGutter(true)
            .withTextAreaInsets(TextAreaInsets(left = 0.05, right = 0.10))
            .withUiElementGap(2)
        ),
        runtime = AppState.initial.runtime.copy(
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
      .copy(editing = EditingState(cursors = List(CursorPosition(1, 2))))
    val runner = CommandRunner.empty.activate(CommandRegistry.default, AppConfig.default)
    val state = AppState.initial.copy(
      persisted = AppState.initial.persisted.copy(
        buffers = Map(buffer.id -> buffer),
        bufferOrder = List(buffer.id),
        layout = AppState.initial.persisted.layout.copy(
          editorPanes = Map(PaneId(0) -> EditorPane.withBuffer(PaneId(0), buffer.id)),
          activeEditorPaneId = Some(PaneId(0)),
          paneOrder = List(PaneId(0))
        ),
        focus = Focus.Surface(SurfaceId("command-runner"))
      ),
      runtime = AppState.initial.runtime.copy(
        uiSurfaces = List(
          UiSurface(
            SurfaceId("command-runner"),
            SurfaceContent.CommandPalette(runner),
            SurfacePresentation.Floating(Some(CursorPosition(1, 2)), SurfacePlacement.BelowCursor)
          )
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
      .copy(editing = EditingState(cursors = List(CursorPosition(1, 2))))
    val state = AppState.initial.copy(
      persisted = AppState.initial.persisted.copy(
        buffers = Map(buffer.id -> buffer),
        bufferOrder = List(buffer.id),
        layout = AppState.initial.persisted.layout.copy(
          editorPanes = Map(PaneId(0) -> EditorPane.withBuffer(PaneId(0), buffer.id)),
          activeEditorPaneId = Some(PaneId(0)),
          paneOrder = List(PaneId(0))
        ),
        focus = Focus.Surface(SurfaceId("quick-info"))
      ),
      runtime = AppState.initial.runtime.copy(
        uiSurfaces = List(
          UiSurface(
            SurfaceId("quick-info"),
            SurfaceContent.QuickInfo("List.map(f)"),
            SurfacePresentation.Floating(Some(CursorPosition(1, 2)), SurfacePlacement.AboveCursor)
          )
        )
      )
    )
    val layout      = LayoutEngine.calculateLayout(state, viewport)
    val contentRect = LayoutEngine.calculateEditorWorkspaceLayout(state, layout).activeContentRect(state).get
    val overlayRect = layout.aboveCursorOverlayRect.getOrElse(fail("expected above-cursor overlay"))

    assertInside(contentRect, overlayRect, s"above-cursor overlay $overlayRect")
  }

  // issue #1059: a settings group drilled into from either entry point renders on the one command-runner surface
  // now, so there is no second below-cursor surface to stack alongside it -- this now just confirms the one surface
  // still stays clamped inside a tiny active editor content rectangle.
  it should "keep the below-cursor command surface inside tiny active editor content rectangles" in {
    val tinyViewport = ViewportSize(32, 6)
    val cursor       = CursorPosition(1, 2)
    val buffer = Buffer
      .fromString(BufferId(1), "alpha\nbeta\ngamma\ndelta")
      .copy(editing = EditingState(cursors = List(cursor)))
    given CommandRegistry = CommandRegistry.default
    val runner = CommandRunner.empty
      .activate(CommandRegistry.default, AppConfig.default)
      .openSettings
      .enterSelectedGroup
    val state = AppState.initial.copy(
      persisted = AppState.initial.persisted.copy(
        config = AppConfig.default.withLineNumbers(false).withGutter(false).withTextAreaInsets(TextAreaInsets()),
        buffers = Map(buffer.id -> buffer),
        bufferOrder = List(buffer.id),
        layout = AppState.initial.persisted.layout.copy(
          editorPanes = Map(PaneId(0) -> EditorPane.withBuffer(PaneId(0), buffer.id)),
          activeEditorPaneId = Some(PaneId(0)),
          paneOrder = List(PaneId(0))
        ),
        focus = Focus.Surface(SurfaceId("command-runner"))
      ),
      runtime = AppState.initial.runtime.copy(
        uiSurfaces = List(
          UiSurface(
            SurfaceId("command-runner"),
            SurfaceContent.CommandPalette(runner),
            SurfacePresentation.Floating(Some(cursor), SurfacePlacement.BelowCursor)
          )
        )
      )
    )

    val layout      = LayoutEngine.calculateLayout(state, tinyViewport)
    val contentRect = LayoutEngine.calculateEditorWorkspaceLayout(state, layout).activeContentRect(state).get
    val stack       = layout.belowCursorOverlayStack

    stack.map(_._1) shouldBe List(SurfaceId("command-runner"))
    stack.foreach { case (surfaceId, rect) => assertInside(contentRect, rect, s"$surfaceId") }
  }

  it should "reserve configured gaps before clamping oversized pinned side panels" in {
    val constrainedViewport = ViewportSize(20, 8)
    val gap                 = 2
    val state = AppState.initial.copy(
      persisted = AppState.initial.persisted.copy(
        config = AppConfig.default
          .withLineNumbers(false)
          .withGutter(false)
          .withTextAreaInsets(TextAreaInsets(left = 0.0, right = 0.0))
          .withUiElementGap(gap)
      ),
      runtime = AppState.initial.runtime.copy(
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
      persisted = AppState.initial.persisted.copy(
        config = AppConfig.default
          .withLineNumbers(false)
          .withGutter(false)
          .withTextAreaInsets(TextAreaInsets(left = 0.0, right = 0.0, top = 0.0, bottom = 0.0))
          .withUiElementGap(gap)
      ),
      runtime = AppState.initial.runtime.copy(
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
      persisted = AppState.initial.persisted.copy(
        config = AppConfig.default.withLineNumbers(false).withGutter(false).withTextAreaInsets(TextAreaInsets())
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
      .copy(editing = EditingState(cursors = List(cursor)))
    val runner = CommandRunner.empty.activate(CommandRegistry.default, AppConfig.default)
    val state = AppState.initial.copy(
      persisted = AppState.initial.persisted.copy(
        config = AppConfig.default
          .withLineNumbers(true)
          .withGutter(true)
          .withTextAreaInsets(TextAreaInsets(left = 0.05, right = 0.05))
          .withUiElementGap(1),
        buffers = Map(buffer.id -> buffer),
        bufferOrder = List(buffer.id),
        layout = AppState.initial.persisted.layout.copy(
          editorPanes = Map(PaneId(0) -> EditorPane.withBuffer(PaneId(0), buffer.id)),
          activeEditorPaneId = Some(PaneId(0)),
          paneOrder = List(PaneId(0))
        ),
        focus = Focus.Surface(SurfaceId("command-runner"))
      ),
      runtime = AppState.initial.runtime.copy(
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
    )
    val calculatedLayout = LayoutEngine.calculateLayout(state, viewport)
    val contract         = EditorLayoutContract.from(state, viewport, calculatedLayout)

    contract.viewportRect shouldBe viewportRect
    contract.contentAreaRect.bottom shouldBe calculatedLayout.gutterRect.map(_.y).getOrElse(viewport.height)
    contract.workspace.paneLayouts shouldBe LayoutEngine.calculateEditorPaneLayouts(state, calculatedLayout)
    contract.pinnedSurfaceTitleRects.keySet shouldBe contract.pinnedSurfaceRects.keySet
    contract.pinnedSurfaceContentRects.keySet shouldBe contract.pinnedSurfaceRects.keySet
    contract.floatingOverlayContentRects.map(_._1) shouldBe contract.floatingOverlayRects.map(_._1)
    contract.belowCursorOverlayRects.map(_._1) shouldBe List(SurfaceId("command-runner"))
    contract.violations shouldBe Nil
  }

  it should "expose frame and content rectangles for pinned surfaces and floating overlays" in {
    val cursor = CursorPosition(1, 2)
    val buffer = Buffer
      .fromString(BufferId(1), "alpha\nbeta\ngamma\ndelta")
      .copy(editing = EditingState(cursors = List(cursor)))
    val runner = CommandRunner.empty.activate(CommandRegistry.default, AppConfig.default)
    val pinnedPanel = UiSurface(
      SurfaceId("left-panel"),
      SurfaceContent.Outline(Nil),
      SurfacePresentation.Pinned(PanelPosition.Left, 16)
    )
    val quickInfo = UiSurface(
      SurfaceId("quick-info"),
      SurfaceContent.QuickInfo("List.map(f)"),
      SurfacePresentation.Floating(Some(cursor), SurfacePlacement.AboveCursor)
    )
    val commandRunner = UiSurface(
      SurfaceId("command-runner"),
      SurfaceContent.CommandPalette(runner),
      SurfacePresentation.Floating(Some(cursor), SurfacePlacement.BelowCursor)
    )
    val state = AppState.initial.copy(
      persisted = AppState.initial.persisted.copy(
        buffers = Map(buffer.id -> buffer),
        bufferOrder = List(buffer.id),
        layout = AppState.initial.persisted.layout.copy(
          editorPanes = Map(PaneId(0) -> EditorPane.withBuffer(PaneId(0), buffer.id)),
          activeEditorPaneId = Some(PaneId(0)),
          paneOrder = List(PaneId(0))
        ),
        focus = Focus.Surface(commandRunner.id)
      ),
      runtime = AppState.initial.runtime.copy(
        uiSurfaces = List(pinnedPanel, quickInfo, commandRunner)
      )
    )

    val calculatedLayout = LayoutEngine.calculateLayout(state, viewport)
    val contract         = EditorLayoutContract.from(state, viewport, calculatedLayout)
    val pinnedView = PinnedPanelViewModel
      .fromState(state, calculatedLayout)
      .find(_.surfaceId.contains(pinnedPanel.id))
      .getOrElse(fail("expected pinned panel view"))
    val overlayViews = OverlayViewModel.fromState(state, calculatedLayout)
    val overlaysById = (overlayViews.aboveCursor.toList ++ overlayViews.belowCursorStack)
      .flatMap(view => view.surfaceId.map(_ -> view))
      .toMap

    val pinnedFrame = contract.pinnedSurfaceRects(pinnedPanel.id)
    contract.pinnedSurfaceTitleRects(pinnedPanel.id) shouldBe pinnedView.titleRect
    contract.pinnedSurfaceContentRects(pinnedPanel.id) shouldBe pinnedView.resolvedContentRect

    val overlayFrames   = contract.floatingOverlayRects.toMap
    val overlayContents = contract.floatingOverlayContentRects.toMap

    contract.aboveCursorOverlayRects.map(_._1) shouldBe List(quickInfo.id)
    contract.belowCursorOverlayRects.map(_._1) shouldBe List(commandRunner.id)

    overlayContents(quickInfo.id) shouldBe overlaysById(quickInfo.id).resolvedContentRect
    overlayContents(commandRunner.id) shouldBe overlaysById(commandRunner.id).resolvedContentRect

    val activeContent = contract.workspace.activeContentRect(state).getOrElse(fail("expected active content rect"))
    assertInside(activeContent, overlayContents(quickInfo.id), "quick info overlay content")
    assertInside(activeContent, overlayContents(commandRunner.id), "command runner overlay content")
    assertInside(pinnedFrame, contract.pinnedSurfaceTitleRects(pinnedPanel.id), "pinned panel title")
    assertInside(contract.contentAreaRect, contract.pinnedSurfaceContentRects(pinnedPanel.id), "pinned panel content")
  }

  it should "report overlapping pinned surface title and content rectangles" in {
    val panel = UiSurface(
      SurfaceId("left-panel"),
      SurfaceContent.Outline(Nil),
      SurfacePresentation.Pinned(PanelPosition.Left, 16)
    )
    val state            = AppState.initial.copy(runtime = AppState.initial.runtime.copy(uiSurfaces = List(panel)))
    val calculatedLayout = LayoutEngine.calculateLayout(state, viewport)
    val contract         = EditorLayoutContract.from(state, viewport, calculatedLayout)
    val titleRect        = contract.pinnedSurfaceTitleRects(panel.id)
    val malformed = contract.copy(
      pinnedSurfaceContentRects = contract.pinnedSurfaceContentRects.updated(panel.id, titleRect)
    )

    malformed.violations.map(violation => violation.ownerName -> violation.childName) should contain(
      s"pinned surface ${panel.id.value} title" -> s"pinned surface ${panel.id.value} content"
    )
  }

  it should "keep markdown preview rows inside the pinned panel content contract" in {
    val buffer = Buffer.fromString(BufferId(1), "# Title\n\nFirst paragraph\n\nSecond paragraph")
    val preview = UiSurface(
      SurfaceId("markdown-preview"),
      SurfaceContent.MarkdownPreview(buffer.id, "Notes"),
      SurfacePresentation.Pinned(PanelPosition.Right, 30)
    )
    val state = AppState.initial.copy(
      persisted = AppState.initial.persisted.copy(
        buffers = Map(buffer.id -> buffer),
        bufferOrder = List(buffer.id)
      ),
      runtime = AppState.initial.runtime.copy(uiSurfaces = List(preview))
    )

    val calculatedLayout = LayoutEngine.calculateLayout(state, viewport)
    val contract         = EditorLayoutContract.from(state, viewport, calculatedLayout)
    val previewView = PinnedPanelViewModel
      .fromState(state, calculatedLayout)
      .find(_.surfaceId.contains(preview.id))
      .getOrElse(fail("expected markdown preview panel"))

    contract.pinnedSurfaceTitleRects(preview.id) shouldBe previewView.titleRect
    contract.pinnedSurfaceContentRects(preview.id) shouldBe previewView.resolvedContentRect
    previewView.rows.map(_.plainText) should not be empty
    contract.pinnedSurfaceRowSlots(preview.id) shouldBe previewView.contentRowSlots
    previewView.contentRowSlots.foreach(slot =>
      assertInside(
        previewView.resolvedContentRect,
        LayoutRect(previewView.resolvedContentRect.x, slot.y, 1, 1),
        "markdown preview row"
      )
    )
    contract.violations shouldBe Nil
  }

  it should "expose frame, title, and content rectangles for expanded surfaces" in {
    val expandedPanel = UiSurface(
      SurfaceId("expanded-panel"),
      SurfaceContent.Diagnostics(Nil),
      SurfacePresentation.Expanded(PanelPosition.Right, 24)
    )
    val state = AppState.initial.copy(
      runtime = AppState.initial.runtime.copy(uiSurfaces = List(expandedPanel))
    )

    val calculatedLayout = LayoutEngine.calculateLayout(state, viewport)
    val contract         = EditorLayoutContract.from(state, viewport, calculatedLayout)
    val expandedView = PinnedPanelViewModel
      .fromState(state, calculatedLayout)
      .find(_.surfaceId.contains(expandedPanel.id))
      .getOrElse(fail("expected expanded panel view"))

    contract.expandedSurfaceRects(expandedPanel.id) shouldBe
      calculatedLayout.expandedPanelRect.getOrElse(fail("expected expanded panel rect"))
    contract.expandedSurfaceTitleRects(expandedPanel.id) shouldBe expandedView.titleRect
    contract.expandedSurfaceContentRects(expandedPanel.id) shouldBe expandedView.resolvedContentRect
    contract.expandedSurfaceRowSlots(expandedPanel.id) shouldBe expandedView.contentRowSlots

    val expandedFrame = contract.expandedSurfaceRects(expandedPanel.id)
    assertInside(contract.contentAreaRect, expandedFrame, "expanded panel frame")
    assertInside(expandedFrame, contract.expandedSurfaceTitleRects(expandedPanel.id), "expanded panel title")
    assertInside(expandedFrame, contract.expandedSurfaceContentRects(expandedPanel.id), "expanded panel content")
    contract
      .expandedSurfaceRowSlots(expandedPanel.id)
      .foreach(slot =>
        withClue(s"expanded ${expandedPanel.id} slot $slot") {
          contract
            .expandedSurfaceContentRects(expandedPanel.id)
            .contains(
              contract.expandedSurfaceContentRects(expandedPanel.id).x,
              slot.y
            )
            .shouldBe(true)
        }
      )
    contract.violations shouldBe Nil
  }

  it should "provide shared panel lookups for pinned and expanded surfaces" in {
    val pinnedPanel = UiSurface(
      SurfaceId("left-panel"),
      SurfaceContent.Outline(Nil),
      SurfacePresentation.Pinned(PanelPosition.Left, 16)
    )
    val expandedPanel = UiSurface(
      SurfaceId("expanded-panel"),
      SurfaceContent.Diagnostics(Nil),
      SurfacePresentation.Expanded(PanelPosition.Right, 24)
    )
    val state = AppState.initial.copy(
      runtime = AppState.initial.runtime.copy(uiSurfaces = List(pinnedPanel, expandedPanel))
    )

    val calculatedLayout = LayoutEngine.calculateLayout(state, viewport)
    val contract         = EditorLayoutContract.from(state, viewport, calculatedLayout)
    val panelViews = PinnedPanelViewModel
      .fromState(state, calculatedLayout)
      .flatMap(view => view.surfaceId.map(_ -> view))
      .toMap

    EditorLayoutContract.panelRectFor(pinnedPanel, calculatedLayout) shouldBe Some(
      contract.pinnedSurfaceRects(pinnedPanel.id)
    )
    EditorLayoutContract.panelRectFor(expandedPanel, calculatedLayout) shouldBe Some(
      contract.expandedSurfaceRects(expandedPanel.id)
    )
    contract.panelRect(pinnedPanel.id) shouldBe Some(contract.pinnedSurfaceRects(pinnedPanel.id))
    contract.panelTitleRect(pinnedPanel.id) shouldBe Some(contract.pinnedSurfaceTitleRects(pinnedPanel.id))
    contract.panelContentRect(pinnedPanel.id) shouldBe Some(panelViews(pinnedPanel.id).resolvedContentRect)
    contract.panelRowSlots(pinnedPanel.id) shouldBe panelViews(pinnedPanel.id).contentRowSlots

    contract.panelRect(expandedPanel.id) shouldBe Some(contract.expandedSurfaceRects(expandedPanel.id))
    contract.panelTitleRect(expandedPanel.id) shouldBe Some(contract.expandedSurfaceTitleRects(expandedPanel.id))
    contract.panelContentRect(expandedPanel.id) shouldBe Some(panelViews(expandedPanel.id).resolvedContentRect)
    contract.panelRowSlots(expandedPanel.id) shouldBe panelViews(expandedPanel.id).contentRowSlots

    contract.panelRect(SurfaceId("missing")) shouldBe None
    contract.panelTitleRect(SurfaceId("missing")) shouldBe None
    contract.panelContentRect(SurfaceId("missing")) shouldBe None
    contract.panelRowSlots(SurfaceId("missing")) shouldBe Nil
  }

  it should "provide shared overlay lookups for floating surfaces" in {
    val cursor = CursorPosition(1, 2)
    val buffer = Buffer
      .fromString(BufferId(1), "alpha\nbeta\ngamma\ndelta")
      .copy(editing = EditingState(cursors = List(cursor)))
    val runner = CommandRunner.empty.activate(CommandRegistry.default, AppConfig.default)
    val quickInfo = UiSurface(
      SurfaceId("quick-info"),
      SurfaceContent.QuickInfo("List.map(f)"),
      SurfacePresentation.Floating(Some(cursor), SurfacePlacement.AboveCursor)
    )
    val commandRunner = UiSurface(
      SurfaceId("command-runner"),
      SurfaceContent.CommandPalette(runner),
      SurfacePresentation.Floating(Some(cursor), SurfacePlacement.BelowCursor)
    )
    val state = AppState.initial.copy(
      persisted = AppState.initial.persisted.copy(
        buffers = Map(buffer.id -> buffer),
        bufferOrder = List(buffer.id),
        layout = AppState.initial.persisted.layout.copy(
          editorPanes = Map(PaneId(0) -> EditorPane.withBuffer(PaneId(0), buffer.id)),
          activeEditorPaneId = Some(PaneId(0)),
          paneOrder = List(PaneId(0))
        ),
        focus = Focus.Surface(commandRunner.id)
      ),
      runtime = AppState.initial.runtime.copy(uiSurfaces = List(quickInfo, commandRunner))
    )

    val calculatedLayout = LayoutEngine.calculateLayout(state, viewport)
    val contract         = EditorLayoutContract.from(state, viewport, calculatedLayout)
    val overlayViews     = OverlayViewModel.fromState(state, calculatedLayout)
    val overlaysById = (overlayViews.aboveCursor.toList ++ overlayViews.belowCursorStack)
      .flatMap(view => view.surfaceId.map(_ -> view))
      .toMap
    val overlayRects = contract.floatingOverlayRects.toMap

    EditorLayoutContract.overlayRectFor(quickInfo.id, calculatedLayout) shouldBe Some(overlayRects(quickInfo.id))
    EditorLayoutContract.overlayRectFor(commandRunner.id, calculatedLayout) shouldBe Some(
      overlayRects(commandRunner.id)
    )
    contract.overlayRect(quickInfo.id) shouldBe Some(overlayRects(quickInfo.id))
    contract.overlayContentRect(quickInfo.id) shouldBe Some(overlaysById(quickInfo.id).resolvedContentRect)
    contract.overlayRowSlots(quickInfo.id) shouldBe overlaysById(quickInfo.id).contentRowSlots

    contract.overlayRect(commandRunner.id) shouldBe Some(overlayRects(commandRunner.id))
    contract.overlayContentRect(commandRunner.id) shouldBe Some(overlaysById(commandRunner.id).resolvedContentRect)
    contract.overlayHeaderRect(commandRunner.id) shouldBe Some(
      LayoutRect(
        overlaysById(commandRunner.id).resolvedContentRect.x,
        overlaysById(commandRunner.id).resolvedContentRect.y,
        overlaysById(commandRunner.id).resolvedContentRect.width,
        1
      )
    )
    contract.overlayRowSlots(commandRunner.id) shouldBe overlaysById(commandRunner.id).contentRowSlots
    assertInside(
      contract.overlayContentRect(commandRunner.id).getOrElse(fail("expected command runner content")),
      contract.overlayHeaderRect(commandRunner.id).getOrElse(fail("expected command runner header")),
      "command runner overlay header"
    )

    contract.overlayRect(SurfaceId("missing")) shouldBe None
    contract.overlayContentRect(SurfaceId("missing")) shouldBe None
    contract.overlayHeaderRect(SurfaceId("missing")) shouldBe None
    contract.overlayRowSlots(SurfaceId("missing")) shouldBe Nil

    val malformed = contract.copy(
      floatingOverlayHeaderRects = contract.floatingOverlayHeaderRects.updated(
        commandRunner.id,
        LayoutRect(0, contract.overlayContentRect(commandRunner.id).getOrElse(fail("expected content")).bottom, 1, 1)
      )
    )

    malformed.violations.map(violation => violation.ownerName -> violation.childName) should contain(
      s"floating overlay ${commandRunner.id.value} content" -> s"floating overlay ${commandRunner.id.value} header"
    )
  }

  it should "provide shared pane header, title, and gutter lookups" in {
    val buffer = Buffer.fromString(BufferId(1), "alpha\nbeta")
    val paneId = PaneId(0)
    val state = AppState.initial.copy(
      persisted = AppState.initial.persisted.copy(
        config = AppConfig.default.withLineNumbers(true).withGutter(true),
        buffers = Map(buffer.id -> buffer),
        bufferOrder = List(buffer.id),
        layout = AppState.initial.persisted.layout.copy(
          editorPanes = Map(paneId -> EditorPane.withBuffer(paneId, buffer.id)),
          activeEditorPaneId = Some(paneId),
          paneOrder = List(paneId)
        )
      )
    )

    val calculatedLayout = LayoutEngine.calculateLayout(state, viewport)
    val contract         = EditorLayoutContract.from(state, viewport, calculatedLayout)
    val activePane = contract.workspace
      .activePaneLayout(state)
      .getOrElse(fail("expected active pane layout"))

    contract.paneLayout(paneId) shouldBe Some(activePane)
    contract.paneHeaderRect(paneId) shouldBe Some(activePane.headerRect)
    contract.paneTitleRect(paneId) shouldBe Some(activePane.titleRect)
    contract.activePaneLayout shouldBe Some(activePane)
    contract.activePaneHeaderRect shouldBe Some(activePane.headerRect)
    contract.activePaneTitleRect shouldBe Some(activePane.titleRect)
    contract.gutterRect shouldBe calculatedLayout.gutterRect

    contract.paneLayout(PaneId(99)) shouldBe None
    contract.paneHeaderRect(PaneId(99)) shouldBe None
    contract.paneTitleRect(PaneId(99)) shouldBe None
  }

  it should "provide shared spacer lookups" in {
    val state = AppState.initial.copy(
      persisted = AppState.initial.persisted.copy(
        config = AppConfig.default
          .withLineNumbers(false)
          .withGutter(false)
          .withTextAreaInsets(TextAreaInsets(left = 0.10, right = 0.15))
      )
    )

    val calculatedLayout = LayoutEngine.calculateLayout(state, viewport)
    val contract         = EditorLayoutContract.from(state, viewport, calculatedLayout)

    contract.leftSpacerRect shouldBe calculatedLayout.leftSpacerRect
    contract.rightSpacerRect shouldBe calculatedLayout.rightSpacerRect
    contract.topSpacerRect shouldBe calculatedLayout.topSpacerRect
    contract.bottomSpacerRect shouldBe calculatedLayout.bottomSpacerRect
    contract.workspace.editorPanelRect shouldBe calculatedLayout.editorPanelRect
  }

  it should "provide shared line-number lookups" in {
    val buffer = Buffer
      .fromString(BufferId(1), "alpha\nbeta\ngamma")
      .copy(viewport = Viewport(topLine = 0, leftColumn = 0, visibleLines = 8, visibleColumns = 40))
    val paneId = PaneId(0)
    val state = AppState.initial.copy(
      persisted = AppState.initial.persisted.copy(
        config = AppConfig.default.withLineNumbers(true),
        buffers = Map(buffer.id -> buffer),
        bufferOrder = List(buffer.id),
        layout = AppState.initial.persisted.layout.copy(
          editorPanes = Map(paneId -> EditorPane.withBuffer(paneId, buffer.id)),
          activeEditorPaneId = Some(paneId),
          paneOrder = List(paneId)
        )
      )
    )

    val calculatedLayout = LayoutEngine.calculateLayout(state, viewport)
    val contract         = EditorLayoutContract.from(state, viewport, calculatedLayout)

    contract.lineNumberRect shouldBe contract.workspace.lineNumberRect
    contract.lineNumberRect shouldBe calculatedLayout.lineNumberRect
    contract.lineNumberRowSlots(itemCount = 3) shouldBe contract.workspace.lineNumberRowSlots(itemCount = 3)
  }

  it should "expose pinned and floating row slots from the shared frame contract" in {
    val cursor = CursorPosition(1, 2)
    val buffer = Buffer
      .fromString(BufferId(1), "alpha\nbeta\ngamma\ndelta")
      .copy(editing = EditingState(cursors = List(cursor)))
    val runner = CommandRunner.empty.activate(CommandRegistry.default, AppConfig.default)
    val pinnedPanel = UiSurface(
      SurfaceId("find-panel"),
      SurfaceContent.ModalWorkflow(Modal.Find("needle", List(FindResult(2, 4)), 0)),
      SurfacePresentation.Pinned(PanelPosition.Left, 18)
    )
    val quickInfo = UiSurface(
      SurfaceId("quick-info"),
      SurfaceContent.QuickInfo("List.map(f)"),
      SurfacePresentation.Floating(Some(cursor), SurfacePlacement.AboveCursor)
    )
    val commandRunner = UiSurface(
      SurfaceId("command-runner"),
      SurfaceContent.CommandPalette(runner),
      SurfacePresentation.Floating(Some(cursor), SurfacePlacement.BelowCursor)
    )
    val state = AppState.initial.copy(
      persisted = AppState.initial.persisted.copy(
        buffers = Map(buffer.id -> buffer),
        bufferOrder = List(buffer.id),
        layout = AppState.initial.persisted.layout.copy(
          editorPanes = Map(PaneId(0) -> EditorPane.withBuffer(PaneId(0), buffer.id)),
          activeEditorPaneId = Some(PaneId(0)),
          paneOrder = List(PaneId(0))
        ),
        focus = Focus.Surface(commandRunner.id)
      ),
      runtime = AppState.initial.runtime.copy(uiSurfaces = List(pinnedPanel, quickInfo, commandRunner))
    )

    val calculatedLayout = LayoutEngine.calculateLayout(state, viewport)
    val contract         = EditorLayoutContract.from(state, viewport, calculatedLayout)
    val pinnedView = PinnedPanelViewModel
      .fromState(state, calculatedLayout)
      .find(_.surfaceId.contains(pinnedPanel.id))
      .getOrElse(fail("expected pinned panel view"))
    val overlayViews = OverlayViewModel.fromState(state, calculatedLayout)
    val overlaysById = (overlayViews.aboveCursor.toList ++ overlayViews.belowCursorStack)
      .flatMap(view => view.surfaceId.map(_ -> view))
      .toMap

    contract.pinnedSurfaceRowSlots(pinnedPanel.id).shouldBe(pinnedView.contentRowSlots)
    contract.floatingOverlayRowSlots(quickInfo.id).shouldBe(overlaysById(quickInfo.id).contentRowSlots)
    contract.floatingOverlayRowSlots(commandRunner.id).shouldBe(overlaysById(commandRunner.id).contentRowSlots)

    contract.pinnedSurfaceRowSlots.foreach {
      case (surfaceId, slots) =>
        val contentRect = contract.pinnedSurfaceContentRects(surfaceId)
        slots.foreach(slot =>
          withClue(s"pinned $surfaceId slot $slot")(contentRect.contains(contentRect.x, slot.y).shouldBe(true))
        )
    }
    val overlayContentRects = contract.floatingOverlayContentRects.toMap
    contract.floatingOverlayRowSlots.foreach {
      case (surfaceId, slots) =>
        val contentRect = overlayContentRects(surfaceId)
        slots.foreach(slot =>
          withClue(s"overlay $surfaceId slot $slot")(contentRect.contains(contentRect.x, slot.y).shouldBe(true))
        )
    }
    contract.violations shouldBe Nil
  }

  it should "enforce configured minimum gaps between stacked below-cursor overlays" in {
    val commands =
      List(
        com.serenity.command.Command
          .typed("open", "Open file", com.serenity.command.CommandIntent.File(FileIntent.OpenFile))
      )
    val registry = CommandRegistry(commands)
    val cursor   = CursorPosition(1, 2)
    val buffer = Buffer
      .fromString(BufferId(1), "alpha\nbeta\ngamma\ndelta")
      .copy(editing = EditingState(cursors = List(cursor)))
    val runner = CommandRunner.empty
      .activate(registry, AppConfig.default)
      .updateSearchTerm("op")(using registry)
    val toolbar = UiSurface(
      SurfaceId("contextual-toolbar"),
      SurfaceContent.ContextualToolbar(ContextualToolbarState()),
      SurfacePresentation.Floating(Some(cursor), SurfacePlacement.BelowCursor)
    )
    val commandRunner = UiSurface(
      SurfaceId("command-runner"),
      SurfaceContent.CommandPalette(runner),
      SurfacePresentation.Floating(Some(cursor), SurfacePlacement.BelowCursor)
    )
    val state = AppState.initial.copy(
      persisted = AppState.initial.persisted.copy(
        config = AppConfig.default.withUiElementGap(2),
        buffers = Map(buffer.id -> buffer),
        bufferOrder = List(buffer.id),
        layout = AppState.initial.persisted.layout.copy(
          editorPanes = Map(PaneId(0) -> EditorPane.withBuffer(PaneId(0), buffer.id)),
          activeEditorPaneId = Some(PaneId(0)),
          paneOrder = List(PaneId(0))
        ),
        focus = Focus.Surface(commandRunner.id)
      ),
      runtime = AppState.initial.runtime.copy(uiSurfaces = List(toolbar, commandRunner))
    )

    val calculatedLayout = LayoutEngine.calculateLayout(state, viewport)
    val contract         = EditorLayoutContract.from(state, viewport, calculatedLayout)
    val stack            = contract.belowCursorOverlayRects

    contract.minimumFloatingOverlayGapRows shouldBe 2
    stack.map(_._1) shouldBe List(toolbar.id, commandRunner.id)
    stack(1)._2.y should be >= stack.head._2.bottom + contract.minimumFloatingOverlayGapRows
    contract.violations shouldBe Nil
  }

  it should "report contract violations with the owning rectangle names" in {
    val badLayout = CalculatedLayout(
      editorPanelRect = LayoutRect(0, 0, viewport.width + 1, viewport.height),
      leftSpacerRect = LayoutRect(-1, 0, 1, viewport.height),
      rightSpacerRect = LayoutRect(viewport.width, 0, 0, viewport.height),
      gutterRect = Some(LayoutRect(0, viewport.height - 1, viewport.width - 1, 1))
    )

    val violations = EditorLayoutContract.from(AppState.initial, viewport, badLayout).violations

    violations.map(violation => violation.ownerName -> violation.childName) should contain allOf (
      "content area" -> "editor panel",
      "content area" -> "left spacer",
      "viewport"     -> "gutter"
    )
  }

  it should "report inactive panes outside the editor panel" in {
    val firstPane  = EditorPane.empty(PaneId(0))
    val secondPane = EditorPane.empty(PaneId(1))
    val state = AppState.initial.copy(
      persisted = AppState.initial.persisted.copy(
        layout = Layout(
          editorPanes = Map(PaneId(0) -> firstPane, PaneId(1) -> secondPane),
          activeEditorPaneId = Some(PaneId(0)),
          paneOrder = List(PaneId(0), PaneId(1)),
          splitDirection = PaneSplitDirection.Vertical
        )
      )
    )
    val layout   = LayoutEngine.calculateLayout(state, viewport)
    val contract = EditorLayoutContract.from(state, viewport, layout)
    val badSecondPane = contract
      .paneLayout(PaneId(1))
      .getOrElse(fail("expected inactive pane layout"))
      .copy(paneRect = LayoutRect(viewport.width, 0, 1, 1))
    val badContract = contract.copy(
      workspace = contract.workspace.copy(
        paneLayouts = contract.workspace.paneLayouts.updated(PaneId(1), badSecondPane)
      )
    )

    badContract.violations.map(violation => violation.ownerName -> violation.childName) should contain(
      "editor panel" -> "pane 1"
    )
  }

  it should "report stacked below-cursor overlay gap violations with overlay names" in {
    val cursor = CursorPosition(1, 2)
    val buffer = Buffer
      .fromString(BufferId(1), "alpha\nbeta\ngamma\ndelta")
      .copy(editing = EditingState(cursors = List(cursor)))
    val runner = CommandRunner.empty.activate(CommandRegistry.default, AppConfig.default)
    val toolbar = UiSurface(
      SurfaceId("contextual-toolbar"),
      SurfaceContent.ContextualToolbar(ContextualToolbarState()),
      SurfacePresentation.Floating(Some(cursor), SurfacePlacement.BelowCursor)
    )
    val commandRunner = UiSurface(
      SurfaceId("command-runner"),
      SurfaceContent.CommandPalette(runner),
      SurfacePresentation.Floating(Some(cursor), SurfacePlacement.BelowCursor)
    )
    val state = AppState.initial.copy(
      persisted = AppState.initial.persisted.copy(
        config = AppConfig.default.withUiElementGap(2),
        buffers = Map(buffer.id -> buffer),
        bufferOrder = List(buffer.id),
        layout = AppState.initial.persisted.layout.copy(
          editorPanes = Map(PaneId(0) -> EditorPane.withBuffer(PaneId(0), buffer.id)),
          activeEditorPaneId = Some(PaneId(0)),
          paneOrder = List(PaneId(0))
        ),
        focus = Focus.Surface(commandRunner.id)
      ),
      runtime = AppState.initial.runtime.copy(uiSurfaces = List(toolbar, commandRunner))
    )
    val calculatedLayout = LayoutEngine.calculateLayout(state, viewport)
    val toolbarRect      = calculatedLayout.belowCursorOverlayStack.head._2
    val overlappingRunnerRect = calculatedLayout
      .belowCursorOverlayStack(1)
      ._2
      .copy(
        y = toolbarRect.bottom + 1
      )
    val badLayout = calculatedLayout.copy(
      belowCursorOverlayRect = Some(toolbarRect),
      belowCursorOverlayStack = List(toolbar.id -> toolbarRect, commandRunner.id -> overlappingRunnerRect)
    )

    val violations = EditorLayoutContract.from(state, viewport, badLayout).violations

    violations.map(violation => violation.ownerName -> violation.childName) should contain(
      "floating overlay gap after contextual-toolbar" -> "floating overlay command-runner frame"
    )
  }
end LayoutContractSpec
