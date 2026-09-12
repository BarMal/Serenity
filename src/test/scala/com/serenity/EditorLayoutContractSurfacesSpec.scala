package com.serenity

import com.serenity.command.{CommandRegistry, CommandRunner}
import com.serenity.config.{AppConfig, TextAreaInsets}
import com.serenity.rope.Balance
import com.serenity.state.models.*
import com.serenity.ui.layout.*
import com.serenity.ui.renderer.{OverlayViewModel, PinnedPanelViewModel}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class EditorLayoutContractSurfacesSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private val viewport = ViewportSize(120, 36)

  private def viewportRect: LayoutRect =
    LayoutRect(0, 0, viewport.width, viewport.height)

  private def assertInside(owner: LayoutRect, child: LayoutRect, clue: String): Unit =
    withClue(clue) {
      owner.containsRect(child) shouldBe true
    }

  "EditorLayoutContract" should "expose reusable contract violations for editor, pane, panel, gutter, and overlay ownership" in {
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
          activeEditorPaneId = Some(PaneId(0))
        ),
        focus = Focus.Surface(SurfaceId("command-runner"))
      ),
      runtime = AppState.initial.runtime.copy(
        uiSurfaces = List(
          UiSurface(
            SurfaceId("left-panel"),
            SurfaceContent.Outline(Nil),
            SurfacePresentation.Docked
          ),
          UiSurface(
            SurfaceId("command-runner"),
            SurfaceContent.CommandPalette(runner),
            SurfacePresentation.Floating(Some(cursor), SurfacePlacement.BelowCursor)
          )
        )
      )
    )
    val dockedState      = DockedPanelFixtures.dockExisting(state, SurfaceId("left-panel"), PanelPosition.Left, 16)
    val calculatedLayout = LayoutEngine.calculateLayout(dockedState, viewport)
    val contract         = EditorLayoutContract.from(dockedState, viewport, calculatedLayout)

    contract.viewportRect shouldBe viewportRect
    contract.contentAreaRect.bottom shouldBe calculatedLayout.gutterRect.map(_.y).getOrElse(viewport.height)
    contract.workspace.paneLayouts shouldBe LayoutEngine.calculateEditorPaneLayouts(dockedState, calculatedLayout)
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
      SurfacePresentation.Docked
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
    val builtState = AppState.initial.copy(
      persisted = AppState.initial.persisted.copy(
        buffers = Map(buffer.id -> buffer),
        bufferOrder = List(buffer.id),
        layout = AppState.initial.persisted.layout.copy(
          editorPanes = Map(PaneId(0) -> EditorPane.withBuffer(PaneId(0), buffer.id)),
          activeEditorPaneId = Some(PaneId(0))
        ),
        focus = Focus.Surface(commandRunner.id)
      ),
      runtime = AppState.initial.runtime.copy(
        uiSurfaces = List(pinnedPanel, quickInfo, commandRunner)
      )
    )
    val state = DockedPanelFixtures.dockExisting(builtState, pinnedPanel.id, PanelPosition.Left, 16)

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
    val panelId = SurfaceId("left-panel")
    val state = DockedPanelFixtures.dock(AppState.initial, panelId, SurfaceContent.Outline(Nil), PanelPosition.Left, 16)
    val calculatedLayout = LayoutEngine.calculateLayout(state, viewport)
    val contract         = EditorLayoutContract.from(state, viewport, calculatedLayout)
    val titleRect        = contract.pinnedSurfaceTitleRects(panelId)
    val malformed = contract.copy(
      pinnedSurfaceContentRects = contract.pinnedSurfaceContentRects.updated(panelId, titleRect)
    )

    malformed.violations.map(violation => violation.ownerName -> violation.childName) should contain(
      s"pinned surface ${panelId.value} title" -> s"pinned surface ${panelId.value} content"
    )
  }

  it should "keep markdown preview rows inside the pinned panel content contract" in {
    val buffer    = Buffer.fromString(BufferId(1), "# Title\n\nFirst paragraph\n\nSecond paragraph")
    val previewId = SurfaceId("markdown-preview")
    val baseState = AppState.initial.copy(
      persisted = AppState.initial.persisted.copy(
        buffers = Map(buffer.id -> buffer),
        bufferOrder = List(buffer.id)
      )
    )
    val state = DockedPanelFixtures.dock(
      baseState,
      previewId,
      SurfaceContent.MarkdownPreview(buffer.id, "Notes"),
      PanelPosition.Right,
      30
    )
    val preview = state.surfaceById(previewId).getOrElse(fail("expected markdown preview surface"))

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
    val expandedId = SurfaceId("expanded-panel")
    val docked =
      DockedPanelFixtures.dock(AppState.initial, expandedId, SurfaceContent.Diagnostics(Nil), PanelPosition.Right, 24)
    val state         = DockedPanelFixtures.expand(docked, expandedId)
    val expandedPanel = state.surfaceById(expandedId).getOrElse(fail("expected expanded panel surface"))

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
    val expandedId = SurfaceId("expanded-panel")
    val withPinned =
      DockedPanelFixtures.dock(
        AppState.initial,
        SurfaceId("left-panel"),
        SurfaceContent.Outline(Nil),
        PanelPosition.Left,
        16
      )
    val withExpanded =
      DockedPanelFixtures.dock(withPinned, expandedId, SurfaceContent.Diagnostics(Nil), PanelPosition.Right, 24)
    val state         = DockedPanelFixtures.expand(withExpanded, expandedId)
    val pinnedPanel   = state.surfaceById(SurfaceId("left-panel")).getOrElse(fail("expected pinned surface"))
    val expandedPanel = state.surfaceById(expandedId).getOrElse(fail("expected expanded surface"))

    val calculatedLayout = LayoutEngine.calculateLayout(state, viewport)
    val contract         = EditorLayoutContract.from(state, viewport, calculatedLayout)
    val panelViews = PinnedPanelViewModel
      .fromState(state, calculatedLayout)
      .flatMap(view => view.surfaceId.map(_ -> view))
      .toMap

    EditorLayoutContract.panelRectFor(pinnedPanel, state, calculatedLayout) shouldBe Some(
      contract.pinnedSurfaceRects(pinnedPanel.id)
    )
    EditorLayoutContract.panelRectFor(expandedPanel, state, calculatedLayout) shouldBe Some(
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
          activeEditorPaneId = Some(PaneId(0))
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
          activeEditorPaneId = Some(paneId)
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
          activeEditorPaneId = Some(paneId)
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
      SurfacePresentation.Docked
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
    val builtState = AppState.initial.copy(
      persisted = AppState.initial.persisted.copy(
        buffers = Map(buffer.id -> buffer),
        bufferOrder = List(buffer.id),
        layout = AppState.initial.persisted.layout.copy(
          editorPanes = Map(PaneId(0) -> EditorPane.withBuffer(PaneId(0), buffer.id)),
          activeEditorPaneId = Some(PaneId(0))
        ),
        focus = Focus.Surface(commandRunner.id)
      ),
      runtime = AppState.initial.runtime.copy(uiSurfaces = List(pinnedPanel, quickInfo, commandRunner))
    )
    val state = DockedPanelFixtures.dockExisting(builtState, pinnedPanel.id, PanelPosition.Left, 18)

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
end EditorLayoutContractSurfacesSpec
