package com.serenity

import com.serenity.command.*
import com.serenity.config.AppConfig
import com.serenity.rope.Balance
import com.serenity.state.models.*
import com.serenity.ui.layout.*
import com.serenity.ui.renderer.OverlayViewModel
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class OverlayViewModelSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private val paneId   = PaneId(0)
  private val bufferId = BufferId(1)

  private def stateWithQuickInfo(text: String): AppState =
    val buffer = Buffer
      .fromString(bufferId, "one\ntwo\nthree")
      .copy(
        cursors = List(CursorPosition(1, 2))
      )
    val pane = EditorPane.withBuffer(paneId, bufferId)

    AppState.initial.copy(
      buffers = Map(bufferId -> buffer),
      bufferOrder = List(bufferId),
      layout = Layout(
        editorPanes = Map(paneId -> pane),
        activeEditorPaneId = Some(paneId)
      ),
      focus = Focus.Surface(SurfaceId("peek")),
      uiSurfaces = List(
        UiSurface(
          SurfaceId("peek"),
          SurfaceContent.QuickInfo(text),
          SurfacePresentation.Floating(Some(CursorPosition(1, 2)), SurfacePlacement.AboveCursor),
          dismissOnMove = true
        )
      )
    )

  "OverlayViewModel.fromState" should "derive an above-cursor quick-info overlay view from peek state" in {
    val state  = stateWithQuickInfo("List.map(f)")
    val layout = LayoutEngine.calculateLayout(state, ViewportSize(100, 24))

    val overlays = OverlayViewModel.fromState(state, layout)

    overlays.aboveCursor shouldBe defined
    overlays.belowCursor shouldBe None

    val overlay = overlays.aboveCursor.get
    overlay.rows.map(_.plainText) shouldBe List("List.map(f)")
    overlay.rect shouldBe layout.aboveCursorOverlayRect.get
    overlay.contentRect shouldBe Some(
      SurfaceFrameLayout.forContent(overlay.rect, state.uiSurfaces.head.content).contentRect
    )
  }

  it should "derive a below-cursor modal overlay view from unified floating surfaces" in {
    val buffer = Buffer
      .fromString(bufferId, "one\ntwo\nthree")
      .copy(
        cursors = List(CursorPosition(1, 2))
      )
    val pane = EditorPane.withBuffer(paneId, bufferId)
    val state = AppState.initial.copy(
      buffers = Map(bufferId -> buffer),
      bufferOrder = List(bufferId),
      layout = Layout(
        editorPanes = Map(paneId -> pane),
        activeEditorPaneId = Some(paneId)
      ),
      focus = Focus.Surface(SurfaceId("modal")),
      uiSurfaces = List(
        UiSurface(
          SurfaceId("modal"),
          SurfaceContent.ModalWorkflow(Modal.Custom("replace", "needle")),
          SurfacePresentation.Floating(Some(CursorPosition(1, 2)), SurfacePlacement.BelowCursor)
        )
      )
    )
    val layout = LayoutEngine.calculateLayout(state, ViewportSize(100, 24))

    val overlays = OverlayViewModel.fromState(state, layout)

    overlays.belowCursor shouldBe defined

    val overlay = overlays.belowCursor.get
    overlay.rows.map(_.plainText) shouldBe List("replace", "needle")
    overlay.rect shouldBe layout.belowCursorOverlayRect.get
  }

  it should "space command palette item slots without spacing other overlay content" in {
    val buffer = Buffer.fromString(bufferId, "one\ntwo\nthree").copy(cursors = List(CursorPosition(1, 2)))
    val pane   = EditorPane.withBuffer(paneId, bufferId)
    val runner = CommandRunner.empty.activate(CommandRegistry.default, AppConfig.default)
    val state = AppState.initial.copy(
      config = AppConfig.default.withCommandRunnerItemGapRows(1),
      buffers = Map(bufferId -> buffer),
      bufferOrder = List(bufferId),
      layout = Layout(editorPanes = Map(paneId -> pane), activeEditorPaneId = Some(paneId)),
      focus = Focus.Surface(SurfaceId("command-runner")),
      uiSurfaces = List(
        UiSurface(
          SurfaceId("command-runner"),
          SurfaceContent.CommandPalette(runner),
          SurfacePresentation.Floating(Some(CursorPosition(1, 2)), SurfacePlacement.BelowCursor)
        )
      )
    )
    val layout  = LayoutEngine.calculateLayout(state, ViewportSize(100, 24))
    val overlay = OverlayViewModel.fromState(state, layout).belowCursor.getOrElse(fail("Expected command overlay"))

    overlay.itemGapRows shouldBe 1
    overlay.contentRowSlots
      .collect { case SurfaceContentRowSlot(SurfaceContentRowKind.Item(_), y) => y }
      .sliding(2)
      .foreach {
        case List(first, second) => second - first shouldBe 2
        case _                   => ()
      }
  }

  it should "derive a focused find overlay view beneath the active cursor" in {
    val buffer = Buffer
      .fromString(bufferId, "one\ntwo\nthree")
      .copy(
        cursors = List(CursorPosition(1, 2))
      )
    val pane = EditorPane.withBuffer(paneId, bufferId)
    val state = AppState.initial.copy(
      buffers = Map(bufferId -> buffer),
      bufferOrder = List(bufferId),
      layout = Layout(
        editorPanes = Map(paneId -> pane),
        activeEditorPaneId = Some(paneId)
      ),
      focus = Focus.Surface(SurfaceId("find")),
      uiSurfaces = List(
        UiSurface(
          SurfaceId("find"),
          SurfaceContent.ModalWorkflow(Modal.Find("two", List(FindResult(1, 0)), 0)),
          SurfacePresentation.Floating(Some(CursorPosition(1, 2)), SurfacePlacement.BelowCursor)
        )
      )
    )
    val layout = LayoutEngine.calculateLayout(state, ViewportSize(100, 24))

    val overlays = OverlayViewModel.fromState(state, layout)
    val overlay  = overlays.belowCursor.getOrElse(fail("Expected find overlay"))

    overlay.header.map(_.plainText) shouldBe Some("find")
    overlay.rows.map(_.plainText) shouldBe List("Find two", "1. 2:1")
    overlay.rows.head.cursorColumn shouldBe Some("Find two".length)
    overlay.rows(1).selected shouldBe true
    overlay.footer.map(_.plainText) shouldBe Some("1 match, 1/1 at 2:1")
    overlay.rect shouldBe layout.belowCursorOverlayRect.get
  }

  it should "allocate spaced framed rows for a context menu at compact density" in {
    val save = Command.typed("save", "Save file", CommandIntent.SaveCurrentFile, label = "Save")
    val find = Command.typed("find", "Find text", CommandIntent.FindInCurrentFile, label = "Find")
    val menu = ContextMenu(
      title = "editor",
      targetFocus = Focus.EditorPane(paneId),
      items = List(
        ContextMenuItem(save.name, save.label, save),
        ContextMenuItem(find.name, find.label, find)
      ),
      selectedIndex = 0
    )
    val buffer = Buffer
      .fromString(bufferId, "one\ntwo\nthree")
      .copy(
        cursors = List(CursorPosition(1, 2))
      )
    val pane = EditorPane.withBuffer(paneId, bufferId)
    val state = AppState.initial.copy(
      config = AppConfig.default
        .withInterfaceDensity(com.serenity.config.InterfaceDensity.Compact)
        .withCommandRunnerItemGapRows(1),
      buffers = Map(bufferId -> buffer),
      bufferOrder = List(bufferId),
      layout = Layout(
        editorPanes = Map(paneId -> pane),
        activeEditorPaneId = Some(paneId)
      ),
      focus = Focus.Surface(SurfaceId("context-menu")),
      uiSurfaces = List(
        UiSurface(
          SurfaceId("context-menu"),
          SurfaceContent.ContextMenu(menu),
          SurfacePresentation.Floating(Some(CursorPosition(1, 2)), SurfacePlacement.BelowCursor)
        )
      )
    )
    val layout = LayoutEngine.calculateLayout(state, ViewportSize(100, 24))

    val overlays = OverlayViewModel.fromState(state, layout)
    val overlay  = overlays.belowCursor.getOrElse(fail("Expected context menu overlay"))

    overlay.rect.height shouldBe SurfaceFrameLayout.frameHeightForItemRows(
      itemRows = 2,
      hasHeader = true,
      hasFooter = true,
      itemGapRows = 1
    )
    overlay.contentRowSlots.map(_.kind) shouldBe List(
      SurfaceContentRowKind.Header,
      SurfaceContentRowKind.Item(0),
      SurfaceContentRowKind.Item(1),
      SurfaceContentRowKind.Footer
    )
  }

  it should "derive an interactive command palette view with cursor and selected row metadata" in {
    val commands = List(
      Command.typed("open", "Open file", CommandIntent.OpenFile),
      Command.typed("close", "Close current file", CommandIntent.CloseCurrentFile)
    )
    val registry = CommandRegistry(commands)
    val runner = CommandRunner.empty
      .activate(registry, AppConfig.default)
      .updateSearchTerm("op")(using registry)
    val buffer = Buffer
      .fromString(bufferId, "one\ntwo\nthree")
      .copy(
        cursors = List(CursorPosition(1, 2))
      )
    val pane = EditorPane.withBuffer(paneId, bufferId)
    val state = AppState.initial.copy(
      buffers = Map(bufferId -> buffer),
      bufferOrder = List(bufferId),
      layout = Layout(
        editorPanes = Map(paneId -> pane),
        activeEditorPaneId = Some(paneId)
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
    val layout = LayoutEngine.calculateLayout(state, ViewportSize(100, 24))

    val overlays = OverlayViewModel.fromState(state, layout)
    val overlay  = overlays.belowCursor.getOrElse(fail("Expected command runner overlay"))

    overlay.header.map(_.plainText) shouldBe Some("search: op")
    overlay.header.flatMap(_.cursorColumn) shouldBe Some("search: op".length)
    overlay.rows.exists(_.selected) shouldBe true
    overlay.rows.map(_.plainText).head should include("Open")
    overlay.rows.map(_.plainText).head should include("Open file")
  }

  it should "skip inactive command palettes so closed overlays do not linger" in {
    val buffer = Buffer
      .fromString(bufferId, "one\ntwo\nthree")
      .copy(
        cursors = List(CursorPosition(1, 2))
      )
    val pane = EditorPane.withBuffer(paneId, bufferId)
    val state = AppState.initial.copy(
      buffers = Map(bufferId -> buffer),
      bufferOrder = List(bufferId),
      layout = Layout(
        editorPanes = Map(paneId -> pane),
        activeEditorPaneId = Some(paneId)
      ),
      focus = Focus.EditorPane(paneId),
      uiSurfaces = List(
        UiSurface(
          SurfaceId("command-runner"),
          SurfaceContent.CommandPalette(CommandRunner.empty),
          SurfacePresentation.Floating(Some(CursorPosition(1, 2)), SurfacePlacement.BelowCursor)
        )
      )
    )
    val layout = LayoutEngine.calculateLayout(state, ViewportSize(100, 24))

    val overlays = OverlayViewModel.fromState(state, layout)

    overlays.belowCursor shouldBe None
  }

  it should "prefer the focused modal surface over earlier below-cursor floating surfaces" in {
    val commands = List(
      Command.typed("open", "Open file", CommandIntent.OpenFile)
    )
    val registry = CommandRegistry(commands)
    val runner = CommandRunner.empty
      .activate(registry, AppConfig.default)
      .updateSearchTerm("op")(using registry)
    val buffer = Buffer
      .fromString(bufferId, "one\ntwo\nthree")
      .copy(
        cursors = List(CursorPosition(1, 2))
      )
    val pane = EditorPane.withBuffer(paneId, bufferId)
    val state = AppState.initial.copy(
      buffers = Map(bufferId -> buffer),
      bufferOrder = List(bufferId),
      layout = Layout(
        editorPanes = Map(paneId -> pane),
        activeEditorPaneId = Some(paneId)
      ),
      focus = Focus.Surface(SurfaceId("file-modal")),
      uiSurfaces = List(
        UiSurface(
          SurfaceId("command-runner"),
          SurfaceContent.CommandPalette(runner),
          SurfacePresentation.Floating(Some(CursorPosition(1, 2)), SurfacePlacement.BelowCursor)
        ),
        UiSurface(
          SurfaceId("file-modal"),
          SurfaceContent.ModalWorkflow(
            Modal.FileWorkflow(
              FileWorkflowState(
                mode = FileWorkflowMode.Open,
                filename = "notes.scala",
                path = "/tmp/project"
              )
            )
          ),
          SurfacePresentation.Floating(Some(CursorPosition(1, 2)), SurfacePlacement.BelowCursor)
        )
      )
    )
    val layout = LayoutEngine.calculateLayout(state, ViewportSize(100, 24))

    val overlays = OverlayViewModel.fromState(state, layout)
    val overlay  = overlays.belowCursor.getOrElse(fail("Expected focused modal overlay"))

    overlay.rows.exists(_.plainText.startsWith("Filename")) shouldBe true
    overlay.rows.exists(_.plainText.startsWith("Path")) shouldBe true
    overlay.header.map(_.plainText) should not contain "search: op"
  }

  it should "stack the command runner and submenu preview beneath the cursor" in {
    val registry          = CommandRegistry.default
    given CommandRegistry = registry
    val runner = CommandRunner.empty
      .activate(registry, AppConfig.default)
      .withActiveCategory(com.serenity.command.CommandCategory.Settings)
    val buffer = Buffer
      .fromString(bufferId, "one\ntwo\nthree")
      .copy(
        cursors = List(CursorPosition(1, 2))
      )
    val pane = EditorPane.withBuffer(paneId, bufferId)
    val state = AppState.initial.copy(
      buffers = Map(bufferId -> buffer),
      bufferOrder = List(bufferId),
      layout = Layout(
        editorPanes = Map(paneId -> pane),
        activeEditorPaneId = Some(paneId)
      ),
      focus = Focus.Surface(SurfaceId("command-runner")),
      uiSurfaces = List(
        UiSurface(
          SurfaceId("command-runner"),
          SurfaceContent.CommandPalette(runner),
          SurfacePresentation.Floating(Some(CursorPosition(1, 2)), SurfacePlacement.BelowCursor)
        ),
        UiSurface(
          SurfaceId("command-runner-submenu"),
          SurfaceContent.CommandPaletteSubmenu(runner, "settings-animation", previewOnly = true),
          SurfacePresentation.Floating(Some(CursorPosition(1, 2)), SurfacePlacement.BelowCursor)
        )
      )
    )
    val layout = LayoutEngine.calculateLayout(state, ViewportSize(100, 24))

    val overlays = OverlayViewModel.fromState(state, layout)
    val stack    = overlays.belowCursorStack

    stack should have size 2
    stack.head.rect.y should be < stack(1).rect.y
    stack(1).rect.y shouldBe stack.head.rect.bottom + 1
  }

  it should "collapse the parent runner to one content row when there is not enough vertical space for both panels" in {
    val registry          = CommandRegistry.default
    given CommandRegistry = registry
    val runner = CommandRunner.empty
      .activate(registry, AppConfig.default)
      .withActiveCategory(com.serenity.command.CommandCategory.Settings)
    val buffer = Buffer
      .fromString(bufferId, "one\ntwo\nthree")
      .copy(
        cursors = List(CursorPosition(2, 2))
      )
    val pane = EditorPane.withBuffer(paneId, bufferId)
    val state = AppState.initial.copy(
      buffers = Map(bufferId -> buffer),
      bufferOrder = List(bufferId),
      layout = Layout(
        editorPanes = Map(paneId -> pane),
        activeEditorPaneId = Some(paneId)
      ),
      focus = Focus.Surface(SurfaceId("command-runner-submenu")),
      uiSurfaces = List(
        UiSurface(
          SurfaceId("command-runner"),
          SurfaceContent.CommandPalette(runner),
          SurfacePresentation.Floating(Some(CursorPosition(2, 2)), SurfacePlacement.BelowCursor)
        ),
        UiSurface(
          SurfaceId("command-runner-submenu"),
          SurfaceContent.CommandPaletteSubmenu(runner, "settings-animation", previewOnly = false),
          SurfacePresentation.Floating(Some(CursorPosition(2, 2)), SurfacePlacement.BelowCursor)
        )
      )
    )
    val layout   = LayoutEngine.calculateLayout(state, ViewportSize(60, 8))
    val overlays = OverlayViewModel.fromState(state, layout)
    val stack    = overlays.belowCursorStack

    stack should have size 2
    stack.head.rows should have size 1
  }

  it should "stack the contextual toolbar above the command runner beneath the cursor" in {
    val commands = List(
      Command.typed("open", "Open file", CommandIntent.OpenFile)
    )
    val registry = CommandRegistry(commands)
    val runner = CommandRunner.empty
      .activate(registry, AppConfig.default)
      .updateSearchTerm("op")(using registry)
    val buffer = Buffer
      .fromString(bufferId, "one\ntwo\nthree")
      .copy(
        cursors = List(CursorPosition(1, 2))
      )
    val pane = EditorPane.withBuffer(paneId, bufferId)
    val state = AppState.initial.copy(
      buffers = Map(bufferId -> buffer),
      bufferOrder = List(bufferId),
      layout = Layout(
        editorPanes = Map(paneId -> pane),
        activeEditorPaneId = Some(paneId)
      ),
      focus = Focus.Surface(SurfaceId("command-runner")),
      config = AppConfig.default.withUiElementGap(0.25),
      uiSurfaces = List(
        UiSurface(
          SurfaceId("contextual-toolbar"),
          SurfaceContent.ContextualToolbar(ContextualToolbarState()),
          SurfacePresentation.Floating(Some(CursorPosition(1, 2)), SurfacePlacement.BelowCursor)
        ),
        UiSurface(
          SurfaceId("command-runner"),
          SurfaceContent.CommandPalette(runner),
          SurfacePresentation.Floating(Some(CursorPosition(1, 2)), SurfacePlacement.BelowCursor)
        )
      )
    )
    val layout = LayoutEngine.calculateLayout(state, ViewportSize(100, 24))

    val overlays = OverlayViewModel.fromState(state, layout)
    val stack    = overlays.belowCursorStack

    stack should have size 2
    stack.head.rows.flatMap(_.segments).exists(_.text.contains("Bold")) shouldBe true
    stack.head.itemGapRows shouldBe 0.25
    stack(1).header.map(_.plainText) shouldBe Some("search: op")
    stack.head.rect.y should be < stack(1).rect.y
  }
