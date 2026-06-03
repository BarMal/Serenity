package com.serenity

import com.serenity.rope.Balance
import com.serenity.command.{Command, CommandRegistry, CommandRunner}
import com.serenity.config.AppConfig
import com.serenity.state.models.*
import com.serenity.ui.layout.{Layout, LayoutEngine, ViewportSize}
import com.serenity.ui.renderer.OverlayViewModel
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class OverlayViewModelSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private val paneId   = PaneId(0)
  private val bufferId = BufferId(1)

  private def stateWithQuickInfo(text: String): AppState =
    val buffer = Buffer.fromString(bufferId, "one\ntwo\nthree").copy(
      cursors = List(CursorPosition(1, 2))
    )
    val pane   = EditorPane.withBuffer(paneId, bufferId)

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
  }

  it should "derive a below-cursor modal overlay view from unified floating surfaces" in {
    val buffer = Buffer.fromString(bufferId, "one\ntwo\nthree").copy(
      cursors = List(CursorPosition(1, 2))
    )
    val pane   = EditorPane.withBuffer(paneId, bufferId)
    val state  = AppState.initial.copy(
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

  it should "derive an interactive command palette view with cursor and selected row metadata" in {
    val commands = List(
      Command("open", "Open file", _ => cats.effect.IO.unit),
      Command("close", "Close current file", _ => cats.effect.IO.unit)
    )
    val registry = CommandRegistry(commands)
    val runner = CommandRunner.empty
      .activate(registry, AppConfig.default)
      .updateSearchTerm("op")(using registry)
    val buffer = Buffer.fromString(bufferId, "one\ntwo\nthree").copy(
      cursors = List(CursorPosition(1, 2))
    )
    val pane   = EditorPane.withBuffer(paneId, bufferId)
    val state  = AppState.initial.copy(
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
    val overlay = overlays.belowCursor.getOrElse(fail("Expected command runner overlay"))

    overlay.header.map(_.plainText) shouldBe Some("search: op")
    overlay.header.flatMap(_.cursorColumn) shouldBe Some("search: op".length)
    overlay.rows.exists(_.selected) shouldBe true
    overlay.rows.map(_.plainText).head should include("open")
    overlay.rows.map(_.plainText).head should include("Open file")
  }

  it should "skip inactive command palettes so closed overlays do not linger" in {
    val buffer = Buffer.fromString(bufferId, "one\ntwo\nthree").copy(
      cursors = List(CursorPosition(1, 2))
    )
    val pane   = EditorPane.withBuffer(paneId, bufferId)
    val state  = AppState.initial.copy(
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
      Command("open", "Open file", _ => cats.effect.IO.unit)
    )
    val registry = CommandRegistry(commands)
    val runner = CommandRunner.empty
      .activate(registry, AppConfig.default)
      .updateSearchTerm("op")(using registry)
    val buffer = Buffer.fromString(bufferId, "one\ntwo\nthree").copy(
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
    val overlay = overlays.belowCursor.getOrElse(fail("Expected focused modal overlay"))

    overlay.rows.exists(_.plainText.startsWith("Filename")) shouldBe true
    overlay.rows.exists(_.plainText.startsWith("Path")) shouldBe true
    overlay.header.map(_.plainText) should not contain "search: op"
  }

  it should "stack the command runner and submenu preview beneath the cursor" in {
    val registry = CommandRegistry.default
    given CommandRegistry = registry
    val runner = CommandRunner.empty
      .activate(registry, AppConfig.default)
      .withActiveCategory(com.serenity.command.CommandCategory.Settings)
    val buffer = Buffer.fromString(bufferId, "one\ntwo\nthree").copy(
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
    val registry = CommandRegistry.default
    given CommandRegistry = registry
    val runner = CommandRunner.empty
      .activate(registry, AppConfig.default)
      .withActiveCategory(com.serenity.command.CommandCategory.Settings)
    val buffer = Buffer.fromString(bufferId, "one\ntwo\nthree").copy(
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
    val layout = LayoutEngine.calculateLayout(state, ViewportSize(60, 8))
    val overlays = OverlayViewModel.fromState(state, layout)
    val stack    = overlays.belowCursorStack

    stack should have size 2
    stack.head.rows should have size 1
  }
