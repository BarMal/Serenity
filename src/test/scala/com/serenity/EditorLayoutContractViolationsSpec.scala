package com.serenity

import com.serenity.command.{CommandRegistry, CommandRunner, FileIntent}
import com.serenity.config.AppConfig
import com.serenity.rope.Balance
import com.serenity.state.models.*
import com.serenity.ui.layout.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class EditorLayoutContractViolationsSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private val viewport = ViewportSize(120, 36)

  "EditorLayoutContract" should "enforce configured minimum gaps between stacked below-cursor overlays" in {
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
          activeEditorPaneId = Some(PaneId(0))
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
          workspaceTree = Some(
            WorkspaceTree(
              WorkspaceNode.Split(
                WorkspaceNodeId("editors"),
                SplitAxis.Vertical,
                0.5,
                WorkspaceNode.Leaf(WorkspaceNodeId("editor-0"), PaneId(0)),
                WorkspaceNode.Leaf(WorkspaceNodeId("editor-1"), PaneId(1))
              )
            )
          )
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
          activeEditorPaneId = Some(PaneId(0))
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
end EditorLayoutContractViolationsSpec
