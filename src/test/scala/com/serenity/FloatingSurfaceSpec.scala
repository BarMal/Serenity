package com.serenity

import com.serenity.command.{CommandPaletteState, CommandRunner, CommandRunnerSurface}
import com.serenity.rope.Balance
import com.serenity.state.models.*
import com.serenity.ui.layout.*
import com.serenity.ui.renderer.Renderer
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class FloatingSurfaceSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private val paneId   = PaneId(0)
  private val bufferId = BufferId(1)

  private def baseState(cursor: CursorPosition = CursorPosition(4, 9)): AppState =
    val buffer = Buffer.fromString(bufferId, "alpha\nbeta\ngamma").copy(editing = EditingState(cursors = List(cursor)))
    val pane   = EditorPane.withBuffer(paneId, bufferId)

    val initial = AppState.initial

    initial.copy(
      persisted = initial.persisted.copy(
        buffers = Map(bufferId -> buffer),
        bufferOrder = List(bufferId),
        layout = Layout(
          editorPanes = Map(paneId -> pane),
          activeEditorPaneId = Some(paneId)
        ),
        focus = Focus.EditorPane(paneId)
      )
    )

  "AppState.floatingSurfaces" should "return only floating ui surfaces" in {
    val runner = CommandRunner(
      isActive = true,
      surface = CommandRunnerSurface.Palette(
        CommandPaletteState(searchTerm = "tog", selectedIndex = 0, filteredCommands = List.empty)
      )
    )
    val base = baseState()
    val state = base.copy(
      runtime = base.runtime.copy(uiSurfaces =
        List(
          UiSurface(
            SurfaceId("peek"),
            SurfaceContent.QuickInfo("map"),
            SurfacePresentation.Floating(Some(CursorPosition(2, 5)), SurfacePlacement.AboveCursor),
            dismissOnMove = true
          ),
          UiSurface(
            SurfaceId("command"),
            SurfaceContent.CommandPalette(runner),
            SurfacePresentation.Floating(Some(CursorPosition(4, 9)), SurfacePlacement.BelowCursor)
          ),
          UiSurface(
            SurfaceId("pinned"),
            SurfaceContent.Diagnostics(Nil),
            SurfacePresentation.Pinned(com.serenity.ui.layout.PanelPosition.Bottom, 8)
          )
        )
      )
    )

    state.floatingSurfaces.map(_.id) shouldBe List(SurfaceId("peek"), SurfaceId("command"))
  }

  "Renderer" should "paint an expanded surface above the editor pane it replaces" in {
    val expanded = UiSurface(
      SurfaceId("diagnostics"),
      SurfaceContent.Diagnostics(Nil),
      SurfacePresentation.Expanded(PanelPosition.Right, 22)
    )
    val expandedBase = baseState()
    val state        = expandedBase.copy(runtime = expandedBase.runtime.copy(uiSurfaces = List(expanded)))
    val surface      = new MockRenderSurface(80, 24)

    Renderer.render(state, cursorVisible = true, surface, ViewportSize(80, 24))

    val frame = UiSceneSnapshot
      .from(state, ViewportSize(80, 24))
      .workspace
      .find(_.id == SceneNodeId.Surface(expanded.id))
      .map(_.frameRect)
      .getOrElse(fail("expected expanded surface frame"))
    (frame.x until frame.right).map(surface.getChar(_, frame.y)).mkString.trim should include("diagnostics")
  }
end FloatingSurfaceSpec
