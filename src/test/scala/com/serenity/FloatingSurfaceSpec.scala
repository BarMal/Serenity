package com.serenity

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import com.serenity.command.CommandRunner
import com.serenity.rope.Balance
import com.serenity.state.models.*
import com.serenity.ui.layout.Layout

class FloatingSurfaceSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private val paneId   = PaneId(0)
  private val bufferId = BufferId(1)

  private def baseState(cursor: CursorPosition = CursorPosition(4, 9)): AppState =
    val buffer = Buffer.fromString(bufferId, "alpha\nbeta\ngamma").copy(cursors = List(cursor))
    val pane   = EditorPane.withBuffer(paneId, bufferId)

    AppState.initial.copy(
      buffers = Map(bufferId -> buffer),
      bufferOrder = List(bufferId),
      layout = Layout(
        editorPanes = Map(paneId -> pane),
        activeEditorPaneId = Some(paneId)
      ),
      focus = Focus.EditorPane(paneId)
    )

  "AppState.floatingSurfaces" should "return only floating ui surfaces" in {
    val runner = CommandRunner(
      isActive = true,
      searchTerm = "tog",
      selectedIndex = 0,
      filteredCommands = List.empty
    )
    val state = baseState().copy(
      uiSurfaces = List(
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

    state.floatingSurfaces.map(_.id) shouldBe List(SurfaceId("peek"), SurfaceId("command"))
  }
end FloatingSurfaceSpec
