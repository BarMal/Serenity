package com.serenity

import java.nio.file.Paths

import com.serenity.keystroke.events.{Direction, PanelInputEvent}
import com.serenity.rope.Balance
import com.serenity.state.components.{ComponentResult, PinnedPanelComponent}
import com.serenity.state.models.*
import com.serenity.ui.layout.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class PinnedPanelComponentSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private val paneId = PaneId(0)

  private def baseState: AppState =
    val bufferId = BufferId(1)
    val buffer   = Buffer.fromString(bufferId, "hello")
    val pane     = EditorPane.withBuffer(paneId, bufferId)
    AppState.initial.copy(
      buffers = Map(bufferId -> buffer),
      bufferOrder = List(bufferId),
      layout = Layout(
        editorPanes = Map(paneId -> pane),
        activeEditorPaneId = Some(paneId)
      ),
      focus = Focus.EditorPane(paneId)
    )

  "PinnedPanelComponent" should "treat pinned ui surfaces as the live source of truth" in {
    val surface = UiSurface.fromPanelContent(
      SurfaceId("left-panel"),
      PanelContent.DirectoryTree(DirectoryTreeData(Paths.get("/repo")), None),
      PanelPosition.Left,
      24
    )
    val state = baseState.copy(
      uiSurfaces = List(surface),
      focus = Focus.Surface(surface.id)
    )

    val component = PinnedPanelComponent(PanelPosition.Left)

    component.processEvent(PanelInputEvent.ReturnFocus, state).shouldBe(ComponentResult.FocusTransfer(Focus.EditorPane(paneId)))
  }

  it should "keep navigation local to the pinned panel" in {
    val surface = UiSurface.fromPanelContent(
      SurfaceId("left-panel"),
      PanelContent.DirectoryTree(DirectoryTreeData(Paths.get("/repo")), None),
      PanelPosition.Left,
      24
    )
    val state = baseState.copy(
      uiSurfaces = List(surface),
      focus = Focus.Surface(surface.id)
    )

    val component = PinnedPanelComponent(PanelPosition.Left)

    component.processEvent(PanelInputEvent.Navigate(Direction.Down), state).shouldBe(ComponentResult.NoChange)
  }

  it should "ignore input when no pinned surface exists at the requested position" in {
    val component = PinnedPanelComponent(PanelPosition.Right)

    component.processEvent(PanelInputEvent.NoOp, baseState).shouldBe(ComponentResult.NoChange)
  }
end PinnedPanelComponentSpec
