package com.serenity

import java.nio.file.Paths

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import com.serenity.rope.Balance
import com.serenity.state.models.*
import com.serenity.ui.layout.*

class PinnedPanelLayoutSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private val paneId   = PaneId(0)
  private val bufferId = BufferId(1)

  private def baseState: AppState =
    val buffer = Buffer.fromString(bufferId, "one\ntwo\nthree")
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

  "LayoutEngine.calculateLayout" should "allocate pinned panel rects and shrink the editor workspace around them" in {
    val state = baseState.copy(
      uiSurfaces = List(
        UiSurface.fromPanelContent(
          SurfaceId("surface-left"),
          PanelContent.DirectoryTree(DirectoryTreeData(Paths.get("/repo")), None),
          PanelPosition.Left,
          24
        ),
        UiSurface.fromPanelContent(
          SurfaceId("surface-bottom"),
          PanelContent.DirectoryTree(DirectoryTreeData(Paths.get("/repo")), None),
          PanelPosition.Bottom,
          6
        )
      )
    )

    val noPanels = LayoutEngine.calculateLayout(baseState, ViewportSize(120, 40))
    val layout   = LayoutEngine.calculateLayout(state, ViewportSize(120, 40))

    layout.pinnedPanelRects(PanelPosition.Left) shouldBe LayoutRect(0, 0, 24, 33)
    layout.pinnedPanelRects(PanelPosition.Bottom) shouldBe LayoutRect(0, 33, 120, 6)
    layout.gutterRect shouldBe Some(LayoutRect(0, 39, 120, 1))

    layout.editorPanelRect.x should be > noPanels.editorPanelRect.x
    layout.editorPanelRect.width should be < noPanels.editorPanelRect.width
    layout.editorPanelRect.bottom shouldBe 33
  }
end PinnedPanelLayoutSpec
