package com.serenity

import java.nio.file.Paths

import com.serenity.rope.Balance
import com.serenity.state.models.*
import com.serenity.ui.layout.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class PinnedPanelLayoutSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private val paneId   = PaneId(0)
  private val bufferId = BufferId(1)

  private def baseState: AppState =
    val buffer = Buffer.fromString(bufferId, "one\ntwo\nthree")
    val pane   = EditorPane.withBuffer(paneId, bufferId)

    AppState.initial.copy(
      persisted = AppState.initial.persisted.copy(
        buffers = Map(bufferId -> buffer),
        bufferOrder = List(bufferId),
        layout = Layout(
          editorPanes = Map(paneId -> pane),
          activeEditorPaneId = Some(paneId),
          workspaceTree = Some(TestWorkspaceTrees.linear(paneId))
        ),
        focus = Focus.EditorPane(paneId)
      )
    )

  // The requested `size` is seeded into the tree as a ratio against `viewport` (issue #817: the tree's ratio is the
  // sole size record for a docked panel), so it must match whatever viewport the test later renders at for the
  // ratio to round-trip back to the exact requested pixel size.
  private def dockedState(
    panels: List[(SurfaceId, PanelContent, PanelPosition, Int)],
    viewport: ViewportSize
  ): AppState =
    val withViewport = baseState.copy(runtime = baseState.runtime.copy(viewportSize = Some(viewport)))
    DockedPanelFixtures.dockAll(withViewport, panels)

  "LayoutEngine.calculateLayout" should "allocate pinned panel rects and shrink the editor workspace around them" in {
    val viewport = ViewportSize(120, 40)
    val state = dockedState(
      List(
        (
          SurfaceId("surface-left"),
          PanelContent.DirectoryTree(DirectoryTreeData(Paths.get("/repo")), None),
          PanelPosition.Left,
          24
        ),
        (
          SurfaceId("surface-bottom"),
          PanelContent.DirectoryTree(DirectoryTreeData(Paths.get("/repo")), None),
          PanelPosition.Bottom,
          6
        )
      ),
      viewport
    )

    val noPanels = LayoutEngine.calculateLayout(baseState, viewport)
    val layout   = LayoutEngine.calculateLayout(state, viewport)

    layout.pinnedPanelRects(PanelPosition.Left) shouldBe LayoutRect(0, 0, 24, 33)
    layout.pinnedPanelRects(PanelPosition.Bottom) shouldBe LayoutRect(0, 33, 120, 6)
    layout.gutterRect shouldBe Some(LayoutRect(0, 39, 120, 1))

    layout.editorPanelRect.x should be > noPanels.editorPanelRect.x
    layout.editorPanelRect.width should be < noPanels.editorPanelRect.width
    layout.editorPanelRect.bottom shouldBe 33
  }

  it should "place an expanded panel in the central editor workspace and keep side panels out of the layout" in {
    val expandViewport = ViewportSize(120, 40)
    val expandedId     = SurfaceId("expanded-outline")
    val dockedOnly = dockedState(List((expandedId, PanelContent.Outline(Nil), PanelPosition.Right, 24)), expandViewport)
    val expanded   = DockedPanelFixtures.expand(dockedOnly, expandedId)
    val state      = expanded.copy(persisted = expanded.persisted.copy(focus = Focus.Surface(expandedId)))

    val layout = LayoutEngine.calculateLayout(state, expandViewport)

    layout.expandedPanelRect shouldBe Some(layout.editorPanelRect)
    layout.editorPanelRect shouldBe LayoutEngine.calculateLayout(baseState, expandViewport).editorPanelRect
  }

  it should "split same-side left and right panels into per-surface rects" in {
    val viewport = ViewportSize(100, 31)
    val state = dockedState(
      List(
        (SurfaceId("left-one"), PanelContent.Outline(Nil), PanelPosition.Left, 20),
        (SurfaceId("left-two"), PanelContent.Diagnostics(Nil), PanelPosition.Left, 24)
      ),
      viewport
    )

    val layout = LayoutEngine.calculateLayout(state, viewport)

    layout.pinnedPanelRects(PanelPosition.Left) shouldBe LayoutRect(0, 0, 24, 30)
    layout.pinnedSurfaceRects(SurfaceId("left-one")) shouldBe LayoutRect(0, 0, 24, 15)
    layout.pinnedSurfaceRects(SurfaceId("left-two")) shouldBe LayoutRect(0, 15, 24, 15)
    layout.editorPanelRect.x shouldBe 27
  }

  it should "split same-side top and bottom panels into per-surface rects" in {
    val viewport = ViewportSize(80, 25)
    val state = dockedState(
      List(
        (SurfaceId("bottom-one"), PanelContent.Terminal("build", 0), PanelPosition.Bottom, 6),
        (SurfaceId("bottom-two"), PanelContent.Diagnostics(Nil), PanelPosition.Bottom, 8)
      ),
      viewport
    )

    val layout = LayoutEngine.calculateLayout(state, viewport)

    layout.pinnedPanelRects(PanelPosition.Bottom) shouldBe LayoutRect(0, 16, 80, 8)
    layout.pinnedSurfaceRects(SurfaceId("bottom-one")) shouldBe LayoutRect(0, 16, 40, 8)
    layout.pinnedSurfaceRects(SurfaceId("bottom-two")) shouldBe LayoutRect(40, 16, 40, 8)
    layout.editorPanelRect.bottom shouldBe 16
  }

  it should "derive ordered same-edge panel rectangles from docked workspace leaves" in {
    val viewport = ViewportSize(100, 31)
    val firstId  = SurfaceId("right-one")
    val secondId = SurfaceId("right-two")
    val state = dockedState(
      List(
        (firstId, PanelContent.Outline(Nil), PanelPosition.Right, 25),
        (secondId, PanelContent.Diagnostics(Nil), PanelPosition.Right, 25)
      ),
      viewport
    )

    val layout = LayoutEngine.calculateLayout(state, viewport)

    layout.pinnedSurfaceRects(firstId) shouldBe LayoutRect(75, 0, 25, 15)
    layout.pinnedSurfaceRects(secondId) shouldBe LayoutRect(75, 15, 25, 15)
    layout.pinnedPanelRects(PanelPosition.Right) shouldBe LayoutRect(75, 0, 25, 30)
    layout.editorPanelRect.right shouldBe 75
  }

  it should "retain the configured editor minimum beside oversized panels on every edge" in {
    val viewport = ViewportSize(100, 31)
    List(
      PanelPosition.Left,
      PanelPosition.Right,
      PanelPosition.Top,
      PanelPosition.Bottom
    ).foreach { position =>
      val state = dockedState(
        List((SurfaceId(s"oversized-$position"), PanelContent.Diagnostics(Nil), position, 1000)),
        viewport
      )

      val layout = LayoutEngine.calculateLayout(state, viewport)

      position match
        case PanelPosition.Left | PanelPosition.Right =>
          layout.editorPanelRect.width should be >= baseState.persisted.config.editorConfig.minimumPaneWidth
        case PanelPosition.Top | PanelPosition.Bottom =>
          layout.editorPanelRect.height should be >= 5
    }
  }

  it should "retain the editor minimum between competing oversized opposite-edge panels" in {
    val viewport = ViewportSize(100, 31)
    val state = dockedState(
      List(
        (SurfaceId("oversized-left"), PanelContent.Outline(Nil), PanelPosition.Left, 1000),
        (SurfaceId("oversized-right"), PanelContent.Diagnostics(Nil), PanelPosition.Right, 1000),
        (SurfaceId("oversized-top"), PanelContent.Terminal("", 0), PanelPosition.Top, 1000),
        (SurfaceId("oversized-bottom"), PanelContent.Diagnostics(Nil), PanelPosition.Bottom, 1000)
      ),
      viewport
    )

    val layout = LayoutEngine.calculateLayout(state, viewport)

    layout.editorPanelRect.width should be >= baseState.persisted.config.editorConfig.minimumPaneWidth
    layout.editorPanelRect.height should be >= 5
  }
end PinnedPanelLayoutSpec
