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
          activeEditorPaneId = Some(paneId)
        ),
        focus = Focus.EditorPane(paneId)
      )
    )

  private def dockedState(panels: List[UiSurface]): AppState =
    val tree =
      panels.foldLeft(baseState.persisted.layout.effectiveWorkspaceTree.getOrElse(fail("expected editor tree"))) {
        (workspaceTree, panel) =>
          val position = panel.presentation match
            case SurfacePresentation.Pinned(value, _) => value
            case other                                => fail(s"expected pinned panel, got $other")
          workspaceTree
            .dock(
              panel.id,
              position,
              WorkspaceNodeId(s"split-${panel.id.value}"),
              WorkspaceNodeId(s"dock-${panel.id.value}")
            )
            .getOrElse(fail(s"expected ${panel.id.value} to dock"))
      }
    baseState.copy(
      persisted = baseState.persisted.copy(layout = baseState.persisted.layout.copy(workspaceTree = Some(tree))),
      runtime = baseState.runtime.copy(uiSurfaces = panels)
    )

  "LayoutEngine.calculateLayout" should "allocate pinned panel rects and shrink the editor workspace around them" in {
    val state = baseState.copy(
      runtime = baseState.runtime.copy(uiSurfaces =
        List(
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

  it should "place an expanded panel in the central editor workspace and keep side panels out of the layout" in {
    val expandedPanel = UiSurface(
      SurfaceId("expanded-outline"),
      SurfaceContent.Outline(Nil),
      SurfacePresentation.Expanded(PanelPosition.Right, 24)
    )
    val state = baseState.copy(
      persisted = baseState.persisted.copy(focus = Focus.Surface(expandedPanel.id)),
      runtime = baseState.runtime.copy(uiSurfaces = List(expandedPanel))
    )

    val layout = LayoutEngine.calculateLayout(state, ViewportSize(120, 40))

    layout.pinnedPanelRects shouldBe empty
    layout.expandedPanelRect shouldBe Some(layout.editorPanelRect)
    layout.editorPanelRect shouldBe LayoutEngine.calculateLayout(baseState, ViewportSize(120, 40)).editorPanelRect
  }

  it should "split same-side left and right panels into per-surface rects" in {
    val state = baseState.copy(
      runtime = baseState.runtime.copy(uiSurfaces =
        List(
          UiSurface.fromPanelContent(
            SurfaceId("left-one"),
            PanelContent.Outline(Nil),
            PanelPosition.Left,
            20
          ),
          UiSurface.fromPanelContent(
            SurfaceId("left-two"),
            PanelContent.Diagnostics(Nil),
            PanelPosition.Left,
            24
          )
        )
      )
    )

    val layout = LayoutEngine.calculateLayout(state, ViewportSize(100, 31))

    layout.pinnedPanelRects(PanelPosition.Left) shouldBe LayoutRect(0, 0, 24, 30)
    layout.pinnedSurfaceRects(SurfaceId("left-one")) shouldBe LayoutRect(0, 0, 24, 15)
    layout.pinnedSurfaceRects(SurfaceId("left-two")) shouldBe LayoutRect(0, 15, 24, 15)
    layout.editorPanelRect.x shouldBe 27
  }

  it should "split same-side top and bottom panels into per-surface rects" in {
    val state = baseState.copy(
      runtime = baseState.runtime.copy(uiSurfaces =
        List(
          UiSurface.fromPanelContent(
            SurfaceId("bottom-one"),
            PanelContent.Terminal("build", 0),
            PanelPosition.Bottom,
            6
          ),
          UiSurface.fromPanelContent(
            SurfaceId("bottom-two"),
            PanelContent.Diagnostics(Nil),
            PanelPosition.Bottom,
            8
          )
        )
      )
    )

    val layout = LayoutEngine.calculateLayout(state, ViewportSize(80, 25))

    layout.pinnedPanelRects(PanelPosition.Bottom) shouldBe LayoutRect(0, 16, 80, 8)
    layout.pinnedSurfaceRects(SurfaceId("bottom-one")) shouldBe LayoutRect(0, 16, 40, 8)
    layout.pinnedSurfaceRects(SurfaceId("bottom-two")) shouldBe LayoutRect(40, 16, 40, 8)
    layout.editorPanelRect.bottom shouldBe 16
  }

  it should "derive ordered same-edge panel rectangles from docked workspace leaves" in {
    val first = UiSurface.fromPanelContent(SurfaceId("right-one"), PanelContent.Outline(Nil), PanelPosition.Right, 25)
    val second =
      UiSurface.fromPanelContent(SurfaceId("right-two"), PanelContent.Diagnostics(Nil), PanelPosition.Right, 25)
    val tree = baseState.persisted.layout.effectiveWorkspaceTree
      .flatMap(
        _.dock(first.id, PanelPosition.Right, WorkspaceNodeId("right-split"), WorkspaceNodeId("right-one"))
      )
      .flatMap(
        _.dock(second.id, PanelPosition.Right, WorkspaceNodeId("right-stack"), WorkspaceNodeId("right-two"))
      )
      .getOrElse(fail("expected docked workspace"))
    val state = baseState.copy(
      persisted = baseState.persisted.copy(layout = baseState.persisted.layout.copy(workspaceTree = Some(tree))),
      runtime = baseState.runtime.copy(uiSurfaces = List(first, second))
    )

    val layout = LayoutEngine.calculateLayout(state, ViewportSize(100, 31))

    layout.pinnedSurfaceRects(first.id) shouldBe LayoutRect(75, 0, 25, 15)
    layout.pinnedSurfaceRects(second.id) shouldBe LayoutRect(75, 15, 25, 15)
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
      val panel = UiSurface.fromPanelContent(
        SurfaceId(s"oversized-$position"),
        PanelContent.Diagnostics(Nil),
        position,
        1000
      )

      val layout = LayoutEngine.calculateLayout(dockedState(List(panel)), viewport)

      position match
        case PanelPosition.Left | PanelPosition.Right =>
          layout.editorPanelRect.width should be >= baseState.persisted.config.minimumPaneWidth
        case PanelPosition.Top | PanelPosition.Bottom =>
          layout.editorPanelRect.height should be >= 5
    }
  }

  it should "retain the editor minimum between competing oversized opposite-edge panels" in {
    val panels = List(
      UiSurface.fromPanelContent(
        SurfaceId("oversized-left"),
        PanelContent.Outline(Nil),
        PanelPosition.Left,
        1000
      ),
      UiSurface.fromPanelContent(
        SurfaceId("oversized-right"),
        PanelContent.Diagnostics(Nil),
        PanelPosition.Right,
        1000
      ),
      UiSurface.fromPanelContent(
        SurfaceId("oversized-top"),
        PanelContent.Terminal("", 0),
        PanelPosition.Top,
        1000
      ),
      UiSurface.fromPanelContent(
        SurfaceId("oversized-bottom"),
        PanelContent.Diagnostics(Nil),
        PanelPosition.Bottom,
        1000
      )
    )

    val layout = LayoutEngine.calculateLayout(dockedState(panels), ViewportSize(100, 31))

    layout.editorPanelRect.width should be >= baseState.persisted.config.minimumPaneWidth
    layout.editorPanelRect.height should be >= 5
  }
end PinnedPanelLayoutSpec
