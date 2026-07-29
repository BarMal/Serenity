package com.serenity

import com.serenity.state.models.{PaneId, SurfaceId}
import com.serenity.ui.layout.{PanelPosition, WorkspaceNode, WorkspaceNodeId, WorkspaceTree}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class WorkspaceTreeDockingSpec extends AnyFlatSpec with Matchers:

  private val editor =
    WorkspaceTree(WorkspaceNode.Leaf(WorkspaceNodeId("editor-0"), PaneId(0)))

  "WorkspaceTree.dock" should "insert ordered surface leaves on every edge without replacing the editor" in {
    val docked =
      List(
        SurfaceId("left")   -> PanelPosition.Left,
        SurfaceId("right")  -> PanelPosition.Right,
        SurfaceId("top")    -> PanelPosition.Top,
        SurfaceId("bottom") -> PanelPosition.Bottom
      ).foldLeft(editor) {
        case (tree, (surfaceId, position)) =>
          tree
            .dock(
              surfaceId,
              position,
              WorkspaceNodeId(s"split-${surfaceId.value}"),
              WorkspaceNodeId(s"dock-${surfaceId.value}")
            )
            .getOrElse(fail(s"expected ${surfaceId.value} to dock"))
      }

    docked.paneIds shouldBe List(PaneId(0))
    docked.dockedSurfaceIds shouldBe List(
      SurfaceId("top"),
      SurfaceId("left"),
      SurfaceId("right"),
      SurfaceId("bottom")
    )
  }

  it should "reject duplicate surfaces and node identities" in {
    val docked = editor
      .dock(SurfaceId("outline"), PanelPosition.Right, WorkspaceNodeId("split"), WorkspaceNodeId("dock"))
      .getOrElse(fail("expected dock"))

    docked.dock(
      SurfaceId("outline"),
      PanelPosition.Left,
      WorkspaceNodeId("other-split"),
      WorkspaceNodeId("other-dock")
    ) shouldBe None
    docked.dock(
      SurfaceId("diagnostics"),
      PanelPosition.Bottom,
      WorkspaceNodeId("split"),
      WorkspaceNodeId("diagnostics-dock")
    ) shouldBe None
  }

  "WorkspaceTree.removeSurface" should "remove a docked leaf and collapse its parent" in {
    val docked = editor
      .dock(SurfaceId("outline"), PanelPosition.Right, WorkspaceNodeId("split"), WorkspaceNodeId("dock"))
      .getOrElse(fail("expected dock"))

    docked.removeSurface(SurfaceId("outline")) shouldBe Some(editor)
  }

  "WorkspaceTree.resizeSurface" should "resize the owning split from the surface allocation" in {
    val right = editor
      .dock(SurfaceId("outline"), PanelPosition.Right, WorkspaceNodeId("split"), WorkspaceNodeId("dock"))
      .getOrElse(fail("expected dock"))
    val resized = right.resizeSurface(SurfaceId("outline"), 0.3).getOrElse(fail("expected resize"))

    resized.root match
      case WorkspaceNode.Split(_, _, ratio, _, _) => ratio shouldBe 0.7
      case _                                      => fail("expected split root")
  }

  "WorkspaceTree.moveSurface" should "preserve the dock leaf identity while moving it to another edge" in {
    val right = editor
      .dock(SurfaceId("outline"), PanelPosition.Right, WorkspaceNodeId("split"), WorkspaceNodeId("dock"))
      .getOrElse(fail("expected dock"))
    val moved = right
      .moveSurface(SurfaceId("outline"), PanelPosition.Top, WorkspaceNodeId("moved-split"))
      .getOrElse(fail("expected move"))

    moved.nodeIdForSurface(SurfaceId("outline")) shouldBe Some(WorkspaceNodeId("dock"))
    moved.dockedSurfaceIds shouldBe List(SurfaceId("outline"))
  }

  "WorkspaceTree.validationErrors" should "reject duplicate, missing, and unknown docked surfaces" in {
    val duplicate = WorkspaceTree(
      WorkspaceNode.Split(
        WorkspaceNodeId("split"),
        com.serenity.ui.layout.SplitAxis.Horizontal,
        0.5,
        WorkspaceNode.DockedSurface(WorkspaceNodeId("left"), SurfaceId("outline"), PanelPosition.Left),
        WorkspaceNode.DockedSurface(WorkspaceNodeId("right"), SurfaceId("outline"), PanelPosition.Right)
      )
    )

    val errors = duplicate.validationErrors(Set.empty, Set(SurfaceId("diagnostics")))

    errors should contain("Workspace tree contains duplicate docked surfaces: outline")
    errors should contain("Workspace tree is missing docked surfaces: diagnostics")
    errors should contain("Workspace tree references non-existent docked surfaces: outline")
    errors should contain("Workspace tree must contain at least one editor pane")
  }
end WorkspaceTreeDockingSpec
