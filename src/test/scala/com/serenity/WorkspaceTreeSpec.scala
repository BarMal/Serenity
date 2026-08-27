package com.serenity

import com.serenity.rope.Balance
import com.serenity.state.models.{
  AppState,
  EditorPane,
  PaneId,
  SurfaceContent,
  SurfaceId,
  SurfacePresentation,
  UiSurface
}
import com.serenity.ui.layout.{
  Layout,
  PaneSplitDirection,
  PanelPosition,
  SplitAxis,
  WorkspaceNode,
  WorkspaceNodeId,
  WorkspaceTree
}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class WorkspaceTreeSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  "WorkspaceTree" should "retain stable node and pane identities through nested splits" in {
    val first  = WorkspaceNode.Leaf(WorkspaceNodeId("editor-0"), PaneId(0))
    val second = WorkspaceNode.Leaf(WorkspaceNodeId("editor-1"), PaneId(1))
    val third  = WorkspaceNode.Leaf(WorkspaceNodeId("editor-2"), PaneId(2))
    val tree = WorkspaceTree(
      WorkspaceNode.Split(
        WorkspaceNodeId("outer"),
        SplitAxis.Horizontal,
        0.4,
        first,
        WorkspaceNode.Split(WorkspaceNodeId("inner"), SplitAxis.Vertical, 0.5, second, third)
      )
    )

    tree.paneIds shouldBe List(PaneId(0), PaneId(1), PaneId(2))
    tree.nodeIds shouldBe List(
      WorkspaceNodeId("outer"),
      WorkspaceNodeId("editor-0"),
      WorkspaceNodeId("inner"),
      WorkspaceNodeId("editor-1"),
      WorkspaceNodeId("editor-2")
    )
  }

  it should "convert legacy ordered panes into an equivalent uniform split tree" in {
    val tree = WorkspaceTree.fromLegacy(
      List(PaneId(0), PaneId(1), PaneId(2)),
      PaneSplitDirection.Horizontal
    )

    tree.map(_.paneIds) shouldBe Some(List(PaneId(0), PaneId(1), PaneId(2)))
    tree.flatMap(_.root.axis) shouldBe Some(SplitAxis.Horizontal)
  }

  it should "adapt legacy layouts in memory without changing their flat pane order" in {
    val panes = Map(
      PaneId(0) -> EditorPane.empty(PaneId(0)),
      PaneId(1) -> EditorPane.empty(PaneId(1))
    )
    val layout = Layout(
      editorPanes = panes,
      activeEditorPaneId = Some(PaneId(0)),
      paneOrder = List(PaneId(1), PaneId(0)),
      splitDirection = PaneSplitDirection.Vertical
    )

    layout.orderedPaneIds shouldBe List(PaneId(1), PaneId(0))
    layout.effectiveWorkspaceTree.map(_.paneIds) shouldBe Some(List(PaneId(1), PaneId(0)))
    layout.effectiveWorkspaceTree.flatMap(_.root.axis) shouldBe Some(SplitAxis.Vertical)
  }

  it should "initialize the default layout as one explicit editor leaf" in {
    Layout.initial.workspaceTree shouldBe Some(
      WorkspaceTree(WorkspaceNode.Leaf(WorkspaceNodeId("editor-0"), PaneId(0)))
    )
  }

  it should "initialize the default app state with one explicit editor leaf" in {
    AppState.initial.persisted.layout.workspaceTree shouldBe Some(
      WorkspaceTree(WorkspaceNode.Leaf(WorkspaceNodeId("editor-0"), PaneId(0)))
    )
  }

  it should "reconcile pinned surfaces added to an explicit default tree" in {
    val state = AppState.initial.copy(
      runtime = AppState.initial.runtime.copy(
        uiSurfaces = List(
          UiSurface(
            SurfaceId("surface-0"),
            SurfaceContent.Outline(Nil, None),
            SurfacePresentation.Pinned(PanelPosition.Left, 30)
          )
        )
      )
    )

    state.validated.map(_.persisted.layout.workspaceTree.map(_.dockedSurfaceIds)) shouldBe Right(
      Some(List(SurfaceId("surface-0")))
    )
  }

  it should "skip rebuilding an already-reconciled workspace tree" in {
    val state = AppState.initial
    val tree  = state.persisted.layout.workspaceTree

    state.validated.map(_.persisted.layout.workspaceTree) shouldBe Right(tree)
    state.validated.toOption.flatMap(_.persisted.layout.workspaceTree).get should be theSameInstanceAs tree.get
  }
end WorkspaceTreeSpec
