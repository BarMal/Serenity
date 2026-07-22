package com.serenity

import com.serenity.state.models.{EditorPane, PaneId}
import com.serenity.ui.layout.{Layout, PaneSplitDirection, SplitAxis, WorkspaceNode, WorkspaceNodeId, WorkspaceTree}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class WorkspaceTreeSpec extends AnyFlatSpec with Matchers:

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
end WorkspaceTreeSpec
