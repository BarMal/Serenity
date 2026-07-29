package com.serenity

import com.serenity.state.models.PaneId
import com.serenity.ui.layout.{SplitAxis, WorkspaceNode, WorkspaceNodeId, WorkspaceTree}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class WorkspaceTreeLifecycleSpec extends AnyFlatSpec with Matchers:

  private val first  = PaneId(0)
  private val second = PaneId(1)
  private val third  = PaneId(2)

  private val nested =
    WorkspaceTree(
      WorkspaceNode.Split(
        WorkspaceNodeId("outer"),
        SplitAxis.Vertical,
        0.4,
        WorkspaceNode.Leaf(WorkspaceNodeId("first"), first),
        WorkspaceNode.Leaf(WorkspaceNodeId("second"), second)
      )
    )

  "WorkspaceTree.split" should "split only the requested leaf and preserve sibling split axes" in {
    val updated = nested
      .split(
        paneId = second,
        newPaneId = third,
        axis = SplitAxis.Horizontal,
        splitId = WorkspaceNodeId("inner"),
        leafId = WorkspaceNodeId("third")
      )
      .getOrElse(fail("expected the pane leaf to split"))

    updated.root shouldBe WorkspaceNode.Split(
      WorkspaceNodeId("outer"),
      SplitAxis.Vertical,
      0.4,
      WorkspaceNode.Leaf(WorkspaceNodeId("first"), first),
      WorkspaceNode.Split(
        WorkspaceNodeId("inner"),
        SplitAxis.Horizontal,
        0.5,
        WorkspaceNode.Leaf(WorkspaceNodeId("second"), second),
        WorkspaceNode.Leaf(WorkspaceNodeId("third"), third)
      )
    )
  }

  "WorkspaceTree.remove" should "remove a leaf and collapse its redundant parent" in {
    val tree = nested
      .split(
        paneId = second,
        newPaneId = third,
        axis = SplitAxis.Horizontal,
        splitId = WorkspaceNodeId("inner"),
        leafId = WorkspaceNodeId("third")
      )
      .getOrElse(fail("expected split tree"))

    tree.remove(second).map(_.root) shouldBe Some(
      WorkspaceNode.Split(
        WorkspaceNodeId("outer"),
        SplitAxis.Vertical,
        0.4,
        WorkspaceNode.Leaf(WorkspaceNodeId("first"), first),
        WorkspaceNode.Leaf(WorkspaceNodeId("third"), third)
      )
    )
    WorkspaceTree(WorkspaceNode.Leaf(WorkspaceNodeId("only"), first)).remove(first) shouldBe None
  }

  "WorkspaceTree.resize" should "update only the owning split and clamp its ratio" in {
    val resized = nested.resize(WorkspaceNodeId("outer"), 2.0).getOrElse(fail("expected owning split"))

    resized.root shouldBe WorkspaceNode.Split(
      WorkspaceNodeId("outer"),
      SplitAxis.Vertical,
      WorkspaceTree.MaximumSplitRatio,
      WorkspaceNode.Leaf(WorkspaceNodeId("first"), first),
      WorkspaceNode.Leaf(WorkspaceNodeId("second"), second)
    )
    resized.resize(WorkspaceNodeId("missing"), 0.5) shouldBe None
  }

  it should "normalize non-finite ratios to the default split ratio" in
    List(Double.NaN, Double.PositiveInfinity, Double.NegativeInfinity).foreach { ratio =>
      val resized = nested.resize(WorkspaceNodeId("outer"), ratio).getOrElse(fail("expected owning split"))

      resized.root match
        case split: WorkspaceNode.Split => split.ratio shouldBe WorkspaceTree.DefaultSplitRatio
        case _: WorkspaceNode.Leaf      => fail("expected split root")
    }

  "WorkspaceTree.validationErrors" should "reject duplicate IDs and pane/tree mismatches" in {
    val duplicate = WorkspaceTree(
      WorkspaceNode.Split(
        WorkspaceNodeId("duplicate"),
        SplitAxis.Horizontal,
        0.5,
        WorkspaceNode.Leaf(WorkspaceNodeId("duplicate"), first),
        WorkspaceNode.Leaf(WorkspaceNodeId("second"), first)
      )
    )

    val errors = duplicate.validationErrors(Set(first, second))

    errors should contain("Workspace tree contains duplicate node IDs: duplicate")
    errors should contain("Workspace tree contains duplicate pane leaves: 0")
    errors should contain("Workspace tree is missing editor panes: 1")
  }
end WorkspaceTreeLifecycleSpec
