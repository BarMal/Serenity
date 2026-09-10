package com.serenity

import com.serenity.rope.Balance
import com.serenity.state.models.{
  AppState,
  AppStateValidation,
  PaneId,
  SurfaceContent,
  SurfaceId,
  SurfacePresentation,
  UiSurface
}
import com.serenity.ui.layout.{
  Layout,
  PanelPosition,
  SessionDockedPanel,
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

  // `WorkspaceTree.fromLegacy` and `Layout.paneOrder`/`splitDirection`/`effectiveWorkspaceTree` (the flat pane-strip
  // compatibility layer) were removed entirely (#821): `Layout.workspaceTree` is now always current whenever any
  // editor pane exists, so there is no more "adapt a flat order/direction into a tree on demand" behaviour to cover.
  // The one surviving piece of that machinery -- seeding a uniform split tree from an ordered pane list, now always
  // horizontal with no direction parameter -- lives on as `SessionDockedPanel.fallbackWorkspaceTree`'s last-resort
  // seed, covered below.
  it should "convert ordered panes into an equivalent uniform horizontal split tree (session/preset fallback seed)" in {
    val tree = SessionDockedPanel.fallbackWorkspaceTree(List(PaneId(0), PaneId(1), PaneId(2)), Nil)

    tree.map(_.paneIds) shouldBe Some(List(PaneId(0), PaneId(1), PaneId(2)))
    tree.flatMap(_.root.axis) shouldBe Some(SplitAxis.Horizontal)
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

  it should "flag a docked surface absent from the workspace tree as invalid (issue #817)" in {
    // The tree is the sole record of docked placement -- unlike the old reconciliation pass, validation no longer
    // derives a tree entry for a `Docked` surface that wasn't placed there by its own pin path (`PanelStateReducer`
    // et al.), so an orphaned one is a genuine error, not something silently repaired.
    val state = AppState.initial.copy(
      runtime = AppState.initial.runtime.copy(
        uiSurfaces = List(
          UiSurface(SurfaceId("surface-0"), SurfaceContent.Outline(Nil, None), SurfacePresentation.Docked)
        )
      )
    )

    AppStateValidation.validated(state).isLeft shouldBe true
  }

  it should "keep a surface docked via its pin path covered by the workspace tree" in {
    val state = DockedPanelFixtures.dock(
      AppState.initial,
      SurfaceId("surface-0"),
      SurfaceContent.Outline(Nil, None),
      PanelPosition.Left,
      30
    )

    AppStateValidation.validated(state).map(_.persisted.layout.workspaceTree.map(_.dockedSurfaceIds)) shouldBe Right(
      Some(List(SurfaceId("surface-0")))
    )
  }

  it should "skip rebuilding an already-reconciled workspace tree" in {
    val state = AppState.initial
    val tree  = state.persisted.layout.workspaceTree

    AppStateValidation.validated(state).map(_.persisted.layout.workspaceTree) shouldBe Right(tree)
    AppStateValidation
      .validated(state)
      .toOption
      .flatMap(_.persisted.layout.workspaceTree)
      .get should be theSameInstanceAs tree.get
  }
end WorkspaceTreeSpec
