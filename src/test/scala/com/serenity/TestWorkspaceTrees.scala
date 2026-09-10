package com.serenity

import com.serenity.state.models.PaneId
import com.serenity.ui.layout.{SplitAxis, WorkspaceNode, WorkspaceNodeId, WorkspaceTree}

/** Builds a simple left-to-right nested horizontal split tree from an explicit pane-id order, for test fixtures that
  * need a valid `Layout.workspaceTree` covering a specific set of panes but don't care about split topology
  * themselves. `Layout.workspaceTree` is now always populated whenever any editor pane exists (#821: the old flat
  * `paneOrder`/`splitDirection` compatibility layer -- and its implicit "no tree yet" fallback -- is gone), so any
  * fixture building a `Layout` with real panes needs an explicit tree. Mirrors
  * `WorkspaceSnapshot.linearWorkspaceTree`'s shape (not reused directly: that one is private to its file).
  */
object TestWorkspaceTrees:

  def linear(paneIds: PaneId*): WorkspaceTree =
    def leaf(paneId: PaneId): WorkspaceNode = WorkspaceNode.Leaf(WorkspaceNodeId(s"editor-${paneId.value}"), paneId)

    def build(paneId: PaneId, remaining: List[PaneId]): WorkspaceNode =
      remaining match
        case Nil => leaf(paneId)
        case next :: tail =>
          WorkspaceNode.Split(
            WorkspaceNodeId(s"split-${paneId.value}-${remaining.map(_.value).mkString("-")}"),
            SplitAxis.Horizontal,
            1.0 / (remaining.size + 1),
            leaf(paneId),
            build(next, tail)
          )

    WorkspaceTree(build(paneIds.head, paneIds.tail))
