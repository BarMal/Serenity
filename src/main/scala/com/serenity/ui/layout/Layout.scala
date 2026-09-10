package com.serenity.ui.layout

import com.serenity.state.models.{EditorPane, PaneId}

final case class Layout(
    editorPanes: Map[PaneId, EditorPane],
    activeEditorPaneId: Option[PaneId],
    workspaceTree: Option[WorkspaceTree] = None,
    maximizedWorkspaceNodeId: Option[WorkspaceNodeId] = None
):

  /** The current pane order, driven solely by `workspaceTree` -- `None` only when there are no editor panes yet
    * (e.g. `Layout.empty`, the transient pre-first-pane bootstrap state).
    */
  def orderedPaneIds: List[PaneId] =
    workspaceTree.map(_.paneIds).getOrElse(Nil)

object Layout:

  def initial: Layout =
    val initialPane = EditorPane.empty(PaneId(0))
    Layout(
      editorPanes = Map(PaneId(0) -> initialPane),
      activeEditorPaneId = Some(PaneId(0)),
      workspaceTree = Some(WorkspaceTree(WorkspaceNode.Leaf(WorkspaceNodeId("editor-0"), PaneId(0))))
    )

  def empty: Layout =
    Layout(
      editorPanes = Map.empty,
      activeEditorPaneId = None
    )
