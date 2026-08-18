package com.serenity.ui.layout

import com.serenity.state.models.{EditorPane, PaneId}

enum PaneSplitDirection:
  case Horizontal
  case Vertical

object PaneSplitDirection:

  def fromString(value: String): PaneSplitDirection =
    value match
      case "Vertical" => Vertical
      case _          => Horizontal

final case class Layout(
    editorPanes: Map[PaneId, EditorPane],
    activeEditorPaneId: Option[PaneId],
    paneOrder: List[PaneId] = Nil,
    splitDirection: PaneSplitDirection = PaneSplitDirection.Horizontal,
    workspaceTree: Option[WorkspaceTree] = None,
    maximizedWorkspaceNodeId: Option[WorkspaceNodeId] = None
):

  def orderedPaneIds: List[PaneId] =
    treeCoveringCurrentPanes.map(_.paneIds).getOrElse(paneOrderOrSorted)

  /** Uses the explicit workspace tree when present and current, otherwise adapts the legacy flat pane model in memory.
    */
  def effectiveWorkspaceTree: Option[WorkspaceTree] =
    treeCoveringCurrentPanes.orElse(WorkspaceTree.fromLegacy(paneOrderOrSorted, splitDirection))

  // A stored tree that predates a direct state update (e.g. a raw pane addition) can omit panes the
  // update just added. Trust it only while it still accounts for every current editor pane.
  private def treeCoveringCurrentPanes: Option[WorkspaceTree] =
    workspaceTree.filter(tree => editorPanes.keySet.subsetOf(tree.paneIds.toSet))

  private def paneOrderOrSorted: List[PaneId] =
    if paneOrder.nonEmpty then paneOrder
    else editorPanes.keys.toList.sortBy(_.value)

object Layout:

  def initial: Layout =
    val initialPane = EditorPane.empty(PaneId(0))
    Layout(
      editorPanes = Map(PaneId(0) -> initialPane),
      activeEditorPaneId = Some(PaneId(0)),
      paneOrder = List(PaneId(0)),
      workspaceTree = Some(WorkspaceTree(WorkspaceNode.Leaf(WorkspaceNodeId("editor-0"), PaneId(0))))
    )

  def empty: Layout =
    Layout(
      editorPanes = Map.empty,
      activeEditorPaneId = None,
      paneOrder = Nil
    )
