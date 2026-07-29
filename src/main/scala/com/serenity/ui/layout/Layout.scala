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

case class Layout(
    editorPanes: Map[PaneId, EditorPane],
    activeEditorPaneId: Option[PaneId],
    paneOrder: List[PaneId] = Nil,
    splitDirection: PaneSplitDirection = PaneSplitDirection.Horizontal,
    workspaceTree: Option[WorkspaceTree] = None,
    maximizedWorkspaceNodeId: Option[WorkspaceNodeId] = None
):

  def orderedPaneIds: List[PaneId] =
    workspaceTree.map(_.paneIds).getOrElse {
      if paneOrder.nonEmpty then paneOrder
      else editorPanes.keys.toList.sortBy(_.value)
    }

  /** Uses the explicit workspace tree when present, otherwise adapts the legacy flat pane model in memory. */
  def effectiveWorkspaceTree: Option[WorkspaceTree] =
    workspaceTree.orElse(WorkspaceTree.fromLegacy(paneOrderOrSorted, splitDirection))

  private def paneOrderOrSorted: List[PaneId] =
    if paneOrder.nonEmpty then paneOrder
    else editorPanes.keys.toList.sortBy(_.value)

object Layout:

  def initial: Layout =
    val initialPane = EditorPane.empty(PaneId(0))
    Layout(
      editorPanes = Map(PaneId(0) -> initialPane),
      activeEditorPaneId = Some(PaneId(0)),
      paneOrder = List(PaneId(0))
    )

  def empty: Layout =
    Layout(
      editorPanes = Map.empty,
      activeEditorPaneId = None,
      paneOrder = Nil
    )
