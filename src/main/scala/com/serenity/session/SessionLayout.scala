package com.serenity.session

import com.serenity.state.models.*
import com.serenity.ui.layout.{
  Layout,
  PaneSplitDirection,
  PanelPosition,
  SplitAxis,
  WorkspaceNode,
  WorkspaceNodeId,
  WorkspaceTree
}
import com.serenity.ui.presets.UiPreset
import com.serenity.ui.presets.UiPreset.given

/** Persistent layout information
  */
final case class SessionLayout(
    editorPanes: List[SessionEditorPane],
    activeEditorPaneId: Option[Int],
    paneOrder: List[Int] = Nil,
    splitDirection: String = PaneSplitDirection.Horizontal.toString,
    workspaceTree: Option[SessionWorkspaceNode] = None,
    maximizedWorkspaceNodeId: Option[String] = None,
    dockedPanels: List[SessionDockedPanel] = Nil
)

/** Versioned session representation of one workspace-tree node. */
enum SessionWorkspaceNode:
  case EditorLeaf(id: String, paneId: Int)
  case DockedSurface(id: String, surfaceId: String, position: String)

  case Split(
      id: String,
      axis: String,
      ratio: Double,
      first: SessionWorkspaceNode,
      second: SessionWorkspaceNode
  )

/** Persistable docked panel content keyed by the surface identity referenced from the workspace tree. */
final case class SessionDockedPanel(surfaceId: String, panel: UiPreset.PinnedPanel)

final case class SessionEditorPane(
    id: Int,
    bufferId: Option[Int]
)

/** Persistent focus state
  */
enum SessionFocus:
  case EditorPane(paneId: Int)
  // Note: We don't persist Surface focus as UI surfaces are not persistent

object SessionLayout:

  final private[session] case class Restored(layout: Layout, surfaces: List[UiSurface], nextSurfaceId: Int)

  def fromAppState(state: AppState): SessionLayout =
    val dockedPanels = state.pinnedSurfaces.flatMap { surface =>
      UiPreset.PinnedPanel.fromSurface(surface).map(SessionDockedPanel(surface.id.value, _))
    }
    val persistedSurfaceIds = dockedPanels.map(panel => SurfaceId(panel.surfaceId)).toSet
    val workspaceTree = state.persisted.layout.workspaceTree
      .filter(_.dockedSurfaceIds.toSet.subsetOf(persistedSurfaceIds))
      .map(tree => fromWorkspaceNode(tree.root))

    fromLayout(state.persisted.layout).copy(
      workspaceTree = workspaceTree,
      maximizedWorkspaceNodeId = state.persisted.layout.maximizedWorkspaceNodeId
        .filter(nodeId => workspaceTree.exists(_ => treeContainsNode(state, nodeId)))
        .map(_.value),
      dockedPanels = dockedPanels
    )

  def fromLayout(layout: Layout): SessionLayout =
    SessionLayout(
      editorPanes = orderedPanes(layout).map(SessionEditorPane.fromEditorPane),
      activeEditorPaneId = layout.activeEditorPaneId.map(_.value),
      paneOrder = layout.paneOrder.map(_.value),
      splitDirection = layout.splitDirection.toString
    )

  private def orderedPanes(layout: Layout): List[EditorPane] =
    val orderedIds = layout.orderedPaneIds.filter(layout.editorPanes.contains)
    val missingIds = layout.editorPanes.keys.toList
      .filterNot(orderedIds.toSet)
      .sortBy(_.value)

    (orderedIds ++ missingIds).flatMap(layout.editorPanes.get)

  def toLayout(sessionLayout: SessionLayout): Layout =
    restore(sessionLayout).layout

  private[session] def restore(
    sessionLayout: SessionLayout,
    validBufferIds: Option[Set[BufferId]] = None
  ): Restored =
    val decodedEditorPanes = sessionLayout.editorPanes.map { sessionPane =>
      val pane = SessionEditorPane
        .toEditorPane(sessionPane)
        .copy(bufferId = sessionPane.bufferId.map(BufferId.apply).filter(id => validBufferIds.forall(_.contains(id))))
      PaneId(sessionPane.id) -> pane
    }.toMap
    val editorPanes =
      if decodedEditorPanes.nonEmpty then decodedEditorPanes
      else Map(PaneId(0) -> EditorPane.empty(PaneId(0)))
    val splitDirection = PaneSplitDirection.fromString(sessionLayout.splitDirection)
    val orderedPaneIds =
      val requested = sessionLayout.paneOrder.map(PaneId.apply).filter(editorPanes.contains)
      requested ++ editorPanes.keys.toList.filterNot(requested.contains).sortBy(_.value)
    val surfaces = sessionLayout.dockedPanels.foldLeft(List.empty[UiSurface]) { (restored, persisted) =>
      val surface = persisted.panel.toUiSurface(SurfaceId(persisted.surfaceId))
      if restored.exists(_.id == surface.id) then restored else restored :+ surface
    }
    val pinnedSurfaceIds = surfaces.map(_.id).toSet
    val decodedTree = sessionLayout.workspaceTree
      .flatMap(toWorkspaceNode)
      .map(WorkspaceTree.apply)
      .filter(_.validationErrors(editorPanes.keySet, pinnedSurfaceIds).isEmpty)
    val fallbackTree  = fallbackWorkspaceTree(orderedPaneIds, splitDirection, surfaces)
    val workspaceTree = decodedTree.orElse(fallbackTree)
    val maximized = sessionLayout.maximizedWorkspaceNodeId
      .map(WorkspaceNodeId.apply)
      .filter(nodeId => workspaceTree.exists(_.surfaceIdForNode(nodeId).nonEmpty))
    val activeEditorPaneId = sessionLayout.activeEditorPaneId
      .map(PaneId.apply)
      .filter(editorPanes.contains)
      .orElse(orderedPaneIds.headOption)
    val layout = Layout(
      editorPanes = editorPanes,
      activeEditorPaneId = activeEditorPaneId,
      paneOrder = orderedPaneIds,
      splitDirection = splitDirection,
      workspaceTree = workspaceTree,
      maximizedWorkspaceNodeId = maximized
    )
    Restored(layout, surfaces, nextSurfaceId(surfaces))

  private def fromWorkspaceNode(node: WorkspaceNode): SessionWorkspaceNode =
    node match
      case WorkspaceNode.Leaf(id, paneId) =>
        SessionWorkspaceNode.EditorLeaf(id.value, paneId.value)
      case WorkspaceNode.DockedSurface(id, surfaceId, position) =>
        SessionWorkspaceNode.DockedSurface(id.value, surfaceId.value, position.toString)
      case WorkspaceNode.Split(id, axis, ratio, first, second) =>
        SessionWorkspaceNode.Split(
          id.value,
          axis.toString,
          ratio,
          fromWorkspaceNode(first),
          fromWorkspaceNode(second)
        )

  private def toWorkspaceNode(node: SessionWorkspaceNode): Option[WorkspaceNode] =
    node match
      case SessionWorkspaceNode.EditorLeaf(id, paneId) =>
        Some(WorkspaceNode.Leaf(WorkspaceNodeId(id), PaneId(paneId)))
      case SessionWorkspaceNode.DockedSurface(id, surfaceId, position) =>
        panelPosition(position).map(WorkspaceNode.DockedSurface(WorkspaceNodeId(id), SurfaceId(surfaceId), _))
      case SessionWorkspaceNode.Split(id, axis, ratio, first, second) =>
        for
          splitAxis  <- splitAxis(axis)
          _          <- Option.when(ratio.isFinite)(())
          firstNode  <- toWorkspaceNode(first)
          secondNode <- toWorkspaceNode(second)
        yield WorkspaceNode.Split(
          WorkspaceNodeId(id),
          splitAxis,
          ratio.max(WorkspaceTree.MinimumSplitRatio).min(WorkspaceTree.MaximumSplitRatio),
          firstNode,
          secondNode
        )

  private def fallbackWorkspaceTree(
    paneIds: List[PaneId],
    splitDirection: PaneSplitDirection,
    surfaces: List[UiSurface]
  ): Option[WorkspaceTree] =
    surfaces.zipWithIndex.foldLeft(WorkspaceTree.fromLegacy(paneIds, splitDirection)) {
      case (Some(tree), (surface, index)) =>
        surface.presentation match
          case SurfacePresentation.Pinned(position, _) =>
            tree.dock(
              surface.id,
              position,
              WorkspaceNodeId(s"restored-dock-split-$index"),
              WorkspaceNodeId(s"restored-dock-${surface.id.value}")
            )
          case _ =>
            Some(tree)
      case (None, _) =>
        None
    }

  private def panelPosition(value: String): Option[PanelPosition] =
    value match
      case "Left"   => Some(PanelPosition.Left)
      case "Right"  => Some(PanelPosition.Right)
      case "Top"    => Some(PanelPosition.Top)
      case "Bottom" => Some(PanelPosition.Bottom)
      case _        => None

  private def splitAxis(value: String): Option[SplitAxis] =
    value match
      case "Horizontal" => Some(SplitAxis.Horizontal)
      case "Vertical"   => Some(SplitAxis.Vertical)
      case _            => None

  private def nextSurfaceId(surfaces: List[UiSurface]): Int =
    surfaces
      .flatMap { surface =>
        Option
          .when(surface.id.value.startsWith("surface-"))(surface.id.value.stripPrefix("surface-"))
          .flatMap(_.toIntOption)
      }
      .maxOption
      .getOrElse(-1) + 1

  private def treeContainsNode(state: AppState, nodeId: WorkspaceNodeId): Boolean =
    state.persisted.layout.workspaceTree.exists(_.nodeIds.contains(nodeId))

object SessionEditorPane:

  def fromEditorPane(pane: EditorPane): SessionEditorPane =
    SessionEditorPane(
      id = pane.id.value,
      bufferId = pane.bufferId.map(_.value)
    )

  def toEditorPane(sessionPane: SessionEditorPane): EditorPane =
    EditorPane(
      id = PaneId(sessionPane.id),
      bufferId = sessionPane.bufferId.map(BufferId.apply),
      viewport = Viewport.default,
      cursors = List(CursorPosition(0, 0)),
      centerLine = 0
    )

object SessionFocus:

  def fromFocus(focus: Focus): Option[SessionFocus] =
    focus match
      case Focus.EditorPane(paneId) => Some(SessionFocus.EditorPane(paneId.value))
      case Focus.Surface(_)         => None // Don't persist surface focus

  def toFocus(sessionFocus: SessionFocus): Focus =
    sessionFocus match
      case SessionFocus.EditorPane(paneId) => Focus.EditorPane(PaneId(paneId))
