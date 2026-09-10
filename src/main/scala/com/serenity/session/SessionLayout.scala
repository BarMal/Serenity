package com.serenity.session

import com.serenity.state.models.*
import com.serenity.ui.layout.{
  Layout,
  SessionDockedPanel,
  SessionWorkspaceNode,
  WorkspaceNodeId,
  WorkspaceTree
}

/** Persistent layout information
  */
final case class SessionLayout(
    editorPanes: List[SessionEditorPane],
    activeEditorPaneId: Option[Int],
    workspaceTree: Option[SessionWorkspaceNode] = None,
    maximizedWorkspaceNodeId: Option[String] = None,
    dockedPanels: List[SessionDockedPanel] = Nil
)

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

  final case class Restored(layout: Layout, surfaces: List[UiSurface], nextSurfaceId: Int)

  def fromAppState(state: AppState): SessionLayout =
    val dockedPanels  = SessionDockedPanel.captureFrom(state)
    val workspaceTree = SessionWorkspaceNode.captureFrom(state, dockedPanels)

    fromLayout(state.persisted.layout).copy(
      workspaceTree = workspaceTree,
      maximizedWorkspaceNodeId = SessionWorkspaceNode.captureMaximizedNodeId(state, workspaceTree),
      dockedPanels = dockedPanels
    )

  def fromLayout(layout: Layout): SessionLayout =
    SessionLayout(
      editorPanes = orderedPanes(layout).map(SessionEditorPane.fromEditorPane),
      activeEditorPaneId = layout.activeEditorPaneId.map(_.value)
    )

  private def orderedPanes(layout: Layout): List[EditorPane] =
    val orderedIds = layout.orderedPaneIds.filter(layout.editorPanes.contains)
    val missingIds = layout.editorPanes.keys.toList
      .filterNot(orderedIds.toSet)
      .sortBy(_.value)

    (orderedIds ++ missingIds).flatMap(layout.editorPanes.get)

  def toLayout(sessionLayout: SessionLayout): Layout =
    restore(sessionLayout).layout

  def restore(
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
    val orderedPaneIds =
      val requested = sessionLayout.editorPanes.map(pane => PaneId(pane.id)).filter(editorPanes.contains)
      requested ++ editorPanes.keys.toList.filterNot(requested.contains).sortBy(_.value)
    val surfaces = sessionLayout.dockedPanels.foldLeft(List.empty[UiSurface]) { (restored, persisted) =>
      val surface = persisted.panel.toUiSurface(SurfaceId(persisted.surfaceId))
      if restored.exists(_.id == surface.id) then restored else restored :+ surface
    }
    val pinnedSurfaceIds = surfaces.map(_.id).toSet
    val decodedTree = sessionLayout.workspaceTree
      .flatMap(SessionWorkspaceNode.toWorkspaceNode)
      .map(WorkspaceTree.apply)
      .filter(_.validationErrors(editorPanes.keySet, pinnedSurfaceIds).isEmpty)
    val fallbackTree =
      SessionDockedPanel.fallbackWorkspaceTree(orderedPaneIds, sessionLayout.dockedPanels)
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
      workspaceTree = workspaceTree,
      maximizedWorkspaceNodeId = maximized
    )
    Restored(layout, surfaces, nextSurfaceId(surfaces))

  /** Next unallocated surface id, high enough to avoid colliding with any of `surfaces`' own ids -- shared by session
    * restore and UI preset apply (`UiPreset.applyToState`), both of which restore panels keyed by their persisted
    * surface id rather than allocating fresh ones.
    */
  def nextSurfaceId(surfaces: List[UiSurface]): Int =
    surfaces
      .flatMap { surface =>
        Option
          .when(surface.id.value.startsWith("surface-"))(surface.id.value.stripPrefix("surface-"))
          .flatMap(_.toIntOption)
      }
      .maxOption
      .getOrElse(-1) + 1

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
      case Focus.Modal              => None // Modal dialogs (#814) are transient, never persisted

  def toFocus(sessionFocus: SessionFocus): Focus =
    sessionFocus match
      case SessionFocus.EditorPane(paneId) => Focus.EditorPane(PaneId(paneId))
