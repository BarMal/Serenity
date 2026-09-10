package com.serenity.ui.layout

import java.nio.file.Path

import com.serenity.state.models.{AppState, BufferId, SurfaceContent, SurfaceId, SurfacePresentation, UiSurface}

/** Persisted form of the [[PanelContent]] cases that can be pinned to a panel position. */
enum SessionPanelContent:
  case DirectoryTree(rootPath: String, selectedPath: Option[String], expandedPaths: List[String])
  case Terminal(buffer: String, cursor: Int)
  case Outline(symbols: List[Symbol])
  case Comments(symbols: List[Symbol])
  case Diagnostics(issues: List[Diagnostic])
  case MarkdownPreview(bufferId: Int, title: String)
  case CompanionSprite

  def toSurfaceContent: SurfaceContent =
    this match
      case DirectoryTree(rootPath, selectedPath, expandedPaths) =>
        SurfaceContent.DirectoryTree(
          DirectoryTreeData(
            rootPath = Path.of(rootPath),
            expandedPaths = expandedPaths.map(Path.of(_)).toSet,
            entries = Map.empty
          ),
          selectedPath.map(Path.of(_))
        )
      case Terminal(buffer, cursor) =>
        SurfaceContent.Terminal(buffer, cursor)
      case Outline(symbols) =>
        SurfaceContent.Outline(symbols)
      case Comments(symbols) =>
        SurfaceContent.Comments(symbols)
      case Diagnostics(issues) =>
        SurfaceContent.Diagnostics(issues)
      case MarkdownPreview(bufferId, title) =>
        SurfaceContent.MarkdownPreview(BufferId(bufferId), title)
      case CompanionSprite =>
        SurfaceContent.CompanionSprite

/** Persisted form of a docked panel's content and last-known placement, restored via [[toUiSurface]]. Shared by session
  * persistence and UI presets (issue #820) so both restore docked panels through the same snapshot model. Distinct from
  * the live [[PinnedPanel]] (which pairs a position with a resolved [[PanelContent]] for the running app, not a
  * persistence format) to avoid the two colliding in this package.
  */
final case class SessionPinnedPanel(
    position: PanelPosition,
    size: Int,
    content: SessionPanelContent
):

  def toUiSurface(id: SurfaceId): UiSurface =
    UiSurface(
      id = id,
      content = content.toSurfaceContent,
      presentation = SurfacePresentation.Docked
    )

object SessionPinnedPanel:

  /** Reads a docked surface's position and size back through the workspace tree (issue #817: the sole record of both),
    * rather than from any position/size carried on the surface itself.
    */
  def fromSurface(surface: UiSurface, state: AppState): Option[SessionPinnedPanel] =
    surface.presentation match
      case SurfacePresentation.Docked =>
        for
          tree     <- state.persisted.layout.workspaceTree
          position <- tree.positionForSurface(surface.id)
          size     <- tree.currentSize(surface.id, state.runtime.viewportSize)
          panel    <- fromSurfaceContent(surface.content, position, size)
        yield panel
      case _ =>
        None

  def fromPanelContent(content: PanelContent, position: PanelPosition, size: Int): Option[SessionPinnedPanel] =
    fromSurfaceContent(content.asSurfaceContent, position, size)

  private def fromSurfaceContent(
    content: SurfaceContent,
    position: PanelPosition,
    size: Int
  ): Option[SessionPinnedPanel] =
    PanelContent.fromSurfaceContent(content).map(panel => SessionPinnedPanel(position, size, toSnapshot(panel)))

  /** Persistence mapping from the pinnable content model to its saved form. Exhaustive over [[PanelContent]] with no
    * wildcard, so adding a new pinnable case is a compile error here until it is given an explicit persistence
    * decision.
    *
    * `activeLocation` on Outline, Comments, and Diagnostics tracks a live cursor/selection highlight; it has no meaning
    * across a save/restore, so the snapshot form deliberately does not carry it.
    */
  private def toSnapshot(content: PanelContent): SessionPanelContent =
    content match
      case PanelContent.DirectoryTree(tree, selectedPath) =>
        SessionPanelContent.DirectoryTree(
          rootPath = tree.rootPath.toString,
          selectedPath = selectedPath.map(_.toString),
          expandedPaths = tree.expandedPaths.toList.map(_.toString).sorted
        )
      case PanelContent.Terminal(buffer, cursor) =>
        SessionPanelContent.Terminal(buffer, cursor)
      case PanelContent.Outline(symbols, _) =>
        SessionPanelContent.Outline(symbols)
      case PanelContent.Comments(symbols, _) =>
        SessionPanelContent.Comments(symbols)
      case PanelContent.Diagnostics(issues, _) =>
        SessionPanelContent.Diagnostics(issues)
      case PanelContent.MarkdownPreview(bufferId, title) =>
        SessionPanelContent.MarkdownPreview(bufferId.value, title)
      case PanelContent.CompanionSprite =>
        SessionPanelContent.CompanionSprite

/** Versioned representation of one workspace-tree node, decoupled from live identifiers ([[WorkspaceNodeId]]/
  * [[com.serenity.state.models.PaneId]] wrapped as raw strings/ints) so it can be persisted and migrated independently
  * of [[WorkspaceNode]]. Shared by session persistence and UI presets (issue #820).
  */
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

object SessionWorkspaceNode:

  def fromWorkspaceNode(node: WorkspaceNode): SessionWorkspaceNode =
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

  def toWorkspaceNode(node: SessionWorkspaceNode): Option[WorkspaceNode] =
    node match
      case SessionWorkspaceNode.EditorLeaf(id, paneId) =>
        Some(WorkspaceNode.Leaf(WorkspaceNodeId(id), com.serenity.state.models.PaneId(paneId)))
      case SessionWorkspaceNode.DockedSurface(id, surfaceId, position) =>
        panelPosition(position).map(WorkspaceNode.DockedSurface(WorkspaceNodeId(id), SurfaceId(surfaceId), _))
      case SessionWorkspaceNode.Split(id, axis, ratio, first, second) =>
        for
          splitAxis  <- toSplitAxis(axis)
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

  /** Derives the persisted tree snapshot from live app state, restricted to the docked surfaces actually being
    * persisted alongside it (`dockedPanels`) -- shared by session persistence (`SessionLayout.fromAppState`) and UI
    * preset capture (`UiPreset.capture`) so both derive the same snapshot the same way. A stored tree referencing a
    * docked surface that isn't in `dockedPanels` (e.g. a surface whose content couldn't be captured) is dropped rather
    * than persisted half-consistent.
    */
  def captureFrom(state: AppState, dockedPanels: List[SessionDockedPanel]): Option[SessionWorkspaceNode] =
    val persistedSurfaceIds = dockedPanels.map(panel => SurfaceId(panel.surfaceId)).toSet
    state.persisted.layout.workspaceTree
      .filter(_.dockedSurfaceIds.toSet.subsetOf(persistedSurfaceIds))
      .map(tree => fromWorkspaceNode(tree.root))

  /** The captured tree's maximized node, dropped when it no longer resolves to a docked surface in the *live* tree (not
    * the captured snapshot) -- mirrors `Layout`'s own invariant that a maximized id always names a currently docked
    * surface.
    */
  def captureMaximizedNodeId(state: AppState, capturedTree: Option[SessionWorkspaceNode]): Option[String] =
    state.persisted.layout.maximizedWorkspaceNodeId
      .filter(nodeId =>
        capturedTree.nonEmpty && state.persisted.layout.workspaceTree.exists(_.nodeIds.contains(nodeId))
      )
      .map(_.value)

  private def panelPosition(value: String): Option[PanelPosition] =
    value match
      case "Left"   => Some(PanelPosition.Left)
      case "Right"  => Some(PanelPosition.Right)
      case "Top"    => Some(PanelPosition.Top)
      case "Bottom" => Some(PanelPosition.Bottom)
      case _        => None

  private def toSplitAxis(value: String): Option[SplitAxis] =
    value match
      case "Horizontal" => Some(SplitAxis.Horizontal)
      case "Vertical"   => Some(SplitAxis.Vertical)
      case _            => None

/** Persistable docked panel content keyed by the surface identity referenced from the workspace tree. */
final case class SessionDockedPanel(surfaceId: String, panel: SessionPinnedPanel)

object SessionDockedPanel:

  /** Derives the persisted docked-panel list from live app state (issue #817: the workspace tree is the sole record of
    * a docked surface's position/size) -- shared by session persistence and UI preset capture so both derive the same
    * snapshot the same way.
    */
  def captureFrom(state: AppState): List[SessionDockedPanel] =
    state.pinnedSurfaces.flatMap { surface =>
      SessionPinnedPanel.fromSurface(surface, state).map(SessionDockedPanel(surface.id.value, _))
    }

  /** Docks each persisted panel at its saved position onto the legacy pane-strip tree -- the shared fallback used when
    * a persisted tree is absent or fails validation, by both session restore and UI preset apply. Position lives on
    * `SessionDockedPanel`/`SessionPinnedPanel` itself, the persistence-format mirror of
    * `WorkspaceNode.DockedSurface.position`, not on the restored `UiSurface`, which no longer carries position at all
    * (issue #817).
    */
  def fallbackWorkspaceTree(
    paneIds: List[com.serenity.state.models.PaneId],
    splitDirection: PaneSplitDirection,
    dockedPanels: List[SessionDockedPanel]
  ): Option[WorkspaceTree] =
    dockedPanels.zipWithIndex.foldLeft(WorkspaceTree.fromLegacy(paneIds, splitDirection)) {
      case (Some(tree), (panel, index)) =>
        tree.dock(
          SurfaceId(panel.surfaceId),
          panel.panel.position,
          WorkspaceNodeId(s"restored-dock-split-$index"),
          WorkspaceNodeId(s"restored-dock-${panel.surfaceId}")
        )
      case (None, _) =>
        None
    }
