package com.serenity.ui.layout

import com.serenity.state.models.{PaneId, SurfaceId}

/** Stable identity for a workspace node across geometry, focus, resizing, and persistence. */
opaque type WorkspaceNodeId = String

object WorkspaceNodeId:
  def apply(value: String): WorkspaceNodeId = value

  extension (id: WorkspaceNodeId) def value: String = id

/** Axis used to divide a workspace rectangle between two child nodes. */
enum SplitAxis:
  case Horizontal
  case Vertical

object SplitAxis:

  def fromLegacy(direction: PaneSplitDirection): SplitAxis =
    direction match
      case PaneSplitDirection.Horizontal => Horizontal
      case PaneSplitDirection.Vertical   => Vertical

/** One persistent node in the editor workspace tree. */
sealed trait WorkspaceNode:
  def id: WorkspaceNodeId
  def paneIds: List[PaneId]
  def dockedSurfaceIds: List[SurfaceId]
  def nodeIds: List[WorkspaceNodeId]
  def axis: Option[SplitAxis]

object WorkspaceNode:

  /** An editor pane occupying one workspace leaf. */
  case class Leaf(id: WorkspaceNodeId, paneId: PaneId) extends WorkspaceNode:
    val paneIds: List[PaneId]             = List(paneId)
    val dockedSurfaceIds: List[SurfaceId] = Nil
    val nodeIds: List[WorkspaceNodeId]    = List(id)
    val axis: Option[SplitAxis]           = None

  /** A pinned UI surface occupying one workspace leaf. */
  case class DockedSurface(id: WorkspaceNodeId, surfaceId: SurfaceId, position: PanelPosition) extends WorkspaceNode:
    val paneIds: List[PaneId]             = Nil
    val dockedSurfaceIds: List[SurfaceId] = List(surfaceId)
    val nodeIds: List[WorkspaceNodeId]    = List(id)
    val axis: Option[SplitAxis]           = None

  /** A ratio-controlled binary split of two workspace branches. */
  case class Split(
      id: WorkspaceNodeId,
      splitAxis: SplitAxis,
      ratio: Double,
      first: WorkspaceNode,
      second: WorkspaceNode
  ) extends WorkspaceNode:
    val paneIds: List[PaneId]             = first.paneIds ++ second.paneIds
    val dockedSurfaceIds: List[SurfaceId] = first.dockedSurfaceIds ++ second.dockedSurfaceIds
    val nodeIds: List[WorkspaceNodeId]    = id :: (first.nodeIds ++ second.nodeIds)
    val axis: Option[SplitAxis]           = Some(splitAxis)

/** Persistent binary composition of editor-pane workspace leaves. */
case class WorkspaceTree(root: WorkspaceNode):
  def paneIds: List[PaneId]             = root.paneIds
  def dockedSurfaceIds: List[SurfaceId] = root.dockedSurfaceIds
  def nodeIds: List[WorkspaceNodeId]    = root.nodeIds

  def positionForSurface(surfaceId: SurfaceId): Option[PanelPosition] =
    WorkspaceTree.dockedSurface(root, surfaceId).map(_.position)

  /** Replaces one editor leaf with a ratio-controlled split containing the original and new pane. */
  def split(
    paneId: PaneId,
    newPaneId: PaneId,
    axis: SplitAxis,
    splitId: WorkspaceNodeId,
    leafId: WorkspaceNodeId
  ): Option[WorkspaceTree] =
    if paneIds.contains(newPaneId) || nodeIds.contains(splitId) || nodeIds.contains(leafId) then None
    else
      WorkspaceTree
        .replaceLeaf(root, paneId) { existing =>
          WorkspaceNode.Split(
            splitId,
            axis,
            WorkspaceTree.DefaultSplitRatio,
            existing,
            WorkspaceNode.Leaf(leafId, newPaneId)
          )
        }
        .map(WorkspaceTree.apply)

  /** Removes one editor leaf and collapses its parent, returning `None` for the final leaf. */
  def remove(paneId: PaneId): Option[WorkspaceTree] =
    WorkspaceTree.removeLeaf(root, paneId).map(WorkspaceTree.apply)

  /** Inserts a docked surface at one workspace edge while preserving all existing branches. */
  def dock(
    surfaceId: SurfaceId,
    position: PanelPosition,
    splitId: WorkspaceNodeId,
    leafId: WorkspaceNodeId
  ): Option[WorkspaceTree] =
    if dockedSurfaceIds.contains(surfaceId) || nodeIds.contains(splitId) || nodeIds.contains(leafId) then None
    else
      val surface = WorkspaceNode.DockedSurface(leafId, surfaceId, position)
      WorkspaceTree.lastDockedSurfaceAt(root, position) match
        case Some(existing) =>
          val axis =
            position match
              case PanelPosition.Left | PanelPosition.Right => SplitAxis.Vertical
              case PanelPosition.Top | PanelPosition.Bottom => SplitAxis.Horizontal
          WorkspaceTree
            .replaceDockedSurface(root, existing.surfaceId)(
              WorkspaceNode.Split(splitId, axis, WorkspaceTree.DefaultSplitRatio, existing, surface)
            )
            .map(WorkspaceTree.apply)
        case None =>
          val (axis, ratio, first, second) =
            position match
              case PanelPosition.Left =>
                (SplitAxis.Horizontal, WorkspaceTree.DefaultDockRatio, surface, root)
              case PanelPosition.Right =>
                (SplitAxis.Horizontal, 1.0 - WorkspaceTree.DefaultDockRatio, root, surface)
              case PanelPosition.Top =>
                (SplitAxis.Vertical, WorkspaceTree.DefaultDockRatio, surface, root)
              case PanelPosition.Bottom =>
                (SplitAxis.Vertical, 1.0 - WorkspaceTree.DefaultDockRatio, root, surface)
          Some(WorkspaceTree(WorkspaceNode.Split(splitId, axis, ratio, first, second)))

  /** Removes one docked surface and collapses its now-redundant parent. */
  def removeSurface(surfaceId: SurfaceId): Option[WorkspaceTree] =
    Option.when(dockedSurfaceIds.contains(surfaceId))(
      WorkspaceTree(WorkspaceTree.removeDockedSurface(root, surfaceId).getOrElse(root))
    )

  /** Moves a docked surface to another edge without changing its stable leaf identity. */
  def moveSurface(
    surfaceId: SurfaceId,
    position: PanelPosition,
    splitId: WorkspaceNodeId
  ): Option[WorkspaceTree] =
    for
      leafId  <- nodeIdForSurface(surfaceId)
      removed <- removeSurface(surfaceId)
      moved   <- removed.dock(surfaceId, position, splitId, leafId)
    yield moved

  /** Updates the owning split so the docked surface receives the requested allocation. */
  def resizeSurface(surfaceId: SurfaceId, ratio: Double): Option[WorkspaceTree] =
    val normalized =
      if ratio.isFinite then ratio.max(WorkspaceTree.MinimumSplitRatio).min(WorkspaceTree.MaximumSplitRatio)
      else WorkspaceTree.DefaultDockRatio
    WorkspaceTree.resizeDockedSurface(root, surfaceId, normalized).map(WorkspaceTree.apply)

  def nodeIdForSurface(surfaceId: SurfaceId): Option[WorkspaceNodeId] =
    WorkspaceTree.dockedSurface(root, surfaceId).map(_.id)

  def surfaceIdForNode(nodeId: WorkspaceNodeId): Option[SurfaceId] =
    WorkspaceTree.dockedSurfaceByNodeId(root, nodeId).map(_.surfaceId)

  /** Returns the editor-only topology used by compatibility pane geometry during dock migration. */
  def editorRoot: Option[WorkspaceNode] =
    WorkspaceTree.withoutDockedSurfaces(root)

  /** Updates one split ratio while retaining a usable allocation for both branches. */
  def resize(splitId: WorkspaceNodeId, ratio: Double): Option[WorkspaceTree] =
    val normalizedRatio =
      if ratio.isFinite then ratio.max(WorkspaceTree.MinimumSplitRatio).min(WorkspaceTree.MaximumSplitRatio)
      else WorkspaceTree.DefaultSplitRatio

    WorkspaceTree
      .updateSplit(root, splitId, normalizedRatio)
      .map(WorkspaceTree.apply)

  /** Reports structural errors relative to the editor panes owned by the enclosing layout. */
  def validationErrors(editorPaneIds: Set[PaneId], pinnedSurfaceIds: Set[SurfaceId] = Set.empty): List[String] =
    val duplicateNodeIds    = WorkspaceTree.duplicates(nodeIds.map(_.value))
    val duplicatePaneIds    = WorkspaceTree.duplicates(paneIds.map(_.value))
    val duplicateSurfaceIds = WorkspaceTree.duplicates(dockedSurfaceIds.map(_.value))
    val treePaneIds         = paneIds.toSet
    val treeSurfaceIds      = dockedSurfaceIds.toSet
    val missingPanes        = (editorPaneIds -- treePaneIds).toList.sortBy(_.value)
    val unknownPanes        = (treePaneIds -- editorPaneIds).toList.sortBy(_.value)
    val missingSurfaces     = (pinnedSurfaceIds -- treeSurfaceIds).toList.sortBy(_.value)
    val unknownSurfaces     = (treeSurfaceIds -- pinnedSurfaceIds).toList.sortBy(_.value)

    List(
      Option.when(paneIds.isEmpty)("Workspace tree must contain at least one editor pane"),
      Option
        .when(duplicateNodeIds.nonEmpty)(
          s"Workspace tree contains duplicate node IDs: ${duplicateNodeIds.mkString(", ")}"
        ),
      Option
        .when(duplicatePaneIds.nonEmpty)(
          s"Workspace tree contains duplicate pane leaves: ${duplicatePaneIds.mkString(", ")}"
        ),
      Option
        .when(duplicateSurfaceIds.nonEmpty)(
          s"Workspace tree contains duplicate docked surfaces: ${duplicateSurfaceIds.mkString(", ")}"
        ),
      Option.when(missingPanes.nonEmpty)(
        s"Workspace tree is missing editor panes: ${missingPanes.map(_.value).mkString(", ")}"
      ),
      Option.when(unknownPanes.nonEmpty)(
        s"Workspace tree references non-existent editor panes: ${unknownPanes.map(_.value).mkString(", ")}"
      ),
      Option.when(missingSurfaces.nonEmpty)(
        s"Workspace tree is missing docked surfaces: ${missingSurfaces.map(_.value).mkString(", ")}"
      ),
      Option.when(unknownSurfaces.nonEmpty)(
        s"Workspace tree references non-existent docked surfaces: ${unknownSurfaces.map(_.value).mkString(", ")}"
      )
    ).flatten

object WorkspaceTree:

  val DefaultSplitRatio: Double = 0.5
  val MinimumSplitRatio: Double = 0.05
  val MaximumSplitRatio: Double = 0.95
  val DefaultDockRatio: Double  = 0.25

  private def replaceLeaf(
    node: WorkspaceNode,
    paneId: PaneId
  )(replace: WorkspaceNode.Leaf => WorkspaceNode): Option[WorkspaceNode] =
    node match
      case leaf: WorkspaceNode.Leaf =>
        Option.when(leaf.paneId == paneId)(replace(leaf))
      case _: WorkspaceNode.DockedSurface =>
        None
      case split: WorkspaceNode.Split =>
        replaceLeaf(split.first, paneId)(replace)
          .map(updated => split.copy(first = updated))
          .orElse(replaceLeaf(split.second, paneId)(replace).map(updated => split.copy(second = updated)))

  private def removeLeaf(node: WorkspaceNode, paneId: PaneId): Option[WorkspaceNode] =
    node match
      case leaf: WorkspaceNode.Leaf =>
        Option.when(leaf.paneId != paneId)(leaf)
      case docked: WorkspaceNode.DockedSurface =>
        Some(docked)
      case split: WorkspaceNode.Split =>
        if split.first.paneIds.contains(paneId) then
          removeLeaf(split.first, paneId).map(updated => split.copy(first = updated)).orElse(Some(split.second))
        else if split.second.paneIds.contains(paneId) then
          removeLeaf(split.second, paneId).map(updated => split.copy(second = updated)).orElse(Some(split.first))
        else Some(split)

  private def updateSplit(
    node: WorkspaceNode,
    splitId: WorkspaceNodeId,
    ratio: Double
  ): Option[WorkspaceNode] =
    node match
      case _: WorkspaceNode.Leaf =>
        None
      case _: WorkspaceNode.DockedSurface =>
        None
      case split: WorkspaceNode.Split if split.id == splitId =>
        Some(split.copy(ratio = ratio))
      case split: WorkspaceNode.Split =>
        updateSplit(split.first, splitId, ratio)
          .map(updated => split.copy(first = updated))
          .orElse(updateSplit(split.second, splitId, ratio).map(updated => split.copy(second = updated)))

  private def dockedSurface(node: WorkspaceNode, surfaceId: SurfaceId): Option[WorkspaceNode.DockedSurface] =
    node match
      case _: WorkspaceNode.Leaf =>
        None
      case docked: WorkspaceNode.DockedSurface =>
        Option.when(docked.surfaceId == surfaceId)(docked)
      case split: WorkspaceNode.Split =>
        dockedSurface(split.first, surfaceId).orElse(dockedSurface(split.second, surfaceId))

  private def dockedSurfaceByNodeId(
    node: WorkspaceNode,
    nodeId: WorkspaceNodeId
  ): Option[WorkspaceNode.DockedSurface] =
    node match
      case _: WorkspaceNode.Leaf =>
        None
      case docked: WorkspaceNode.DockedSurface =>
        Option.when(docked.id == nodeId)(docked)
      case split: WorkspaceNode.Split =>
        dockedSurfaceByNodeId(split.first, nodeId).orElse(dockedSurfaceByNodeId(split.second, nodeId))

  private def lastDockedSurfaceAt(
    node: WorkspaceNode,
    position: PanelPosition
  ): Option[WorkspaceNode.DockedSurface] =
    node match
      case _: WorkspaceNode.Leaf =>
        None
      case docked: WorkspaceNode.DockedSurface =>
        Option.when(docked.position == position)(docked)
      case split: WorkspaceNode.Split =>
        lastDockedSurfaceAt(split.second, position).orElse(lastDockedSurfaceAt(split.first, position))

  private def replaceDockedSurface(
    node: WorkspaceNode,
    surfaceId: SurfaceId
  )(replacement: WorkspaceNode): Option[WorkspaceNode] =
    node match
      case _: WorkspaceNode.Leaf =>
        None
      case docked: WorkspaceNode.DockedSurface =>
        Option.when(docked.surfaceId == surfaceId)(replacement)
      case split: WorkspaceNode.Split =>
        replaceDockedSurface(split.first, surfaceId)(replacement)
          .map(updated => split.copy(first = updated))
          .orElse(
            replaceDockedSurface(split.second, surfaceId)(replacement).map(updated => split.copy(second = updated))
          )

  private def removeDockedSurface(node: WorkspaceNode, surfaceId: SurfaceId): Option[WorkspaceNode] =
    node match
      case leaf: WorkspaceNode.Leaf =>
        Some(leaf)
      case docked: WorkspaceNode.DockedSurface =>
        Option.when(docked.surfaceId != surfaceId)(docked)
      case split: WorkspaceNode.Split =>
        if split.first.dockedSurfaceIds.contains(surfaceId) then
          removeDockedSurface(split.first, surfaceId)
            .map(updated => split.copy(first = updated))
            .orElse(Some(split.second))
        else if split.second.dockedSurfaceIds.contains(surfaceId) then
          removeDockedSurface(split.second, surfaceId)
            .map(updated => split.copy(second = updated))
            .orElse(Some(split.first))
        else Some(split)

  private def resizeDockedSurface(
    node: WorkspaceNode,
    surfaceId: SurfaceId,
    surfaceRatio: Double
  ): Option[WorkspaceNode] =
    node match
      case _: WorkspaceNode.Leaf | _: WorkspaceNode.DockedSurface =>
        None
      case split: WorkspaceNode.Split
          if split.first.dockedSurfaceIds.contains(surfaceId) && split.second.paneIds.nonEmpty =>
        Some(split.copy(ratio = surfaceRatio))
      case split: WorkspaceNode.Split
          if split.second.dockedSurfaceIds.contains(surfaceId) && split.first.paneIds.nonEmpty =>
        Some(split.copy(ratio = 1.0 - surfaceRatio))
      case split: WorkspaceNode.Split =>
        resizeDockedSurface(split.first, surfaceId, surfaceRatio)
          .map(updated => split.copy(first = updated))
          .orElse(
            resizeDockedSurface(split.second, surfaceId, surfaceRatio).map(updated => split.copy(second = updated))
          )

  private def withoutDockedSurfaces(node: WorkspaceNode): Option[WorkspaceNode] =
    node match
      case leaf: WorkspaceNode.Leaf =>
        Some(leaf)
      case _: WorkspaceNode.DockedSurface =>
        None
      case split: WorkspaceNode.Split =>
        (withoutDockedSurfaces(split.first), withoutDockedSurfaces(split.second)) match
          case (Some(first), Some(second)) => Some(split.copy(first = first, second = second))
          case (Some(first), None)         => Some(first)
          case (None, Some(second))        => Some(second)
          case (None, None)                => None

  private def duplicates[A](values: List[A]): List[A] =
    values
      .groupMapReduce(identity)(_ => 1)(_ + _)
      .collect { case (value, count) if count > 1 => value }
      .toList
      .sortBy(_.toString)

  /** Converts the legacy uniform pane strip into an equivalent nested binary tree. */
  def fromLegacy(paneIds: List[PaneId], direction: PaneSplitDirection): Option[WorkspaceTree] =
    def leaf(paneId: PaneId): WorkspaceNode =
      WorkspaceNode.Leaf(WorkspaceNodeId(s"editor-${paneId.value}"), paneId)

    def build(paneId: PaneId, remaining: List[PaneId]): WorkspaceNode =
      remaining match
        case Nil => leaf(paneId)
        case nextPaneId :: tail =>
          val ratio = 1.0 / (remaining.size + 1)
          WorkspaceNode.Split(
            WorkspaceNodeId(s"legacy-${paneId.value}-${remaining.map(_.value).mkString("-")}"),
            SplitAxis.fromLegacy(direction),
            ratio,
            leaf(paneId),
            build(nextPaneId, tail)
          )

    paneIds match
      case Nil           => None
      case first :: rest => Some(WorkspaceTree(build(first, rest)))
