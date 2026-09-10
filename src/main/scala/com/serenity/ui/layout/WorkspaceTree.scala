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

/** One persistent node in the editor workspace tree. */
sealed trait WorkspaceNode:
  def id: WorkspaceNodeId
  def paneIds: List[PaneId]
  def dockedSurfaceIds: List[SurfaceId]
  def nodeIds: List[WorkspaceNodeId]
  def axis: Option[SplitAxis]

object WorkspaceNode:

  /** An editor pane occupying one workspace leaf. */
  final case class Leaf(id: WorkspaceNodeId, paneId: PaneId) extends WorkspaceNode:
    val paneIds: List[PaneId]             = List(paneId)
    val dockedSurfaceIds: List[SurfaceId] = Nil
    val nodeIds: List[WorkspaceNodeId]    = List(id)
    val axis: Option[SplitAxis]           = None

  /** A pinned UI surface occupying one workspace leaf. */
  final case class DockedSurface(id: WorkspaceNodeId, surfaceId: SurfaceId, position: PanelPosition)
      extends WorkspaceNode:
    val paneIds: List[PaneId]             = Nil
    val dockedSurfaceIds: List[SurfaceId] = List(surfaceId)
    val nodeIds: List[WorkspaceNodeId]    = List(id)
    val axis: Option[SplitAxis]           = None

  /** A ratio-controlled binary split of two workspace branches. */
  final case class Split(
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
final case class WorkspaceTree(root: WorkspaceNode):
  def paneIds: List[PaneId]             = root.paneIds
  def dockedSurfaceIds: List[SurfaceId] = root.dockedSurfaceIds
  def nodeIds: List[WorkspaceNodeId]    = root.nodeIds

  def positionForSurface(surfaceId: SurfaceId): Option[PanelPosition] =
    WorkspaceTree.dockedSurface(root, surfaceId).map(_.position)

  /** Fresh, collision-free split/leaf node IDs for docking a not-yet-docked surface (issue #817) -- the one place every
    * pin path (`PanelStateReducer`, `UiPreset`, `SessionLayout`, the companion sprite panel) derives the IDs it hands
    * to [[dock]], so they can't drift out of sync with each other.
    */
  def nextDockIds(surfaceId: SurfaceId): (WorkspaceNodeId, WorkspaceNodeId) =
    (WorkspaceNodeId(s"dock-split-${surfaceId.value}-${nodeIds.size}"), WorkspaceNodeId(s"dock-${surfaceId.value}"))

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

  /** Docks `surfaceId` at `position` with the given absolute `size` (converted to a ratio against `viewportSize`, issue
    * #817) -- the one place every pin path performs both steps together, so they can't drift apart.
    *
    * `dock`'s "no panel at this edge yet" branch wraps the *entire* existing tree as one side of a brand-new split,
    * nesting one level deeper whatever was already docked at the opposite edge of the same axis (Left/Right, or
    * Top/Bottom) -- which would otherwise silently shrink that surface's rendered extent even though nothing asked to
    * resize it. This re-seeds the opposite surface's ratio from its own pre-dock absolute size (via [[currentSize]] /
    * [[allocationRatio]], which are ancestor-aware) after the new dock, so docking a second panel on the other side
    * never moves a first one's size, however deep the resulting nesting.
    */
  def dockSized(
    surfaceId: SurfaceId,
    position: PanelPosition,
    splitId: WorkspaceNodeId,
    leafId: WorkspaceNodeId,
    size: Int,
    viewportSize: Option[ViewportSize]
  ): Option[WorkspaceTree] =
    val opposite = WorkspaceTree.oppositePosition(position)
    val opposingSurface = dockedSurfaceIds
      .find(id => positionForSurface(id).contains(opposite))
      .flatMap(id => currentSize(id, viewportSize).map(id -> _))
    for
      docked  <- dock(surfaceId, position, splitId, leafId)
      ratio   <- docked.allocationRatio(surfaceId, size, viewportSize)
      resized <- docked.resizeSurface(surfaceId, ratio)
      finalTree <- opposingSurface match
        case Some((oppositeId, oppositeSize)) =>
          resized.allocationRatio(oppositeId, oppositeSize, viewportSize).flatMap(resized.resizeSurface(oppositeId, _))
        case None => Some(resized)
    yield finalTree

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

  /** The docked surface's current allocation, read back from the same owning split [[resizeSurface]] writes -- the sole
    * size record for a docked surface (issue #817), so callers needing "how big is this panel right now" (e.g. a
    * command/keyboard resize delta) read it here rather than from any size carried on the surface itself.
    */
  def ratioForSurface(surfaceId: SurfaceId): Option[Double] =
    WorkspaceTree.ratioForDockedSurface(root, surfaceId)

  /** `surfaceId`'s current absolute size (in cells), read back through its owning split's ratio against the extent that
    * split actually receives in *this* tree -- not `viewportSize` directly, since nesting under another docked
    * surface's split (issue #817's `dockSized` compensation, or simply two panels sharing an axis) leaves less than the
    * full viewport to divide. Inverse of [[allocationRatio]].
    */
  def currentSize(surfaceId: SurfaceId, viewportSize: Option[ViewportSize]): Option[Int] =
    for
      position <- positionForSurface(surfaceId)
      ratio    <- ratioForSurface(surfaceId)
      extent <- WorkspaceTree.availableExtentForSurface(
        root,
        surfaceId,
        WorkspaceTree.axisFor(position),
        WorkspaceTree.totalExtent(position, viewportSize)
      )
    yield math.round(ratio * extent).toInt

  /** The ratio `surfaceId`'s owning split needs so the surface renders at `requestedSize` cells, given the extent that
    * split actually receives in *this* tree (see [[currentSize]]). `surfaceId` must already be docked -- [[dockSized]]
    * calls this only after placing the surface, so the extent reflects its real ancestor chain.
    */
  def allocationRatio(surfaceId: SurfaceId, requestedSize: Int, viewportSize: Option[ViewportSize]): Option[Double] =
    for
      position <- positionForSurface(surfaceId)
      extent <- WorkspaceTree.availableExtentForSurface(
        root,
        surfaceId,
        WorkspaceTree.axisFor(position),
        WorkspaceTree.totalExtent(position, viewportSize)
      )
    yield requestedSize.toDouble / extent.max(1)

  /** Rearranges the docked surfaces at one edge into `desiredOrder`, preserving every other branch -- the tree encodes
    * same-edge order via nesting (issue #817: `dock`'s "existing panel at this edge" branch always nests the newly
    * docked surface after the ones already there), so reordering redocks each surface in turn via [[moveSurface]], the
    * same primitive this tree already uses to relocate a surface to a different edge. An id in `desiredOrder` not
    * currently docked at `position` is skipped, leaving the tree unchanged for that entry rather than aborting the
    * whole reorder.
    */
  def reorderAt(position: PanelPosition, desiredOrder: List[SurfaceId]): WorkspaceTree =
    desiredOrder.foldLeft(this) { (tree, surfaceId) =>
      if tree.positionForSurface(surfaceId).contains(position) then
        val (splitId, _) = tree.nextDockIds(surfaceId)
        tree.moveSurface(surfaceId, position, splitId).getOrElse(tree)
      else tree
    }

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

  /** Assumed total-cells extent used to convert an absolute docked-panel size when no real viewport is known yet
    * (matches every call site's own prior fallback).
    */
  private val AssumedViewportExtent = 100

  /** Grows and shrinks `tree` to contain exactly `targetPaneIds`, for callers (`UiPreset.resizeEditorPanes`) that
    * decide a whole target pane set at once rather than adding or removing one pane at a time. New panes are spliced in
    * before dropped ones are removed, so `.remove` never targets the tree's last remaining leaf when the target set is
    * otherwise disjoint from the current one; each new pane splits off the most recently added leaf (or seeds a fresh
    * single-leaf tree when `tree` is `None`, i.e. no panes existed yet).
    */
  def withExactPanes(tree: Option[WorkspaceTree], targetPaneIds: List[PaneId]): Option[WorkspaceTree] =
    val currentPaneIds = tree.map(_.paneIds).getOrElse(Nil)
    val newPaneIds     = targetPaneIds.filterNot(currentPaneIds.contains)
    val droppedPaneIds = currentPaneIds.filterNot(targetPaneIds.contains)

    def leaf(paneId: PaneId): WorkspaceTree = WorkspaceTree(
      WorkspaceNode.Leaf(WorkspaceNodeId(s"editor-${paneId.value}"), paneId)
    )

    val withNewPanes = newPaneIds.foldLeft(tree) { (acc, newPaneId) =>
      acc.flatMap(_.paneIds.lastOption) match
        case Some(anchor) =>
          acc
            .flatMap(
              _.split(
                anchor,
                newPaneId,
                SplitAxis.Horizontal,
                WorkspaceNodeId(s"resize-split-${anchor.value}-${newPaneId.value}"),
                WorkspaceNodeId(s"editor-${newPaneId.value}")
              )
            )
            .orElse(Some(leaf(newPaneId)))
        case None => Some(leaf(newPaneId))
    }
    droppedPaneIds.foldLeft(withNewPanes)((acc, droppedId) => acc.flatMap(_.remove(droppedId)).orElse(acc))

  /** The other edge on the same axis (Left/Right, or Top/Bottom) -- the one whose docked surface, if any, `dockSized`
    * re-seeds when a new dock at `position` would otherwise nest it (and shrink its rendered extent) unasked.
    */
  def oppositePosition(position: PanelPosition): PanelPosition =
    position match
      case PanelPosition.Left   => PanelPosition.Right
      case PanelPosition.Right  => PanelPosition.Left
      case PanelPosition.Top    => PanelPosition.Bottom
      case PanelPosition.Bottom => PanelPosition.Top

  /** The axis a docked panel's position divides along -- Left/Right split width, Top/Bottom split height. */
  private def axisFor(position: PanelPosition): SplitAxis =
    position match
      case PanelPosition.Left | PanelPosition.Right => SplitAxis.Horizontal
      case PanelPosition.Top | PanelPosition.Bottom => SplitAxis.Vertical

  /** The full viewport extent along `position`'s axis, falling back to [[AssumedViewportExtent]] before any real
    * viewport is known.
    */
  private def totalExtent(position: PanelPosition, viewportSize: Option[ViewportSize]): Int =
    viewportSize.fold(AssumedViewportExtent) { viewport =>
      position match
        case PanelPosition.Left | PanelPosition.Right => viewport.width
        case PanelPosition.Top | PanelPosition.Bottom => viewport.height
    }

  /** The extent actually available to the split [[resizeDockedSurface]] would update for `surfaceId` -- `extent`
    * reduced by every same-`axis` ancestor split's fraction on the path from `node` down to that split (a
    * different-axis ancestor divides the other dimension, so it passes `extent` through unchanged). This is what
    * [[WorkspaceTree.currentSize]] and [[WorkspaceTree.allocationRatio]] divide by instead of the raw viewport, so a
    * docked surface's absolute size round-trips correctly regardless of how deeply it ends up nested.
    *
    * Mirrors [[resizeDockedSurface]]'s own match order exactly -- the split it updates for `surfaceId` is the outermost
    * one with `surfaceId` somewhere in one branch and an editor pane in the other, not necessarily `surfaceId`'s direct
    * parent: two panels stacked on the same edge (`dock`'s "existing panel at this position" branch) nest the second
    * surface's own leaf under an inner split with no editor-pane branch at all, so `resizeDockedSurface` -- and this --
    * skip past it to the split one level up.
    */
  private def availableExtentForSurface(
    node: WorkspaceNode,
    surfaceId: SurfaceId,
    axis: SplitAxis,
    extent: Int
  ): Option[Int] =
    node match
      case _: WorkspaceNode.Leaf | _: WorkspaceNode.DockedSurface =>
        None
      case split: WorkspaceNode.Split
          if split.first.dockedSurfaceIds.contains(surfaceId) && split.second.paneIds.nonEmpty =>
        Some(extent)
      case split: WorkspaceNode.Split
          if split.second.dockedSurfaceIds.contains(surfaceId) && split.first.paneIds.nonEmpty =>
        Some(extent)
      case split: WorkspaceNode.Split =>
        val firstExtent  = if split.splitAxis == axis then math.round(extent * split.ratio).toInt else extent
        val secondExtent = if split.splitAxis == axis then extent - firstExtent else extent
        availableExtentForSurface(split.first, surfaceId, axis, firstExtent)
          .orElse(availableExtentForSurface(split.second, surfaceId, axis, secondExtent))

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

  private def ratioForDockedSurface(node: WorkspaceNode, surfaceId: SurfaceId): Option[Double] =
    node match
      case _: WorkspaceNode.Leaf | _: WorkspaceNode.DockedSurface =>
        None
      case split: WorkspaceNode.Split
          if split.first.dockedSurfaceIds.contains(surfaceId) && split.second.paneIds.nonEmpty =>
        Some(split.ratio)
      case split: WorkspaceNode.Split
          if split.second.dockedSurfaceIds.contains(surfaceId) && split.first.paneIds.nonEmpty =>
        Some(1.0 - split.ratio)
      case split: WorkspaceNode.Split =>
        ratioForDockedSurface(split.first, surfaceId).orElse(ratioForDockedSurface(split.second, surfaceId))

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
