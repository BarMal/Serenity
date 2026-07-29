package com.serenity.ui.layout

import com.serenity.state.models.PaneId

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
  def nodeIds: List[WorkspaceNodeId]
  def axis: Option[SplitAxis]

object WorkspaceNode:

  /** An editor pane occupying one workspace leaf. */
  case class Leaf(id: WorkspaceNodeId, paneId: PaneId) extends WorkspaceNode:
    val paneIds: List[PaneId]          = List(paneId)
    val nodeIds: List[WorkspaceNodeId] = List(id)
    val axis: Option[SplitAxis]        = None

  /** A ratio-controlled binary split of two workspace branches. */
  case class Split(
      id: WorkspaceNodeId,
      splitAxis: SplitAxis,
      ratio: Double,
      first: WorkspaceNode,
      second: WorkspaceNode
  ) extends WorkspaceNode:
    val paneIds: List[PaneId]          = first.paneIds ++ second.paneIds
    val nodeIds: List[WorkspaceNodeId] = id :: (first.nodeIds ++ second.nodeIds)
    val axis: Option[SplitAxis]        = Some(splitAxis)

/** Persistent binary composition of editor-pane workspace leaves. */
case class WorkspaceTree(root: WorkspaceNode):
  def paneIds: List[PaneId]          = root.paneIds
  def nodeIds: List[WorkspaceNodeId] = root.nodeIds

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

  /** Updates one split ratio while retaining a usable allocation for both branches. */
  def resize(splitId: WorkspaceNodeId, ratio: Double): Option[WorkspaceTree] =
    val normalizedRatio =
      if ratio.isFinite then ratio.max(WorkspaceTree.MinimumSplitRatio).min(WorkspaceTree.MaximumSplitRatio)
      else WorkspaceTree.DefaultSplitRatio

    WorkspaceTree
      .updateSplit(root, splitId, normalizedRatio)
      .map(WorkspaceTree.apply)

  /** Reports structural errors relative to the editor panes owned by the enclosing layout. */
  def validationErrors(editorPaneIds: Set[PaneId]): List[String] =
    val duplicateNodeIds = WorkspaceTree.duplicates(nodeIds.map(_.value))
    val duplicatePaneIds = WorkspaceTree.duplicates(paneIds.map(_.value))
    val treePaneIds      = paneIds.toSet
    val missingPanes     = (editorPaneIds -- treePaneIds).toList.sortBy(_.value)
    val unknownPanes     = (treePaneIds -- editorPaneIds).toList.sortBy(_.value)

    List(
      Option
        .when(duplicateNodeIds.nonEmpty)(
          s"Workspace tree contains duplicate node IDs: ${duplicateNodeIds.mkString(", ")}"
        ),
      Option
        .when(duplicatePaneIds.nonEmpty)(
          s"Workspace tree contains duplicate pane leaves: ${duplicatePaneIds.mkString(", ")}"
        ),
      Option.when(missingPanes.nonEmpty)(
        s"Workspace tree is missing editor panes: ${missingPanes.map(_.value).mkString(", ")}"
      ),
      Option.when(unknownPanes.nonEmpty)(
        s"Workspace tree references non-existent editor panes: ${unknownPanes.map(_.value).mkString(", ")}"
      )
    ).flatten

object WorkspaceTree:

  val DefaultSplitRatio: Double = 0.5
  val MinimumSplitRatio: Double = 0.05
  val MaximumSplitRatio: Double = 0.95

  private def replaceLeaf(
    node: WorkspaceNode,
    paneId: PaneId
  )(replace: WorkspaceNode.Leaf => WorkspaceNode): Option[WorkspaceNode] =
    node match
      case leaf: WorkspaceNode.Leaf =>
        Option.when(leaf.paneId == paneId)(replace(leaf))
      case split: WorkspaceNode.Split =>
        replaceLeaf(split.first, paneId)(replace)
          .map(updated => split.copy(first = updated))
          .orElse(replaceLeaf(split.second, paneId)(replace).map(updated => split.copy(second = updated)))

  private def removeLeaf(node: WorkspaceNode, paneId: PaneId): Option[WorkspaceNode] =
    node match
      case leaf: WorkspaceNode.Leaf =>
        Option.when(leaf.paneId != paneId)(leaf)
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
      case split: WorkspaceNode.Split if split.id == splitId =>
        Some(split.copy(ratio = ratio))
      case split: WorkspaceNode.Split =>
        updateSplit(split.first, splitId, ratio)
          .map(updated => split.copy(first = updated))
          .orElse(updateSplit(split.second, splitId, ratio).map(updated => split.copy(second = updated)))

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
