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

object WorkspaceTree:

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
