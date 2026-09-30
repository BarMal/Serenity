package com.serenity.ui.widget

/** A tree node's children, which a tree may not have read yet. */
enum TreeChildren[+K, +A]:
  case Leaf
  case Unloaded
  case Loading
  case Loaded(nodes: Vector[TreeNode[K, A]])
  case Failed(reason: String)

final case class TreeNode[+K, +A](key: K, value: A, children: TreeChildren[K, A] = TreeChildren.Leaf):
  def isBranch: Boolean = children != TreeChildren.Leaf

/** One visible line of a tree: a node, how deep it sits, and whether it is expanded. */
final case class TreeRow[+K, +A](node: TreeNode[K, A], depth: Int, expanded: Boolean, parent: Option[K])

/** What a tree reports back after an input, beyond its own new state. */
enum TreeOutcome[+K, +A]:
  /** An unloaded branch was expanded: the caller reads its children and hands them to [[TreeView.withChildren]]. */
  case LoadChildren(key: K)
  case Activated(node: TreeNode[K, A])
  case Dismissed

/** A tree with the usual keyboard semantics: Right expands a branch or steps into it, Left collapses one or steps out
  * to its parent, and Up/Down/Page/Home/End move through the visible rows. Built on [[SelectableList]] over those rows,
  * so selection, scrolling, hover and clicks behave exactly as in any list.
  */
final case class TreeView[K, A](
    roots: Vector[TreeNode[K, A]],
    expanded: Set[K] = Set.empty[K],
    rows: SelectableList[TreeRow[K, A]] = SelectableList[TreeRow[K, A]](Vector.empty, endBehaviour = EndBehaviour.Stop)
):

  def selectedNode: Option[TreeNode[K, A]] = rows.selectedItem.map(_.node)

  def update(input: WidgetInput, visibleRows: Int): (TreeView[K, A], Option[TreeOutcome[K, A]]) =
    input match
      case WidgetInput.Right => rows.selectedItem.fold((this, None))(row => expandOrEnter(row, visibleRows))
      case WidgetInput.Left  => (rows.selectedItem.fold(this)(row => collapseOrLeave(row, visibleRows)), None)
      case WidgetInput.Activate =>
        rows.selectedItem.fold((this, None)) { row =>
          if row.node.isBranch then toggle(row, visibleRows)
          else (this, Some(TreeOutcome.Activated(row.node)))
        }
      case WidgetInput.Click(index, clicks) if clicks >= 2 =>
        val selected = copy(rows = rows.select(index, visibleRows))
        selected.update(WidgetInput.Activate, visibleRows)
      case other =>
        val (moved, outcome) = rows.update(other, visibleRows)
        (copy(rows = moved), outcome.collect { case ListOutcome.Dismissed => TreeOutcome.Dismissed })

  /** Hands the tree the children of `key`, once read (or why they could not be). */
  def withChildren(key: K, children: TreeChildren[K, A], visibleRows: Int): TreeView[K, A] =
    copy(roots = roots.map(replaced(_, key, children))).relaid(visibleRows)

  def withExpanded(key: K, isExpanded: Boolean, visibleRows: Int): TreeView[K, A] =
    copy(expanded = if isExpanded then expanded + key else expanded - key).relaid(visibleRows)

  private def expandOrEnter(row: TreeRow[K, A], visibleRows: Int): (TreeView[K, A], Option[TreeOutcome[K, A]]) =
    if !row.node.isBranch then (this, None)
    else if !row.expanded then expand(row, visibleRows)
    else
      row.node.children match
        case TreeChildren.Loaded(children) if children.nonEmpty =>
          (copy(rows = rows.moveBy(1, visibleRows)), None)
        case _ => (this, None)

  private def collapseOrLeave(row: TreeRow[K, A], visibleRows: Int): TreeView[K, A] =
    if row.expanded then withExpanded(row.node.key, isExpanded = false, visibleRows)
    else
      row.parent
        .map(parent => rows.items.indexWhere(_.node.key == parent))
        .filter(_ >= 0)
        .fold(this)(index => copy(rows = rows.select(index, visibleRows)))

  private def toggle(row: TreeRow[K, A], visibleRows: Int): (TreeView[K, A], Option[TreeOutcome[K, A]]) =
    if row.expanded then (withExpanded(row.node.key, isExpanded = false, visibleRows), None)
    else expand(row, visibleRows)

  private def expand(row: TreeRow[K, A], visibleRows: Int): (TreeView[K, A], Option[TreeOutcome[K, A]]) =
    row.node.children match
      case TreeChildren.Unloaded | TreeChildren.Failed(_) =>
        val loading = withChildren(row.node.key, TreeChildren.Loading, visibleRows)
        (
          loading.withExpanded(row.node.key, isExpanded = true, visibleRows),
          Some(TreeOutcome.LoadChildren(row.node.key))
        )
      case _ => (withExpanded(row.node.key, isExpanded = true, visibleRows), None)

  private def replaced(node: TreeNode[K, A], key: K, children: TreeChildren[K, A]): TreeNode[K, A] =
    if node.key == key then node.copy(children = children)
    else
      node.children match
        case TreeChildren.Loaded(nodes) =>
          node.copy(children = TreeChildren.Loaded(nodes.map(replaced(_, key, children))))
        case _ => node

  private def relaid(visibleRows: Int): TreeView[K, A] =
    copy(rows = rows.withItems(TreeView.flatten(roots, expanded), visibleRows)(_.node.key == _.node.key))

object TreeView:

  def of[K, A](roots: Seq[TreeNode[K, A]], expanded: Set[K] = Set.empty[K]): TreeView[K, A] =
    val flat = flatten(roots.toVector, expanded)
    TreeView(roots.toVector, expanded, SelectableList.of(flat, EndBehaviour.Stop))

  def flatten[K, A](roots: Vector[TreeNode[K, A]], expanded: Set[K]): Vector[TreeRow[K, A]] =
    def rowsOf(node: TreeNode[K, A], depth: Int, parent: Option[K]): Vector[TreeRow[K, A]] =
      val isExpanded = node.isBranch && expanded.contains(node.key)
      val children = node.children match
        case TreeChildren.Loaded(nodes) if isExpanded => nodes.flatMap(rowsOf(_, depth + 1, Some(node.key)))
        case _                                        => Vector.empty
      TreeRow(node, depth, isExpanded, parent) +: children
    roots.flatMap(rowsOf(_, 0, None))
