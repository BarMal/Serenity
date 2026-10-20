package com.serenity.rope.btree

import com.serenity.rope.{Balance, Branch, Leaf, Rope}

/** Cutting a tree at an offset by path copying, into both halves or into just one of them.
  *
  * The children either side of the cut are sibling subtrees of one height. Each side is those siblings joined under a
  * node of their own (short of children, which is fine for a root) and then the cut child's half is concatenated on,
  * which attaches it at the right depth and merges it into its neighbour if it is short.
  */
private[rope] object BTreeSplit:

  /** `index` must lie strictly inside the branch. */
  def split(branch: Branch, index: Int)(using Balance): (Rope, Rope) =
    val children = branch.children
    val child    = branch.childContaining(index)
    val within   = index - branch.starts(child)
    if within == 0 then (BTreeBuild.assemble(children.take(child)), BTreeBuild.assemble(children.drop(child)))
    else
      val (cutLeft, cutRight) = children(child).splitAt(within).getOrElse((children(child), Leaf("")))
      (
        BTreeJoin.concat(BTreeBuild.assemble(children.take(child)), cutLeft),
        BTreeJoin.concat(cutRight, BTreeBuild.assemble(children.drop(child + 1)))
      )

  /** The text before `index`, which must lie strictly inside the branch, without building the half after it. */
  def prefix(branch: Branch, index: Int)(using Balance): Rope =
    val children = branch.children
    val child    = branch.childContaining(index)
    val within   = index - branch.starts(child)
    val before   = BTreeBuild.assemble(children.take(child))
    if within == 0 then before else BTreeJoin.concat(before, children(child).prefixTo(within))

  /** The text from `index`, which must lie strictly inside the branch, without building the half before it. */
  def suffix(branch: Branch, index: Int)(using Balance): Rope =
    val children = branch.children
    val child    = branch.childContaining(index)
    val within   = index - branch.starts(child)
    if within == 0 then BTreeBuild.assemble(children.drop(child))
    else BTreeJoin.concat(children(child).suffixFrom(within), BTreeBuild.assemble(children.drop(child + 1)))
