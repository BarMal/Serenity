package com.serenity.rope.btree

import com.serenity.rope.{Balance, Branch, Leaf, Rope}

/** Concatenation of two trees in time proportional to the difference in their heights.
  *
  * The shorter tree is attached on the matching side of the taller one at the depth where the heights agree, and any
  * node that overflows on the way back up is split in two. A node that is short of children, or a leaf that is short of
  * text, is merged into the neighbour it is attached to, so joining valid trees yields a valid tree.
  */
private[rope] object BTreeJoin:

  def concat(left: Rope, right: Rope)(using Balance): Rope =
    if left.weight == 0 then right
    else if right.weight == 0 then left
    else
      val parts =
        if left.height == right.height then joinSiblings(left, right)
        else if left.height > right.height then graftRight(left, right)
        else graftLeft(left, right)
      BTreeBuild.assemble(parts)

  /** Two nodes of one height as one valid node or two. */
  private def joinSiblings(left: Rope, right: Rope)(using balance: Balance): Vector[Rope] =
    (left, right) match
      case (a: Branch, b: Branch) =>
        if a.children.size >= Fanout.Min && b.children.size >= Fanout.Min then Vector(a, b)
        else BTreeBuild.group(a.children ++ b.children, Fanout.Max)
      case (a: Leaf, b: Leaf) => joinLeaves(a, b, LeafLimits.of(balance))
      case _                  => Vector(left, right)

  private def joinLeaves(left: Leaf, right: Leaf, limits: LeafLimits)(using Balance): Vector[Rope] =
    if left.value.isEmpty then Vector(right)
    else if right.value.isEmpty then Vector(left)
    else if left.value.length >= limits.min && right.value.length >= limits.min then Vector(left, right)
    else Chunking.pieces(left.value + right.value, limits).map(piece => Leaf(piece))

  /** `tall` with `small` appended at the end of its last descendant of `small`'s height. */
  private def graftRight(tall: Rope, small: Rope)(using Balance): Vector[Rope] =
    tall match
      case branch: Branch =>
        val children = branch.children
        val last     = children.size - 1
        val attached =
          if children(last).height > small.height then graftRight(children(last), small)
          else joinSiblings(children(last), small)
        BTreeBuild.group(children.take(last) ++ attached, Fanout.Max)
      case _ => Vector(tall, small)

  /** `tall` with `small` prepended at the start of its first descendant of `small`'s height. */
  private def graftLeft(small: Rope, tall: Rope)(using Balance): Vector[Rope] =
    tall match
      case branch: Branch =>
        val children = branch.children
        val attached =
          if children(0).height > small.height then graftLeft(small, children(0))
          else joinSiblings(small, children(0))
        BTreeBuild.group(attached ++ children.drop(1), Fanout.Max)
      case _ => Vector(small, tall)
