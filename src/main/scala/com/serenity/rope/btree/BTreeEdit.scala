package com.serenity.rope.btree

import com.serenity.rope.{Balance, Branch, Leaf, Rope}

/** Insertion and deletion by copying the path to the edited leaf and sharing every other node with the old tree.
  *
  * An edit inside one leaf touches one node per level. An edit that cannot stay inside one leaf, or would leave a leaf
  * short of text, goes through split and concat instead, which restore the node bounds locally rather than rebuilding
  * anything.
  */
private[rope] object BTreeEdit:

  /** `text` (already LF-only) inserted at `at`, which must be within `[0, rope.weight]`. */
  def insert(rope: Rope, at: Int, text: String)(using balance: Balance): Rope =
    val limits = LeafLimits.of(balance)
    if text.isEmpty then rope
    else if text.length > limits.max then insertLarge(rope, at, text)
    else BTreeBuild.assemble(insertInto(rope, at, text, limits))

  /** `[start, end)` removed, where `0 <= start <= end <= rope.weight`. */
  def deleteRange(rope: Rope, start: Int, end: Int)(using balance: Balance): Rope =
    if start >= end then rope
    else
      deleteWithinLeaf(rope, start, end, LeafLimits.of(balance), isRoot = true)
        .getOrElse(deleteBySplitting(rope, start, end))

  private def insertLarge(rope: Rope, at: Int, text: String)(using Balance): Rope =
    val (before, after) = rope.splitAt(at).getOrElse((rope, Leaf("")))
    BTreeJoin.concat(BTreeJoin.concat(before, BTreeBuild.fromText(text)), after)

  /** The nodes that replace `node` once `text` is inserted at `at` within it: one, or several when a leaf overflows. */
  private def insertInto(node: Rope, at: Int, text: String, limits: LeafLimits)(using Balance): Vector[Rope] =
    node match
      case leaf: Leaf =>
        val value = leaf.value
        Chunking.pieces(value.substring(0, at) + text + value.substring(at), limits).map(piece => Leaf(piece))
      case branch: Branch =>
        val child    = branch.childEndingAtOrAfter(at)
        val replaced = insertInto(branch.children(child), at - branch.starts(child), text, limits)
        val children =
          if replaced.size == 1 then branch.children.updated(child, replaced(0))
          else branch.children.patch(child, replaced, 1)
        BTreeBuild.group(children, Fanout.Max)

  /** The tree with `[start, end)` cut out of a single leaf, or `None` when the range spans leaves or would leave a leaf
    * (other than a lone root leaf) below its minimum.
    */
  private def deleteWithinLeaf(node: Rope, start: Int, end: Int, limits: LeafLimits, isRoot: Boolean)(using
    Balance
  ): Option[Rope] =
    node match
      case leaf: Leaf =>
        val value = leaf.value
        Option.when(isRoot || value.length - (end - start) >= limits.min)(
          Leaf(value.substring(0, start) + value.substring(end))
        )
      case branch: Branch =>
        val child = branch.childContaining(start)
        Option
          .when(end <= branch.starts(child + 1))(child)
          .flatMap { index =>
            deleteWithinLeaf(
              branch.children(index),
              start - branch.starts(index),
              end - branch.starts(index),
              limits,
              false
            )
              .map(edited => Branch(branch.children.updated(index, edited)))
          }

  private def deleteBySplitting(rope: Rope, start: Int, end: Int)(using Balance): Rope =
    BTreeJoin.concat(rope.prefixTo(start), rope.suffixFrom(end))
