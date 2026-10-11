package com.serenity.rope

import scala.annotation.{tailrec, unused}

/** Finds the offset range within an edited `Rope` that may have changed, without reconstructing either rope's text.
  *
  * `Rope.insert`/`delete` rebuild only the path from the root to the edited leaf, so every subtree an edit did not
  * touch keeps the exact same object reference in the new tree ([[Node]] and [[Leaf]] both fast-path `equals` on `eq`
  * for this reason). [[changedOffsetRange]] walks both trees together, skipping any subtree pair that is `eq` without
  * looking inside it, and falls back to a plain character scan only once it reaches leaf-sized subtrees -- so the cost
  * of finding "what changed" is proportional to how much of the tree an edit actually touched, not to the document's
  * size.
  *
  * Where the two trees' shapes do not line up (after a rebuild, or an edit that moved a subtree boundary) the walk
  * descends whichever side is taller and reads the other through an offset window, so nothing is split, rebuilt or
  * rebalanced to compare them. The range is the exact common prefix and suffix of the two texts; a subtree a rebalance
  * rebuilt costs the walk its leaves, not the text it holds.
  */
object RopeDiff:

  /** `None` when `before` and `after` have identical content (including the common case where they are the exact same
    * object). Otherwise the `(start, end)` offset range in `after` that may differ from `before`.
    */
  def changedOffsetRange(before: Rope, after: Rope)(using @unused balance: Balance): Option[(Int, Int)] =
    if isSameReference(before, after) then None
    else
      val bound  = math.min(before.weight, after.weight)
      val prefix = commonPrefixWeight(before, after, bound)
      if prefix == bound && before.weight == after.weight then None
      else
        val suffixBound = bound - prefix
        val suffix      = if suffixBound <= 0 then 0 else commonSuffixWeight(before, after, suffixBound)
        Some((prefix, after.weight - suffix))

  private def isSameReference(a: Rope, b: Rope): Boolean =
    (a: AnyRef) eq (b: AnyRef)

  private def commonPrefixWeight(a: Rope, b: Rope, bound: Int): Int =
    commonRun(a, 0, b, 0, bound, fromEnd = false)

  private def commonSuffixWeight(a: Rope, b: Rope, bound: Int): Int =
    commonRun(a, 0, b, 0, bound, fromEnd = true)

  /** How many characters `a` and `b` share over `length` characters, counted from the start of each (from the end,
    * backwards, when `fromEnd`) after leaving out the first `aSkip` and `bSkip` of them. A subtree pair that is `eq` at
    * the same skip is shared whole without being read.
    */
  private def commonRun(a: Rope, aSkip: Int, b: Rope, bSkip: Int, length: Int, fromEnd: Boolean): Int =
    if length <= 0 then 0
    else if isSameReference(a, b) && aSkip == bSkip then length
    else
      a match
        case aNode: Node if isTaller(aNode, b) => splitRun(aNode, aSkip, b, bSkip, length, fromEnd)
        case _ =>
          b match
            case bNode: Node => splitRun(bNode, bSkip, a, aSkip, length, fromEnd)
            case Leaf(bText) =>
              a match
                case Leaf(aText) => sharedChars(aText, aSkip, bText, bSkip, length, fromEnd)
                case _           => 0

  private def isTaller(node: Node, other: Rope): Boolean =
    other match
      case otherNode: Node => node.height >= otherNode.height
      case _               => true

  /** [[commonRun]] with `node`, the taller side, read through its children: the window falls in one of them or spans
    * both, and `other` is only ever read through the same window.
    */
  private def splitRun(node: Node, nodeSkip: Int, other: Rope, otherSkip: Int, length: Int, fromEnd: Boolean): Int =
    val first  = if fromEnd then node.right else node.left
    val second = if fromEnd then node.left else node.right
    if nodeSkip >= first.weight then commonRun(second, nodeSkip - first.weight, other, otherSkip, length, fromEnd)
    else if nodeSkip + length <= first.weight then commonRun(first, nodeSkip, other, otherSkip, length, fromEnd)
    else
      val inFirst = first.weight - nodeSkip
      val shared  = commonRun(first, nodeSkip, other, otherSkip, inFirst, fromEnd)
      if shared < inFirst then shared
      else shared + commonRun(second, 0, other, otherSkip + inFirst, length - inFirst, fromEnd)

  @tailrec
  private def sharedChars(
    a: String,
    aSkip: Int,
    b: String,
    bSkip: Int,
    length: Int,
    fromEnd: Boolean,
    from: Int = 0
  ): Int =
    if from >= length then from
    else
      val aAt = if fromEnd then a.length - 1 - aSkip - from else aSkip + from
      val bAt = if fromEnd then b.length - 1 - bSkip - from else bSkip + from
      if a.charAt(aAt) != b.charAt(bAt) then from
      else sharedChars(a, aSkip, b, bSkip, length, fromEnd, from + 1)
