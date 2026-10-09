package com.serenity.rope

/** Finds the offset range within an edited `Rope` that may have changed, without reconstructing either rope's text.
  *
  * `Rope.insert`/`delete` rebuild only the path from the root to the edited leaf, so every subtree an edit did not
  * touch keeps the exact same object reference in the new tree ([[Node]] and [[Leaf]] both fast-path `equals` on `eq`
  * for this reason). [[changedOffsetRange]] walks both trees together, skipping any subtree pair that is `eq` without
  * looking inside it, and falls back to a plain character scan only once it reaches leaf-sized subtrees -- so the cost
  * of finding "what changed" is proportional to how much of the tree an edit actually touched, not to the document's
  * size.
  *
  * `rebalance` can occasionally rebuild a subtree wider than the literal edit (a weight/height/depth imbalance, or a
  * leaf outgrowing `Balance.leafChunkSize`), which replaces every leaf in that subtree with a fresh object even where
  * the text is unchanged. When that happens this walk reports a wider range than the literal edit -- never a narrower
  * one. The bias is the same one [[com.serenity.ui.layout.DirtyLineDiff]] documents for the same reason: report too
  * much rather than too little.
  */
object RopeDiff:

  /** `None` when `before` and `after` have identical content (including the common case where they are the exact same
    * object). Otherwise the `(start, end)` offset range in `after` that may differ from `before`.
    */
  def changedOffsetRange(before: Rope, after: Rope)(using Balance): Option[(Int, Int)] =
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

  private def commonPrefixWeight(a: Rope, b: Rope, bound: Int)(using Balance): Int =
    if bound <= 0 then 0
    else if isSameReference(a, b) then math.min(a.weight, bound)
    else
      (a, b) match
        case (Node(al, ar), Node(bl, br)) =>
          alignedPrefixWeight(al, ar, bl, br, bound).getOrElse(splitPrefixWeight(a, b, bound))
        case _ => splitPrefixWeight(a, b, bound)

  /** Compares the two left children directly, so an edit inside one subtree never splits -- and so rebalances and
    * rebuilds -- the other rope. `None` when the common prefix runs past left children of different lengths.
    */
  private def alignedPrefixWeight(al: Rope, ar: Rope, bl: Rope, br: Rope, bound: Int)(using Balance): Option[Int] =
    val leftBound = math.min(math.min(al.weight, bl.weight), bound)
    val leftMatch = commonPrefixWeight(al, bl, leftBound)
    if leftMatch < leftBound || leftMatch >= bound then Some(leftMatch)
    else Option.when(al.weight == bl.weight)(leftMatch + commonPrefixWeight(ar, br, bound - leftMatch))

  private def splitPrefixWeight(a: Rope, b: Rope, bound: Int)(using Balance): Int =
    a match
      case Node(al, ar) =>
        val leftLen         = math.min(al.weight, b.weight)
        val (bLeft, bRight) = splitAtClamped(b, leftLen)
        val leftBound       = math.min(leftLen, bound)
        val leftMatch       = commonPrefixWeight(al, bLeft, leftBound)
        if leftMatch < leftBound || leftMatch >= bound then leftMatch
        else leftMatch + commonPrefixWeight(ar, bRight, bound - leftMatch)
      case _ =>
        b match
          case Node(bl, br) =>
            val leftLen         = math.min(bl.weight, a.weight)
            val (aLeft, aRight) = splitAtClamped(a, leftLen)
            val leftBound       = math.min(leftLen, bound)
            val leftMatch       = commonPrefixWeight(aLeft, bl, leftBound)
            if leftMatch < leftBound || leftMatch >= bound then leftMatch
            else leftMatch + commonPrefixWeight(aRight, br, bound - leftMatch)
          case _ =>
            matchingPrefixChars(a, b, math.min(bound, math.min(a.weight, b.weight)))

  @annotation.tailrec
  private def matchingPrefixChars(a: Rope, b: Rope, n: Int, from: Int = 0): Int =
    if from >= n || a.index(from) != b.index(from) then from
    else matchingPrefixChars(a, b, n, from + 1)

  private def commonSuffixWeight(a: Rope, b: Rope, bound: Int)(using Balance): Int =
    if bound <= 0 then 0
    else if isSameReference(a, b) then math.min(a.weight, bound)
    else
      (a, b) match
        case (Node(al, ar), Node(bl, br)) =>
          alignedSuffixWeight(al, ar, bl, br, bound).getOrElse(splitSuffixWeight(a, b, bound))
        case _ => splitSuffixWeight(a, b, bound)

  /** [[alignedPrefixWeight]] from the right-hand end. */
  private def alignedSuffixWeight(al: Rope, ar: Rope, bl: Rope, br: Rope, bound: Int)(using Balance): Option[Int] =
    val rightBound = math.min(math.min(ar.weight, br.weight), bound)
    val rightMatch = commonSuffixWeight(ar, br, rightBound)
    if rightMatch < rightBound || rightMatch >= bound then Some(rightMatch)
    else Option.when(ar.weight == br.weight)(rightMatch + commonSuffixWeight(al, bl, bound - rightMatch))

  private def splitSuffixWeight(a: Rope, b: Rope, bound: Int)(using Balance): Int =
    a match
      case Node(al, ar) =>
        val rightLen        = math.min(ar.weight, b.weight)
        val (bLeft, bRight) = splitAtClamped(b, b.weight - rightLen)
        val rightBound      = math.min(rightLen, bound)
        val rightMatch      = commonSuffixWeight(ar, bRight, rightBound)
        if rightMatch < rightBound || rightMatch >= bound then rightMatch
        else rightMatch + commonSuffixWeight(al, bLeft, bound - rightMatch)
      case _ =>
        b match
          case Node(bl, br) =>
            val rightLen        = math.min(br.weight, a.weight)
            val (aLeft, aRight) = splitAtClamped(a, a.weight - rightLen)
            val rightBound      = math.min(rightLen, bound)
            val rightMatch      = commonSuffixWeight(aRight, br, rightBound)
            if rightMatch < rightBound || rightMatch >= bound then rightMatch
            else rightMatch + commonSuffixWeight(aLeft, bl, bound - rightMatch)
          case _ =>
            matchingSuffixChars(a, b, math.min(bound, math.min(a.weight, b.weight)))

  @annotation.tailrec
  private def matchingSuffixChars(a: Rope, b: Rope, n: Int, from: Int = 0): Int =
    if from >= n || a.index(a.weight - 1 - from) != b.index(b.weight - 1 - from) then from
    else matchingSuffixChars(a, b, n, from + 1)

  /** `at` is always derived from a subtree's own `weight` clamped against the other rope's `weight`, so it is
    * mathematically always in `[0, r.weight]`.
    *
    * The halves are only compared, never kept, so they are cut without `Rope.splitAt`'s rebalancing: cutting a large
    * rope near one end leaves a sliver beside a large subtree, and rebalancing that rebuilds the subtree.
    */
  private def splitAtClamped(r: Rope, at: Int)(using Balance): (Rope, Rope) =
    cut(r, math.max(0, math.min(at, r.weight)))

  private def cut(r: Rope, at: Int)(using Balance): (Rope, Rope) =
    r match
      case _ if at == 0        => (Rope.empty, r)
      case _ if at == r.weight => (r, Rope.empty)
      case Node(left, right) =>
        if at == left.weight then (left, right)
        else if at < left.weight then
          val (before, after) = cut(left, at)
          (before, Node(after, right))
        else
          val (before, after) = cut(right, at - left.weight)
          (Node(left, before), after)
      case Leaf(value) => (Leaf(value.take(at)), Leaf(value.drop(at)))
