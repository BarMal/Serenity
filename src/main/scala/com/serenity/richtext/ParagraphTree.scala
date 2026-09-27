package com.serenity.richtext

/** A balanced, size-augmented binary tree of paragraph leaves, weighted by paragraph count and character count.
  *
  * This is the derived structural index [[RichTextDocument]] keeps over its paragraph sequence (#1663): point
  * lookup ([[paragraphAt]]), range transforms ([[updatedRange]], [[forallInRange]]), splitting ([[ParagraphTree.splitAt]])
  * and rejoining ([[ParagraphTree.link]]) only walk `O(log n)` nodes plus whatever paragraphs the caller actually
  * touches, instead of rebuilding the whole sequence the way mapping over a `List` does on every edit.
  *
  * Balance follows the weight-balanced-tree scheme used by Adams' "Efficient sets -- a balancing act" (as
  * implemented by Haskell's `Data.Map`/`Data.Set` in `containers`): a node is rebalanced with a single or double
  * rotation whenever one side holds more than [[ParagraphTree.BalanceFactor]] times the other, via the
  * [[ParagraphTree.balance]] and [[ParagraphTree.link]] smart constructors. That invariant keeps tree height at
  * `O(log n)` regardless of how the tree was assembled -- direct bulk build, or incremental split/link -- since
  * rotations only regroup existing subtrees and never reorder leaves.
  */
sealed trait ParagraphTree:
  def paragraphCount: Int
  def charCount: Int

  /** The paragraph at sequence index `index`, or `None` when out of `[0, paragraphCount)`. `O(log n)`. */
  def paragraphAt(index: Int): Option[RichTextParagraph] =
    if index < 0 || index >= paragraphCount then None
    else
      def go(node: ParagraphTree, i: Int): Option[RichTextParagraph] =
        node match
          case ParagraphTree.Empty            => None
          case ParagraphTree.Leaf(paragraph)  => Some(paragraph)
          case ParagraphTree.Branch(left, right, _, _) =>
            val leftCount = left.paragraphCount
            if i < leftCount then go(left, i) else go(right, i - leftCount)
      go(this, index)

  /** Flattens the tree back into paragraph order. `O(n)`, with `O(log n)` recursion depth. */
  def toParagraphs: List[RichTextParagraph] =
    def go(node: ParagraphTree, acc: List[RichTextParagraph]): List[RichTextParagraph] =
      node match
        case ParagraphTree.Empty                     => acc
        case ParagraphTree.Leaf(paragraph)           => paragraph :: acc
        case ParagraphTree.Branch(left, right, _, _) => go(left, go(right, acc))
    go(this, Nil)

  /** Applies `f` to every paragraph whose absolute index falls in `[startIndex, endIndex]` (inclusive, and not
    * clamped to the document's own bounds -- callers that want clamping do it before calling in). Subtrees that
    * fall entirely outside the range are returned unchanged (by reference), so an update touches `O(log n + k)`
    * nodes for `k` touched paragraphs rather than rebuilding every node in the tree.
    */
  def updatedRange(startIndex: Int, endIndex: Int)(f: (RichTextParagraph, Int) => RichTextParagraph): ParagraphTree =
    def go(node: ParagraphTree, offset: Int): ParagraphTree =
      node match
        case ParagraphTree.Empty => ParagraphTree.Empty
        case ParagraphTree.Leaf(paragraph) =>
          if offset >= startIndex && offset <= endIndex then ParagraphTree.Leaf(f(paragraph, offset)) else node
        case branch @ ParagraphTree.Branch(left, right, count, _) =>
          val leftCount     = left.paragraphCount
          val leftTouches   = offset <= endIndex && (offset + leftCount - 1) >= startIndex
          val rightTouches  = (offset + leftCount) <= endIndex && (offset + count - 1) >= startIndex
          val newLeft       = if leftTouches then go(left, offset) else left
          val newRight      = if rightTouches then go(right, offset + leftCount) else right
          if (newLeft eq left) && (newRight eq right) then branch else ParagraphTree.bin(newLeft, newRight)
    go(this, 0)

  /** True when `p` holds for every paragraph in `[startIndex, endIndex]`, vacuously true when the range covers no
    * paragraph. Short-circuits both across subtrees that fall outside the range and on the first failing paragraph.
    */
  def forallInRange(startIndex: Int, endIndex: Int)(p: (RichTextParagraph, Int) => Boolean): Boolean =
    def go(node: ParagraphTree, offset: Int): Boolean =
      node match
        case ParagraphTree.Empty           => true
        case ParagraphTree.Leaf(paragraph) => offset < startIndex || offset > endIndex || p(paragraph, offset)
        case ParagraphTree.Branch(left, right, count, _) =>
          val leftCount     = left.paragraphCount
          val leftTouches   = offset <= endIndex && (offset + leftCount - 1) >= startIndex
          val rightTouches  = (offset + leftCount) <= endIndex && (offset + count - 1) >= startIndex
          (!leftTouches || go(left, offset)) && (!rightTouches || go(right, offset + leftCount))
    go(this, 0)

  /** True when some paragraph satisfies `p`, short-circuiting on the first match. */
  def existsAny(p: RichTextParagraph => Boolean): Boolean =
    this match
      case ParagraphTree.Empty                     => false
      case ParagraphTree.Leaf(paragraph)           => p(paragraph)
      case ParagraphTree.Branch(left, right, _, _) => left.existsAny(p) || right.existsAny(p)

  /** Applies `f` to every paragraph, preserving the existing tree shape (no rebalancing is needed: the paragraph
    * count of every subtree is unchanged).
    */
  def mapAll(f: RichTextParagraph => RichTextParagraph): ParagraphTree =
    this match
      case ParagraphTree.Empty           => ParagraphTree.Empty
      case ParagraphTree.Leaf(paragraph) => ParagraphTree.Leaf(f(paragraph))
      case ParagraphTree.Branch(left, right, count, _) =>
        val newLeft  = left.mapAll(f)
        val newRight = right.mapAll(f)
        ParagraphTree.Branch(newLeft, newRight, count, newLeft.charCount + newRight.charCount)

  /** Replaces the paragraphs in `[startIndex, endIndex]` (inclusive, clamped to `[0, paragraphCount - 1]` by the
    * caller) with `replacement`, via `O(log n)` split/link plus the `O(m)` cost of building the replacement span.
    */
  def replaceSlice(startIndex: Int, endIndex: Int, replacement: List[RichTextParagraph]): ParagraphTree =
    val (prefix, rest)      = ParagraphTree.splitAt(this, startIndex)
    val (_, suffix)         = ParagraphTree.splitAt(rest, endIndex - startIndex + 1)
    ParagraphTree.link(ParagraphTree.link(prefix, ParagraphTree.fromParagraphs(replacement)), suffix)

object ParagraphTree:
  case object Empty extends ParagraphTree:
    def paragraphCount: Int = 0
    def charCount: Int      = 0

  final case class Leaf(paragraph: RichTextParagraph) extends ParagraphTree:
    def paragraphCount: Int = 1
    def charCount: Int      = paragraph.plainText.length

  final case class Branch private[ParagraphTree] (
    left: ParagraphTree,
    right: ParagraphTree,
    paragraphCount: Int,
    charCount: Int
  ) extends ParagraphTree

  private val BalanceFactor = 3

  /** Builds a balanced tree from a flat paragraph list in `O(n)` via divide-and-conquer over an indexed view,
    * rather than folding `link` over each element one at a time.
    */
  def fromParagraphs(paragraphs: List[RichTextParagraph]): ParagraphTree =
    val items = paragraphs.toVector
    def build(from: Int, until: Int): ParagraphTree =
      val size = until - from
      if size <= 0 then Empty
      else if size == 1 then Leaf(items(from))
      else
        val mid = from + size / 2
        bin(build(from, mid), build(mid, until))
    build(0, items.length)

  /** Splits `tree` into the paragraphs before `index` and from `index` onward, `O(log n)`. Out-of-range indices
    * clamp to the nearest end rather than failing.
    */
  def splitAt(tree: ParagraphTree, index: Int): (ParagraphTree, ParagraphTree) =
    tree match
      case Empty => (Empty, Empty)
      case Leaf(_) =>
        if index <= 0 then (Empty, tree) else (tree, Empty)
      case Branch(left, right, count, _) =>
        if index <= 0 then (Empty, tree)
        else if index >= count then (tree, Empty)
        else
          val leftCount = left.paragraphCount
          if index < leftCount then
            val (leftLeft, leftRight) = splitAt(left, index)
            (leftLeft, link(leftRight, right))
          else if index > leftCount then
            val (rightLeft, rightRight) = splitAt(right, index - leftCount)
            (link(left, rightLeft), rightRight)
          else (left, right)

  /** Joins two trees back into one, in order, `O(log n)`: descends the taller side to keep the weight-balance
    * invariant instead of naively stacking a [[Branch]] on top (which would defeat the height bound for a long
    * chain of splits).
    */
  def link(left: ParagraphTree, right: ParagraphTree): ParagraphTree =
    (left, right) match
      case (Empty, t) => t
      case (t, Empty) => t
      case _ =>
        val leftCount  = left.paragraphCount
        val rightCount = right.paragraphCount
        if BalanceFactor * leftCount < rightCount then
          right match
            case Branch(rl, rr, _, _) => balance(link(left, rl), rr)
            case _                    => balance(left, right)
        else if BalanceFactor * rightCount < leftCount then
          left match
            case Branch(ll, lr, _, _) => balance(ll, link(lr, right))
            case _                    => balance(left, right)
        else balance(left, right)

  private def bin(left: ParagraphTree, right: ParagraphTree): ParagraphTree =
    (left, right) match
      case (Empty, Empty)   => Empty
      case (single, Empty)  => single
      case (Empty, single)  => single
      case _                => Branch(left, right, left.paragraphCount + right.paragraphCount, left.charCount + right.charCount)

  /** Rebalances `left`/`right` into one node, applying a single or double rotation once one side holds more than
    * [[BalanceFactor]] times the other's paragraph count.
    */
  private def balance(left: ParagraphTree, right: ParagraphTree): ParagraphTree =
    val leftCount  = left.paragraphCount
    val rightCount = right.paragraphCount
    if leftCount + rightCount < 2 then bin(left, right)
    else if rightCount > BalanceFactor * leftCount then
      right match
        case Branch(rl, rr, _, _) if rl.paragraphCount < 2 * rr.paragraphCount => bin(bin(left, rl), rr)
        case Branch(Branch(rll, rlr, _, _), rr, _, _)                         => bin(bin(left, rll), bin(rlr, rr))
        case _                                                                 => bin(left, right)
    else if leftCount > BalanceFactor * rightCount then
      left match
        case Branch(ll, lr, _, _) if lr.paragraphCount < 2 * ll.paragraphCount => bin(ll, bin(lr, right))
        case Branch(ll, Branch(lrl, lrr, _, _), _, _)                          => bin(bin(ll, lrl), bin(lrr, right))
        case _                                                                 => bin(left, right)
    else bin(left, right)
