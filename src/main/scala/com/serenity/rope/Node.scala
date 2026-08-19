package com.serenity.rope

final case class Node(left: Rope, right: Rope)(using balance: Balance) extends Rope:

  override val weight: Int       = left.weight + right.weight
  override val height: Int       = Math.max(left.height, right.height) + 1
  override val newlineCount: Int = left.newlineCount + right.newlineCount

  override val lastLineLength: Int =
    if right.weight == 0 then left.lastLineLength
    else if right.newlineCount == 0 && !left.endsWithNewline then left.lastLineLength + right.lastLineLength
    else right.lastLineLength

  override val endsWithNewline: Boolean =
    if right.weight == 0 then left.endsWithNewline else right.endsWithNewline

  override def isWeightBalanced: Boolean =
    Math.abs(left.weight - right.weight) <= balance.weightBalance

  override def isHeightBalanced: Boolean =
    Math.abs(left.height - right.height) <= balance.heightBalance

  /** Rebuilding is O(leaves), so the criterion decides how often an edit costs that. Weight symmetry between the two
    * children cannot survive an edit without a rebuild, and says nothing about the depth `index` and `splitAt` descend;
    * the Fibonacci bound is what those actually pay for.
    */
  override def rebalance: Rope =
    if isDepthBalanced && isHeightBalanced then this else rebuild

  /** Splices the text into the leaf that holds `index`, rebuilding only the path down to it.
    *
    * The inherited `splitAt` + `concat` + `concat` route rebuilt the whole rope for a single character, because
    * concatenating a one-character leaf onto a large rope fails any symmetry criterion. Descending instead is the
    * short-leaf case from Boehm, Atkinson and Plass, and what a b-tree rope does when it edits a chunk in place.
    */
  override def insert(index: Int, str: String): Rope =
    if index < 0 || index > weight then this
    else if index <= left.weight then Node(left.insert(index, str), right).rebalance
    else Node(left, right.insert(index - left.weight, str)).rebalance

  override def deleteRight(start: Int, count: Int): Rope =
    if count == 0 then this
    else if count < 0 then deleteLeft(start, Math.abs(count))
    else if start < 0 then deleteRight(0, count + start)
    else if start + count > weight then deleteRight(start, weight - start)
    else if start >= left.weight then Node(left, right.deleteRight(start - left.weight, count)).rebalance
    else if start + count <= left.weight then Node(left.deleteRight(start, count), right).rebalance
    else
      val fromLeft = left.weight - start
      Node(left.deleteRight(start, fromLeft), right.deleteRight(0, count - fromLeft)).rebalance

  override def splitAt(index: Int): Option[(Rope, Rope)] =
    if index < 0 || index > weight then None
    else if index == 0 then Some(Leaf(""), this)
    else if weight == index then Some(this, Leaf(""))
    else if left.weight == index then Some(left, right)
    else if index < left.weight then
      left.splitAt(index).map {
        case (first, second) =>
          (first.rebalance, Node(second, right).rebalance)
      }
    else
      right.splitAt(index - left.weight).map {
        case (first, second) =>
          (Node(left, first).rebalance, second.rebalance)
      }

  override def index(i: Int): Option[Char] =
    if i < left.weight then left.index(i) else right.index(i - left.weight)

  // The compiler-generated equals recurses through left == that.left && right == that.right with no fast path,
  // so comparing two large ropes -- even the very same object to itself -- walks the entire tree and can stack
  // overflow on deeply skewed trees. Short-circuiting on reference identity and on a cheap weight mismatch avoids
  // that descent for the two most common comparisons: "is this literally the same content" and "clearly different
  // length".
  override def equals(obj: Any): Boolean =
    obj match
      case that: AnyRef if (this: AnyRef).eq(that) => true
      case that: Node                              => weight == that.weight && left == that.left && right == that.right
      case _                                       => false
