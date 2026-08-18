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

  override def rebalance: Rope =
    if isWeightBalanced && isHeightBalanced then this
    else rebuild

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
