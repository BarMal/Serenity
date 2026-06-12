package com.serenity.rope

case class Node(left: Rope, right: Rope)(using balance: Balance) extends Rope:

  override val weight: Int = left.weight + right.weight
  override val height: Int = Math.max(left.height, right.height) + 1

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
