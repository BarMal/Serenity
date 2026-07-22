package com.serenity.rope

case class Leaf(value: String)(using balance: Balance) extends Rope:
  override def weight: Int               = value.length
  override def height: Int               = 1
  override val newlineCount: Int         = value.count(_ == '\n')
  override val lastLineLength: Int       = value.length - value.lastIndexOf('\n') - 1
  override val endsWithNewline: Boolean  = value.endsWith("\n")
  override def isWeightBalanced: Boolean = true
  override def isHeightBalanced: Boolean = true
  override def rebalance: Rope =
    if value.length > balance.leafChunkSize then Rope(value) else this

  override def splitAt(index: Int): Option[(Rope, Rope)] =
    if index < 0 || index > value.length then None
    else Some(Leaf(value.take(index)), Leaf(value.drop(index)))

  override def index(i: Int): Option[Char] =
    Option.when(i >= 0 && i < value.length)(value.charAt(i))

  override def insert(index: Int, str: String): Rope =
    if index < 0 || index > value.length then this
    else
      val (pre, post) = value.splitAt(index)
      Rope((pre + str) + post)

  override def deleteLeft(start: Int, count: Int): Rope =
    val (pre, post) = value.splitAt(start)
    Leaf(pre.dropRight(count) + post)

  override def deleteRight(start: Int, count: Int): Rope =
    val (pre, post) = value.splitAt(start)
    Leaf(pre + post.drop(count))

  override def collect(): String = value
