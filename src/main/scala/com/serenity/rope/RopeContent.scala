package com.serenity.rope

import scala.annotation.tailrec

/** Content equality and hashing for ropes, independent of how the tree is shaped (#1942).
  *
  * Both walk the leaf chunks left to right and never build the rope's text. Equality also skips any pair of subtrees
  * that are the same object -- an edit shares every subtree it did not touch -- and compares the rest a chunk at a
  * time, so ropes cut at different places are compared without either being rebuilt.
  */
private[rope] object RopeContent:

  def equal(a: Rope, b: Rope): Boolean =
    (a: AnyRef).eq(b) || (a.weight == b.weight && (sameTree(List(a, b)) || sameChunks(Cursor.over(a), Cursor.over(b))))

  /** The common case -- ropes of identical shape, e.g. one rebuilt from the same text -- needs no cursor bookkeeping.
    * Any mismatch, including a mere difference in shape, returns false and leaves the verdict to `sameChunks`. `pairs`
    * holds the ropes still to compare, two at a time.
    */
  @tailrec
  private def sameTree(pairs: List[Rope]): Boolean = pairs match
    case Nil => true
    case a :: b :: rest =>
      if a.eq(b) then sameTree(rest)
      else
        (a, b) match
          case (Node(leftA, rightA), Node(leftB, rightB)) =>
            leftA.weight == leftB.weight && sameTree(leftA :: leftB :: rightA :: rightB :: rest)
          case (Leaf(textA), Leaf(textB)) => textA == textB && sameTree(rest)
          case _                          => false
    case _ :: Nil => false

  /** Agrees with `String.hashCode` on the rope's text. */
  def hash(rope: Rope): Int =
    @tailrec
    def go(stack: List[Rope], acc: Int): Int = stack match
      case Nil => acc
      case head :: rest =>
        head match
          case Node(left, right) => go(left :: right :: rest, acc)
          case Leaf(value)       => go(rest, value.foldLeft(acc)((running, char) => 31 * running + char))
    go(List(rope), 0)

  def equalsText(rope: Rope, text: String): Boolean =
    rope.weight == text.length &&
      rope.chunksInRange(0, rope.weight).forall((offset, chunk) => text.startsWith(chunk, offset))

  /** The unread remainder of a rope: the characters of `chunk` from `offset`, then the subtrees on `pending`. */
  final private case class Cursor(pending: List[Rope], chunk: String, offset: Int):
    def exhausted: Boolean = offset >= chunk.length

    def remaining: Int = chunk.length - offset

    def consumed(count: Int): Cursor = copy(offset = offset + count)

    def skippingHead: Cursor = copy(pending = pending.drop(1))

    /** Replaces the head subtree with its children, or loads it as the chunk if it is a leaf. */
    def descended: Cursor = pending match
      case Node(left, right) :: rest => copy(pending = left :: right :: rest)
      case Leaf(value) :: rest       => Cursor(rest, value, 0)
      case Nil                       => this

  private object Cursor:
    def over(rope: Rope): Cursor = Cursor(List(rope), "", 0)

  /** Total weights are equal on entry, so a side that runs out first can only have empty subtrees left. */
  @tailrec
  private def sameChunks(a: Cursor, b: Cursor): Boolean =
    if a.exhausted && b.exhausted then
      (a.pending, b.pending) match
        case (Nil, _) | (_, Nil)                         => true
        case (headA :: _, headB :: _) if headA.eq(headB) => sameChunks(a.skippingHead, b.skippingHead)
        case (_ :: _, Node(_, _) :: _)                   => sameChunks(a, b.descended)
        case _                                           => sameChunks(a.descended, b)
    else if a.exhausted then a.pending.nonEmpty && sameChunks(a.descended, b)
    else if b.exhausted then b.pending.nonEmpty && sameChunks(a, b.descended)
    else
      val length = math.min(a.remaining, b.remaining)
      a.chunk.regionMatches(a.offset, b.chunk, b.offset, length) && sameChunks(a.consumed(length), b.consumed(length))
