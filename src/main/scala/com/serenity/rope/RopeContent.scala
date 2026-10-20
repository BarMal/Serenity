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
    (a: AnyRef).eq(b) || (a.weight == b.weight && (sameTree(a, b) || sameChunks(Cursor.over(a), Cursor.over(b))))

  /** The common case -- ropes of identical shape, e.g. one rebuilt from the same text -- needs no cursor bookkeeping.
    * Any mismatch, including a mere difference in shape, returns false and leaves the verdict to `sameChunks`. It walks
    * by index and allocates nothing, so comparing two independently built ropes costs the string comparisons alone.
    */
  private def sameTree(a: Rope, b: Rope): Boolean =
    (a: AnyRef).eq(b) ||
      ((a, b) match
        case (branchA: Branch, branchB: Branch) =>
          sameOffsets(branchA, branchB, 0) && sameChildren(branchA, branchB, 0)
        case (Leaf(textA), Leaf(textB)) => textA == textB
        case _                          => false)

  @tailrec
  private def sameOffsets(a: Branch, b: Branch, index: Int): Boolean =
    a.children.size == b.children.size &&
      (index > a.children.size || (a.starts(index) == b.starts(index) && sameOffsets(a, b, index + 1)))

  @tailrec
  private def sameChildren(a: Branch, b: Branch, index: Int): Boolean =
    index >= a.children.size || (sameTree(a.children(index), b.children(index)) && sameChildren(a, b, index + 1))

  /** Agrees with `String.hashCode` on the rope's text. */
  def hash(rope: Rope): Int = hashRope(rope, 0)

  private def hashRope(rope: Rope, seed: Int): Int =
    rope match
      case branch: Branch => hashChildren(branch, 0, seed)
      case Leaf(value)    => hashChars(value, 0, seed)

  @tailrec
  private def hashChildren(branch: Branch, index: Int, seed: Int): Int =
    if index >= branch.children.size then seed
    else hashChildren(branch, index + 1, hashRope(branch.children(index), seed))

  @tailrec
  private def hashChars(value: String, index: Int, seed: Int): Int =
    if index >= value.length then seed else hashChars(value, index + 1, 31 * seed + value.charAt(index))

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
      case (branch: Branch) :: rest => copy(pending = branch.children.toList ::: rest)
      case Leaf(value) :: rest      => Cursor(rest, value, 0)
      case Nil                      => this

  private object Cursor:
    def over(rope: Rope): Cursor = Cursor(List(rope), "", 0)

  /** Total weights are equal on entry, so a side that runs out first can only have empty subtrees left. */
  @tailrec
  private def sameChunks(a: Cursor, b: Cursor): Boolean =
    if a.exhausted && b.exhausted then
      (a.pending, b.pending) match
        case (Nil, _) | (_, Nil)                         => true
        case (headA :: _, headB :: _) if headA.eq(headB) => sameChunks(a.skippingHead, b.skippingHead)
        case (_ :: _, (_: Branch) :: _)                  => sameChunks(a, b.descended)
        case _                                           => sameChunks(a.descended, b)
    else if a.exhausted then a.pending.nonEmpty && sameChunks(a.descended, b)
    else if b.exhausted then b.pending.nonEmpty && sameChunks(a, b.descended)
    else
      val length = math.min(a.remaining, b.remaining)
      a.chunk.regionMatches(a.offset, b.chunk, b.offset, length) && sameChunks(a.consumed(length), b.consumed(length))
