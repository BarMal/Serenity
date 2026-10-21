package com.serenity.rope.btree

import com.serenity.rope.Balance

/** How many children an internal node holds. Every node but the root keeps between `Min` and `Max`, so an edit that
  * overflows a node splits it into halves that are both above `Min`, and a join that leaves one short merges it into a
  * neighbour.
  */
private[rope] object Fanout:
  val Max: Int    = 16
  val Min: Int    = 8
  val Target: Int = 12

/** How many UTF-16 units a leaf holds: never more than `max` and, outside a tree that is a single leaf, never fewer
  * than `min`. A bulk build fills leaves to `target`, which leaves room to type into them before they split.
  */
final private[rope] case class LeafLimits(max: Int, min: Int, target: Int)

private[rope] object LeafLimits:

  /** `leafChunkSize` is the largest leaf; below 4 a leaf could not be cut without leaving a piece empty. */
  def of(balance: Balance): LeafLimits =
    val max = math.max(4, balance.leafChunkSize)
    LeafLimits(max = max, min = math.max(1, max / 4), target = math.max(2, max / 2))

/** Cuts text into leaf-sized pieces without separating a surrogate pair or a CR from its LF. */
private[rope] object Chunking:

  /** `text` as the fewest near-equal pieces of at most `limits.max` units. Text that already fits is returned as it is,
    * so the caller's string is reused rather than copied.
    */
  def pieces(text: String, limits: LeafLimits): Vector[String] =
    if text.length <= limits.max then Vector(text)
    else cut(text, ceilDiv(text.length, limits.max - 1))

  /** `text` as pieces of about `limits.target` units, for building a tree that has room to grow. */
  def bulkPieces(text: String, limits: LeafLimits): Vector[String] =
    if text.length <= limits.max then Vector(text)
    else cut(text, ceilDiv(text.length, limits.target))

  private def ceilDiv(dividend: Int, divisor: Int): Int = (dividend + divisor - 1) / divisor

  private def cut(text: String, count: Int): Vector[String] =
    val boundaries = Vector.tabulate(count + 1) { part =>
      if part == 0 then 0
      else if part == count then text.length
      else safeCut(text, (text.length.toLong * part / count).toInt)
    }
    boundaries.zip(boundaries.drop(1)).collect { case (from, until) if until > from => text.substring(from, until) }

  private def safeCut(text: String, at: Int): Int =
    val splitsSurrogatePair =
      Character.isHighSurrogate(text.charAt(at - 1)) && Character.isLowSurrogate(text.charAt(at))
    val splitsCrLf = text.charAt(at - 1) == '\r' && text.charAt(at) == '\n'
    if splitsSurrogatePair || splitsCrLf then at - 1 else at
