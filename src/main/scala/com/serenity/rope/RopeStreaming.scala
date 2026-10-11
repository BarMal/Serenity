package com.serenity.rope

import scala.annotation.tailrec

/** Builds a rope from text that is already LF-only, one leaf at a time, in time linear in its length. */
object RopeBuilder:

  /** `leaves` joined left to right, paired level by level so the tree is as shallow as the leaf count allows. Unlike
    * [[Rope.apply]], no character is copied: each leaf string is the one passed in.
    */
  def fromLeaves(leaves: Iterable[String])(using Balance): Rope =
    pairUp(leaves.iterator.filter(_.nonEmpty).map[Rope](Leaf(_)).toVector)

  @tailrec
  private def pairUp(level: Vector[Rope])(using Balance): Rope =
    level match
      case Vector()       => Leaf("")
      case Vector(single) => single
      case _ =>
        pairUp(
          level
            .grouped(2)
            .map[Rope] {
              case Vector(left, right) => Node(left, right)
              case Vector(only)        => only
              case _                   => Leaf("")
            }
            .toVector
        )

/** A rope's text as the leaves it is stored in. */
object RopeChunks:

  /** The leaf strings of `rope` from left to right, without joining them. */
  def of(rope: Rope): Iterator[String] =
    rope.chunksInRange(0, rope.weight).map((_, chunk) => chunk)
