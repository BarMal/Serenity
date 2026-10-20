package com.serenity.rope

import com.serenity.rope.btree.BTreeBuild

/** Builds a rope from text that is already LF-only, one leaf at a time, in time linear in its length. */
object RopeBuilder:

  /** `leaves` joined left to right under as few levels of branches as their number allows. Unlike [[Rope.apply]], no
    * character is copied: each leaf string is the one passed in, unless it is longer than a leaf may be.
    */
  def fromLeaves(leaves: Iterable[String])(using Balance): Rope =
    BTreeBuild.fromLeafValues(leaves)

  /** The given leaf objects themselves, joined in order; for specs that need particular leaf instances in a tree. */
  private[rope] def ofLeaves(leaves: Leaf*)(using Balance): Rope =
    BTreeBuild.assemble(leaves.filter(_.weight > 0).toVector)

/** A rope's text as the leaves it is stored in. */
object RopeChunks:

  /** The leaf strings of `rope` from left to right, without joining them. */
  def of(rope: Rope): Iterator[String] =
    rope.chunksInRange(0, rope.weight).map((_, chunk) => chunk)
