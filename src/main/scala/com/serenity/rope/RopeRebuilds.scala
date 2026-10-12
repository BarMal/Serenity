package com.serenity.rope

import java.util.concurrent.atomic.LongAdder

/** Running totals of whole-subtree rebuilds, so a latency spike can be attributed to a rebuild rather than to the JIT
  * or the garbage collector. The binary-tree rope rebuilt a subtree from its leaves whenever an edit unbalanced it; the
  * B-tree splits and merges one node at a time and never does, so these stay at zero. They remain so a benchmark
  * written against the old rope reports the absence of rebuilds instead of failing to compile. Only counts; it never
  * influences an edit.
  */
object RopeRebuilds:

  final case class Totals(rebuilds: Long, leaves: Long)

  private val rebuildCount = LongAdder()
  private val leafCount    = LongAdder()

  def totals: Totals = Totals(rebuildCount.sum(), leafCount.sum())

  private[rope] def record(leaves: Int): Unit =
    rebuildCount.increment()
    leafCount.add(leaves.toLong)
