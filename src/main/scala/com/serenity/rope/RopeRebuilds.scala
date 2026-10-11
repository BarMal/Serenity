package com.serenity.rope

import java.util.concurrent.atomic.LongAdder

/** Running totals of the subtree rebuilds `Rope.rebuild` performs, so a latency spike can be attributed to a rebuild
  * rather than to the JIT or the garbage collector. Only counts; it never influences an edit.
  */
object RopeRebuilds:

  final case class Totals(rebuilds: Long, leaves: Long)

  private val rebuildCount = LongAdder()
  private val leafCount    = LongAdder()

  def totals: Totals = Totals(rebuildCount.sum(), leafCount.sum())

  private[rope] def record(leaves: Int): Unit =
    rebuildCount.increment()
    leafCount.add(leaves.toLong)
