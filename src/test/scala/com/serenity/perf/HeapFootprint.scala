package com.serenity.perf

import java.lang.management.ManagementFactory
import java.lang.ref.Reference

/** Retained-heap measurement: what a value keeps alive once everything built along the way is garbage. */
private[perf] object HeapFootprint:

  private val memory = ManagementFactory.getMemoryMXBean

  private val StableSamples = 3
  private val MaxGcRounds   = 60

  /** Collection and the JVM's own threads leave the reading moving by a few KiB; a footprint is judged in MiB. */
  private val StableWithinBytes = 64L * 1024

  private[perf] def isStable(readings: Seq[Long]): Boolean =
    readings.lengthCompare(StableSamples) >= 0 && {
      val lastReadings = readings.takeRight(StableSamples)
      lastReadings.max - lastReadings.min <= StableWithinBytes
    }

  /** Forces collections until three consecutive used-heap readings agree, then returns the lowest of them. */
  def settledUsedBytes(): Long =
    @annotation.tailrec
    def loop(round: Int, readings: Vector[Long]): Long =
      System.gc()
      val updated = readings :+ memory.getHeapMemoryUsage.getUsed
      if isStable(updated) || round >= MaxGcRounds then updated.takeRight(StableSamples).min
      else loop(round + 1, updated)
    loop(1, Vector.empty)

  /** Heap held by the value `build` returns, over a baseline settled before building. The value stays strongly
    * reachable through the reading, so the collector cannot reclaim what is being measured. `beforeBaseline` runs ahead
    * of the baseline reading to displace anything the previous build left pinned.
    */
  def retainedBytes(build: () => AnyRef, beforeBaseline: () => Unit = () => ()): Long =
    beforeBaseline()
    val baseline = settledUsedBytes()
    val held     = build()
    val after    = settledUsedBytes()
    Reference.reachabilityFence(held)
    (after - baseline).max(0L)

end HeapFootprint
