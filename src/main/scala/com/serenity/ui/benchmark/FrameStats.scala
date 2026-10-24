package com.serenity.ui.benchmark

/** What a run of frames cost, in milliseconds, by nearest-rank percentile. */
final case class FrameStats(count: Int, p50Ms: Double, p95Ms: Double, maxMs: Double, dropped: Int)

object FrameStats:

  private val NanosPerMilli = 1_000_000.0

  def of(samplesNanos: Vector[Long], dropped: Int): FrameStats =
    val sorted = samplesNanos.sorted
    def at(share: Double): Double =
      sorted.lift(math.ceil(share * sorted.length).toInt - 1).fold(0.0)(_ / NanosPerMilli)
    FrameStats(sorted.length, at(0.50), at(0.95), sorted.lastOption.fold(0.0)(_ / NanosPerMilli), dropped)
