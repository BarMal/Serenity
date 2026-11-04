package com.serenity.ui.benchmark

/** What a run of frames cost, in milliseconds, by nearest-rank percentile, with the frames slower than 33 ms (two
  * display refreshes) and the frames whose last presentation came before the render call returned. The frame counts
  * round a time up to whole refresh intervals, and `missedRefresh` is the frames that took more than one.
  */
final case class FrameStats(
    count: Int,
    p50Ms: Double,
    p95Ms: Double,
    maxMs: Double,
    dropped: Int,
    p75Ms: Double = 0.0,
    p90Ms: Double = 0.0,
    p99Ms: Double = 0.0,
    over33Ms: Int = 0,
    endedBeforeReturn: Int = 0,
    p50Frames: Int = 0,
    p95Frames: Int = 0,
    missedRefresh: Int = 0
)

object FrameStats:

  private val NanosPerMilli = 1_000_000.0
  private val SlowFrameMs   = 33.0

  def of(
    samplesNanos: Vector[Long],
    dropped: Int,
    endedBeforeReturn: Int = 0,
    refreshNanos: Long = RefreshRate.Assumed60.nanos
  ): FrameStats =
    val sorted = samplesNanos.sorted
    def at(share: Double): Double =
      sorted.lift(math.ceil(share * sorted.length).toInt - 1).fold(0.0)(_ / NanosPerMilli)
    def frames(nanos: Long): Int = math.max(1, math.ceil(nanos.toDouble / refreshNanos).toInt)
    def framesAt(share: Double): Int =
      sorted.lift(math.ceil(share * sorted.length).toInt - 1).fold(0)(frames)
    FrameStats(
      sorted.length,
      at(0.50),
      at(0.95),
      sorted.lastOption.fold(0.0)(_ / NanosPerMilli),
      dropped,
      p75Ms = at(0.75),
      p90Ms = at(0.90),
      p99Ms = at(0.99),
      over33Ms = sorted.count(_ / NanosPerMilli > SlowFrameMs),
      endedBeforeReturn = endedBeforeReturn,
      p50Frames = framesAt(0.50),
      p95Frames = framesAt(0.95),
      missedRefresh = sorted.count(nanos => frames(nanos) > 1)
    )
