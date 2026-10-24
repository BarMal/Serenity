package com.serenity.ui.benchmark

/** What a run of frames cost, in milliseconds, by nearest-rank percentile, with the frames slower than 33 ms (two
  * display refreshes) and the frames whose last presentation came before the render call returned.
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
    endedBeforeReturn: Int = 0
)

object FrameStats:

  private val NanosPerMilli = 1_000_000.0
  private val SlowFrameMs   = 33.0

  def of(samplesNanos: Vector[Long], dropped: Int, endedBeforeReturn: Int = 0): FrameStats =
    val sorted = samplesNanos.sorted
    def at(share: Double): Double =
      sorted.lift(math.ceil(share * sorted.length).toInt - 1).fold(0.0)(_ / NanosPerMilli)
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
      endedBeforeReturn = endedBeforeReturn
    )
