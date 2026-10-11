package com.serenity.perf

/** Latency statistics and spike attribution for the rope edit benchmark, free of timing and IO so they can be tested
  * exactly.
  */
private[perf] object RopeEditStatistics:

  val SpikeThresholdNanos: Long = 1_000_000L

  /** One timed edit and what happened around it. */
  final case class Sample(nanos: Long, rebuilds: Long, collections: Long)

  final case class Spikes(total: Int, withRebuild: Int, withCollection: Int, unexplained: Int, firstHundredEdits: Int)

  final case class Percentiles(p50: Long, p99: Long, p999: Long, max: Long)

  def percentiles(samples: Vector[Sample]): Percentiles =
    val sorted = samples.map(_.nanos).sorted
    Percentiles(
      nearestRank(sorted, 0.50),
      nearestRank(sorted, 0.99),
      nearestRank(sorted, 0.999),
      sorted.lastOption.getOrElse(0L)
    )

  def spikes(samples: Vector[Sample]): Spikes =
    val slow = samples.zipWithIndex.filter((sample, _) => sample.nanos > SpikeThresholdNanos)
    Spikes(
      total = slow.size,
      withRebuild = slow.count((sample, _) => sample.rebuilds > 0),
      withCollection = slow.count((sample, _) => sample.collections > 0),
      unexplained = slow.count((sample, _) => sample.rebuilds == 0 && sample.collections == 0),
      firstHundredEdits = slow.count((_, index) => index < 100)
    )

  private def nearestRank(sorted: Vector[Long], percentile: Double): Long =
    if sorted.isEmpty then 0L else sorted((math.ceil(percentile * sorted.size).toInt - 1).max(0))

end RopeEditStatistics
