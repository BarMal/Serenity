package com.serenity.perf

/** Latency statistics for the typing profile, kept free of timing and IO so they can be tested exactly. */
private[perf] object TypingStatistics:

  final case class Summary(count: Int, meanMs: Double, p50Ms: Double, p95Ms: Double)

  /** A range of keystrokes, numbered from 1 and inclusive at both ends. */
  final case class Bucket(first: Int, last: Int):
    def label: String = s"$first-$last"

  object Bucket:
    val cold: Vector[Bucket] =
      Vector(Bucket(1, 50), Bucket(51, 100), Bucket(101, 200), Bucket(201, 400), Bucket(401, 600))

  final case class BucketSummary(bucket: Bucket, summary: Option[Summary])

  def summarize(samplesMs: Vector[Double]): Option[Summary] =
    Option.when(samplesMs.nonEmpty) {
      val sorted = samplesMs.sorted
      Summary(sorted.size, sorted.sum / sorted.size, nearestRank(sorted, 0.50), nearestRank(sorted, 0.95))
    }

  def bucketed(samplesMs: Vector[Double], buckets: Vector[Bucket]): Vector[BucketSummary] =
    buckets.map(bucket => BucketSummary(bucket, summarize(samplesMs.slice(bucket.first - 1, bucket.last))))

  private def nearestRank(sorted: Vector[Double], percentile: Double): Double =
    sorted((math.ceil(percentile * sorted.size).toInt - 1).max(0))

end TypingStatistics
