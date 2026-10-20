package com.serenity.perf

import com.serenity.perf.RopeEditStatistics.{Percentiles, Sample, Spikes}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class RopeEditStatisticsSpec extends AnyFlatSpec with Matchers:

  private def quiet(nanos: Long): Sample = Sample(nanos, rebuilds = 0, collections = 0)

  "percentiles" should "take nearest-rank p50, p99 and p99.9 and the maximum" in {
    val samples = (1L to 1000L).toVector.map(quiet)
    RopeEditStatistics.percentiles(samples) shouldBe Percentiles(p50 = 500, p99 = 990, p999 = 999, max = 1000)
  }

  it should "not depend on the order of the samples" in {
    val samples = (1L to 1000L).toVector.map(quiet)
    RopeEditStatistics.percentiles(samples.reverse) shouldBe RopeEditStatistics.percentiles(samples)
  }

  it should "be zero for no samples" in {
    RopeEditStatistics.percentiles(Vector.empty) shouldBe Percentiles(0, 0, 0, 0)
  }

  "spikes" should "count only edits strictly over a millisecond" in {
    val samples = Vector(quiet(1_000_000L), quiet(1_000_001L), quiet(5_000_000L))
    RopeEditStatistics.spikes(samples).total shouldBe 2
  }

  it should "attribute each slow edit to a rebuild, a collection, or neither" in {
    val samples = Vector(
      Sample(2_000_000L, rebuilds = 1, collections = 0),
      Sample(2_000_000L, rebuilds = 0, collections = 1),
      Sample(2_000_000L, rebuilds = 1, collections = 1),
      Sample(2_000_000L, rebuilds = 0, collections = 0),
      Sample(10L, rebuilds = 3, collections = 0)
    )
    RopeEditStatistics.spikes(samples) shouldBe Spikes(
      total = 4,
      withRebuild = 2,
      withCollection = 2,
      unexplained = 1,
      firstHundredEdits = 4
    )
  }

  it should "count slow edits among the first hundred as warm-up suspects" in {
    val samples = Vector.fill(150)(quiet(10L)).updated(99, quiet(2_000_000L)).updated(100, quiet(2_000_000L))
    RopeEditStatistics.spikes(samples).firstHundredEdits shouldBe 1
  }

end RopeEditStatisticsSpec
