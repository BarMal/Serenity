package com.serenity.ui.benchmark

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class FrameStatsSpec extends AnyFlatSpec with Matchers:

  private def millis(values: Int*): Vector[Long] = values.toVector.map(_ * 1_000_000L)

  "FrameStats.of" should "take the nearest-rank percentiles of the frame times, in milliseconds" in {
    val stats = FrameStats.of(millis(1 to 100*), dropped = 0)
    stats.count shouldBe 100
    stats.p50Ms shouldBe 50.0
    stats.p95Ms shouldBe 95.0
    stats.maxMs shouldBe 100.0
  }

  it should "not depend on the order the frames were timed in" in {
    FrameStats.of(millis(5, 1, 9, 3, 7), dropped = 0) shouldBe FrameStats.of(millis(1, 3, 5, 7, 9), dropped = 0)
  }

  it should "give a single frame's time for every figure" in {
    val stats = FrameStats.of(millis(12), dropped = 0)
    (stats.p50Ms, stats.p95Ms, stats.maxMs) shouldBe (12.0, 12.0, 12.0)
  }

  it should "keep the number of frames that were dropped" in {
    FrameStats.of(millis(1, 2, 3), dropped = 4).dropped shouldBe 4
  }

  it should "report nothing measured for no frames" in {
    val stats = FrameStats.of(Vector.empty, dropped = 2)
    (stats.count, stats.p50Ms, stats.p95Ms, stats.maxMs, stats.dropped) shouldBe (0, 0.0, 0.0, 0.0, 2)
  }
