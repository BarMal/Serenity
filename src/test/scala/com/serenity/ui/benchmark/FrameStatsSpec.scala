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

  it should "take the tail of the frame times and count the frames slower than 33 ms" in {
    val stats = FrameStats.of(millis(1 to 100*), dropped = 0)
    (stats.p75Ms, stats.p90Ms, stats.p99Ms) shouldBe (75.0, 90.0, 99.0)
    stats.over33Ms shouldBe 67
    FrameStats.of(millis(33, 34), dropped = 0).over33Ms shouldBe 1
  }

  it should "keep the number of frames that ended before the render returned" in {
    FrameStats.of(millis(1, 2, 3), dropped = 0, endedBeforeReturn = 2).endedBeforeReturn shouldBe 2
  }

  it should "round each frame time up to whole refresh intervals and count the frames that miss one or more" in {
    val sixty = RefreshRate.Assumed60.nanos
    val stats =
      FrameStats.of(Vector(16_000_000L, 16_666_667L, 16_700_000L, 33_400_000L), dropped = 0, refreshNanos = sixty)
    (stats.p50Frames, stats.p95Frames) shouldBe (1, 3)
    stats.missedRefresh shouldBe 2
  }

  it should "count a frame as presented within a refresh when no frame was timed to take longer" in {
    val stats = FrameStats.of(millis(1, 5, 16), dropped = 0)
    (stats.p50Frames, stats.p95Frames, stats.missedRefresh) shouldBe (1, 1, 0)
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
