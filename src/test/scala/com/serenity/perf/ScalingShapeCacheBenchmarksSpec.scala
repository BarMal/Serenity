package com.serenity.perf

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ScalingShapeCacheBenchmarksSpec extends AnyFlatSpec with Matchers:

  import ScalingShapeCacheBenchmarks.FrameTraffic

  private val Millisecond = 1_000_000L

  "ScalingShapeCacheBenchmarks.summarise" should "average the lookups, misses and older-generation hits over a pass's frames" in {
    val frames = Vector(
      FrameTraffic(lookups = 100, shapings = 10, promotions = 5, evictions = 0, nanos = 4 * Millisecond),
      FrameTraffic(lookups = 100, shapings = 30, promotions = 15, evictions = 2048, nanos = 8 * Millisecond),
      FrameTraffic(lookups = 100, shapings = 20, promotions = 10, evictions = 0, nanos = 6 * Millisecond)
    )

    val summary = ScalingShapeCacheBenchmarks.summarise(pass = 2, frames)

    summary.pass shouldBe 2
    summary.frames shouldBe 3
    summary.lookupsPerFrame shouldBe 100.0
    summary.missesPerFrame shouldBe 20.0
    summary.olderHitsPerFrame shouldBe 10.0
    summary.evictions shouldBe 2048L
    summary.p50Ms shouldBe 6.0
    summary.hitRate shouldBe 0.8
  }

  it should "report a hit rate of one for a pass with no lookups, not a division by zero" in {
    ScalingShapeCacheBenchmarks.summarise(1, Vector(FrameTraffic(0, 0, 0, 0, Millisecond))).hitRate shouldBe 1.0
  }

  "ScalingShapeCacheBenchmarks.trace" should "cycle the buffers and account for every lookup the cache counted" in {
    val (passes, totals) = ScalingShapeCacheBenchmarks.trace(
      MultiBufferFixture.Kind.Prose,
      buffers = 3,
      passes = 3,
      MultiBufferFixture.TestSize
    )

    passes should have size 3
    passes.foreach(_ should have size 3)
    passes.flatten.foreach(frame => frame.shapings should be <= frame.lookups)
    passes.head.map(_.shapings).sum should be > 0L
    passes.flatten.map(_.lookups).sum shouldBe totals.lookups
    passes.flatten.map(_.shapings).sum shouldBe totals.shapings
    passes.flatten.map(_.promotions).sum shouldBe totals.promotions
    passes.flatten.map(_.evictions).sum shouldBe totals.evictions
  }

  it should "shape less once the buffers have all been drawn, when they fit the cache" in {
    val (passes, _) = ScalingShapeCacheBenchmarks.trace(
      MultiBufferFixture.Kind.Prose,
      buffers = 3,
      passes = 2,
      MultiBufferFixture.TestSize
    )

    passes(1).map(_.shapings).sum should be < passes.head.map(_.shapings).sum
  }

  it should "look up far more text once every line needs shaping than for plain prose, which never reaches the cache" in {
    def lookups(shaped: Boolean): Long =
      ScalingShapeCacheBenchmarks
        .trace(MultiBufferFixture.Kind.Prose, buffers = 2, passes = 1, MultiBufferFixture.TestSize, shaped)
        ._2
        .lookups

    lookups(shaped = true) should be > lookups(shaped = false)
  }
