package com.serenity.ui.skia

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** What the first use of Skia text costs an owner's caches, counted without Skia. */
class WarmupStatsSpec extends AnyFlatSpec with Matchers:

  "Warm-up stats" should "start at nothing" in:
    new WarmupStats().snapshot shouldBe WarmupStats.Snapshot.empty

  it should "add up the faces built, the shapers made and the text shaped" in:
    val stats = new WarmupStats()
    stats.faceBuilt(2_000_000L)
    stats.faceBuilt(3_000_000L)
    stats.shaperMade(500_000L)
    stats.shaped(100_000L)
    stats.shaped(150_000L)
    stats.shaped(50_000L)
    stats.snapshot shouldBe WarmupStats.Snapshot(
      faces = 2,
      faceNanos = 5_000_000L,
      shapers = 1,
      shaperNanos = 500_000L,
      shapings = 3,
      shapingNanos = 300_000L
    )

  "A snapshot" should "add to another, as the stats of several owners do" in:
    val one = WarmupStats.Snapshot(1, 10L, 1, 20L, 2, 30L)
    val two = WarmupStats.Snapshot(3, 100L, 0, 0L, 5, 300L)
    one + two shouldBe WarmupStats.Snapshot(4, 110L, 1, 20L, 7, 330L)
    one + WarmupStats.Snapshot.empty shouldBe one
