package com.serenity.perf

import java.nio.file.{Files, Paths}

import scala.jdk.CollectionConverters.*

import com.serenity.state.manager.RenderCaches
import com.serenity.ui.layout.WrappedLineCache
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ScalingFootprintSpec extends AnyFlatSpec with Matchers:

  private def wrapEntries(caches: RenderCaches): Int =
    caches.wrappedLines match
      case bounded: WrappedLineCache.Bounded => bounded.size
      case _                                 => -1

  "The scaling footprint scenarios" should "be one buffer and twenty, each drawn once, in code and in prose" in {
    ScalingFootprint.scenarios.map(_.name) shouldBe List(
      "footprint.buffers_1_shown.code",
      "footprint.buffers_1_shown.prose",
      "footprint.buffers_20_shown.code",
      "footprint.buffers_20_shown.prose"
    )
  }

  it should "all be on the record-only list" in {
    val prefixes = Files
      .readAllLines(Paths.get("scripts", "perf_record_only.txt"))
      .asScala
      .map(_.split("#", 2).head.trim)
      .filter(_.nonEmpty)

    ScalingFootprint.scenarios.map(_.name).filterNot(name => prefixes.exists(name.startsWith)) shouldBe Nil
  }

  it should "keep the state and the caches the drawn buffers filled, which is what a session leaves resident" in {
    val kept = ScalingFootprint.build(MultiBufferFixture.Kind.Prose, shown = 3, MultiBufferFixture.TestSize)

    kept.state.persisted.buffers should have size 3
    wrapEntries(kept.caches) should be > 0
    kept.frames shouldBe 3
  }

  it should "draw each buffer, so a cache entry exists for every one of them" in {
    val one   = ScalingFootprint.build(MultiBufferFixture.Kind.Prose, shown = 1, MultiBufferFixture.TestSize)
    val three = ScalingFootprint.build(MultiBufferFixture.Kind.Prose, shown = 3, MultiBufferFixture.TestSize)

    wrapEntries(three.caches) should be > wrapEntries(one.caches)
  }

  it should "be built into the footprint run" in {
    FootprintBenchmarks.scenarios.map(_.name) should contain allElementsOf ScalingFootprint.scenarios.map(_.name)
  }
