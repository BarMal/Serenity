package com.serenity.perf

import cats.effect.unsafe.implicits.global
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class CacheOwnershipBenchmarksSpec extends AnyFlatSpec with Matchers:

  private lazy val benchmarks = CacheOwnershipBenchmarks.benchmarks

  "the cache-ownership scenarios" should "cover anchor wrapping, every cold font probe and two runtimes" in {
    benchmarks.map(_.name) shouldBe List(
      "layout.anchor_wrap.warm_caret_lookup",
      "layout.anchor_wrap.after_edit.caret_lookup",
      "layout.anchor_wrap.after_edit.reduce_only",
      "layout.cold_font.glyph_advances",
      "layout.cold_font.shaping_barriers",
      "layout.cold_font.font_line_extents",
      "layout.cold_font.measured_layout_memo.proportional",
      "layout.cold_font.measured_layout_memo.monospaced",
      "layout.cold_font.prose_column",
      "layout.cold_font.text_layout_snapshot",
      "laptop.input.state_manager.two_runtimes.same_text",
      "laptop.input.state_manager.two_runtimes.different_text"
    )
  }

  it should "verify against their own fixtures" in
    benchmarks.foreach(benchmark => withClue(benchmark.name)(benchmark.verify()))

  it should "track allocation, so the ABBA allocation threshold has numbers to compare" in
    benchmarks.foreach(benchmark =>
      withClue(benchmark.name)(BenchmarkRunner.tracksAllocation(benchmark.name) shouldBe true)
    )

end CacheOwnershipBenchmarksSpec
