package com.serenity.perf

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class BoundaryCacheBenchmarksSpec extends AnyFlatSpec with Matchers:

  private lazy val benchmarks = BoundaryCacheBenchmarks.benchmarks()

  "the boundary-cache scenarios" should "cover word and grapheme motion, a keystroke after an edit, whole-word find and rope counts" in {
    benchmarks.map(_.name) shouldBe List(
      "text.boundary.word_motion",
      "text.boundary.grapheme_motion",
      "text.boundary.keystroke_after_edit",
      "text.boundary.find_whole_word",
      "text.counts.rope_build",
      "text.counts.rope_edit"
    )
  }

  it should "verify against their own fixtures" in
    benchmarks.foreach(benchmark => withClue(benchmark.name)(benchmark.verify()))

  it should "run repeatedly without failing" in
    benchmarks.foreach(benchmark => withClue(benchmark.name)((1 to 5).foreach(_ => benchmark.run())))

  it should "track allocation, so the ABBA allocation threshold has numbers to compare" in
    benchmarks.foreach(benchmark =>
      withClue(benchmark.name)(BenchmarkRunner.tracksAllocation(benchmark.name) shouldBe true)
    )
