package com.serenity.perf

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class SourceRowCountBenchmarksSpec extends AnyFlatSpec with Matchers:

  private lazy val benchmarks = SourceRowCountBenchmarks.benchmarks()

  "the source row-count scenarios" should "cover a source file with ligatures off and on" in {
    benchmarks.map(_.name) shouldBe List(
      "layout.row_count.source_ligatures_off",
      "layout.row_count.source_ligatures_on"
    )
  }

  it should "verify against the wrap of each line on its own" in
    benchmarks.foreach(benchmark => withClue(benchmark.name)(benchmark.verify()))

  it should "run repeatedly without failing" in
    benchmarks.foreach(benchmark => withClue(benchmark.name)((1 to 3).foreach(_ => benchmark.run())))
