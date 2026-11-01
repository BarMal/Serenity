package com.serenity.perf

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class RowCountBenchmarksSpec extends AnyFlatSpec with Matchers:

  private lazy val benchmarks = RowCountBenchmarks.benchmarks()

  "the row-count scenarios" should "cover a cold count and a count after the panel grew" in {
    benchmarks.map(_.name) shouldBe List("layout.row_count.cold", "layout.row_count.after_grow")
  }

  it should "verify against a count with no cache" in
    benchmarks.foreach(benchmark => withClue(benchmark.name)(benchmark.verify()))

  it should "run repeatedly without failing" in
    benchmarks.foreach(benchmark => withClue(benchmark.name)((1 to 3).foreach(_ => benchmark.run())))
