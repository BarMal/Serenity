package com.serenity.perf

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class IndexWalkBenchmarksSpec extends AnyFlatSpec with Matchers:

  private lazy val small = IndexWalkBenchmarks.benchmarks(TreeShape.Small, "small")

  "the index-walk scenarios" should "be a cold and a warm walk of the 100k tree, under names the gate can pair" in {
    IndexWalkBenchmarks.benchmarks().map(_.name) shouldBe List("index.walk.cold.100k", "index.walk.warm.100k")
  }

  it should "name a smaller fixture's rows after its label" in {
    small.map(_.name) shouldBe List("index.walk.cold.small", "index.walk.warm.small")
  }

  it should "verify against their own fixture" in
    small.foreach(benchmark => withClue(benchmark.name)(benchmark.verify()))

  it should "run repeatedly without failing" in
    small.foreach(benchmark => withClue(benchmark.name)((1 to 3).foreach(_ => benchmark.run())))

  it should "track allocation, so the ABBA allocation threshold has numbers to compare" in
    IndexWalkBenchmarks
      .benchmarks()
      .foreach(benchmark => withClue(benchmark.name)(BenchmarkRunner.tracksAllocation(benchmark.name) shouldBe true))
