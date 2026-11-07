package com.serenity.perf

import cats.effect.unsafe.implicits.global
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** The rows whose work runs off the benchmark's own thread report what every thread allocated (#2618). */
class AllThreadsRowsSpec extends AnyFlatSpec with Matchers:

  private def measuring(benchmarks: List[BenchmarkRunner.Benchmark]): List[String] =
    benchmarks.filter(_.allThreads).map(_.name)

  "the keystroke rows" should "measure every thread" in {
    measuring(LaptopFrameBenchmarks.typingBenchmarks) shouldBe LaptopFrameBenchmarks.typingBenchmarks.map(_.name)
    measuring(LigatureTypingBenchmarks.benchmarks) shouldBe List("laptop.render.typing_frame_2x.ligature_font")
  }

  "the go-to-line and index walk rows" should "measure every thread" in {
    measuring(GoToLineBenchmarks.benchmarks) shouldBe GoToLineBenchmarks.benchmarks.map(_.name)
    measuring(IndexWalkBenchmarks.benchmarks()) shouldBe IndexWalkBenchmarks.benchmarks().map(_.name)
  }

  "a benchmark declared without the flag" should "measure only its own thread" in {
    BenchmarkRunner.Benchmark("laptop.test.plain", 1, 1, () => (), () => ()).allThreads shouldBe false
  }
