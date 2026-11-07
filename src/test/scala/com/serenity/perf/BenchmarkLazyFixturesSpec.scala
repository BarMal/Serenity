package com.serenity.perf

import java.util.concurrent.ConcurrentLinkedQueue

import scala.jdk.CollectionConverters.*

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** A benchmark is its name and settings until it runs: what it measures is built by the runner, before the timed calls
  * and so outside what is timed, which lets the names be read with no window and no fixture.
  */
class BenchmarkLazyFixturesSpec extends AnyFlatSpec with Matchers:

  final private class Events:
    private val seen = new ConcurrentLinkedQueue[String]

    def add(event: String): Unit =
      val _ = seen.add(event)
    def all: List[String]         = seen.asScala.toList
    def count(event: String): Int = all.count(_ == event)

  private def family(events: Events, buildMillis: Long = 0L): List[BenchmarkRunner.Benchmark] =
    lazy val fixture =
      events.add("fixture")
      if buildMillis > 0 then Thread.sleep(buildMillis)
      41
    BenchmarkRunner.Benchmark.sharing(fixture)(
      List(
        BenchmarkRunner.Benchmark(
          "first",
          2,
          3,
          () =>
            events.add("verify first")
            assert(fixture == 41)
          ,
          () => fixture + 1
        ),
        BenchmarkRunner.Benchmark("second", 2, 3, () => assert(fixture == 41), () => fixture + 2)
      )
    )

  "A declared benchmark" should "build no fixture until it runs" in {
    val events = new Events
    family(events).map(_.name) shouldBe List("first", "second")
    events.all shouldBe Nil
  }

  it should "have its fixture built before it is verified" in {
    val events = new Events
    val _      = BenchmarkRunner.runBenchmark(family(events).head)
    events.all.take(2) shouldBe List("fixture", "verify first")
  }

  it should "have its fixture built outside the warm-up, which a slow build must not shorten" in {
    val result = BenchmarkRunner.runBenchmark(family(new Events, buildMillis = 700L).head)
    // A fixture built by the first warm-up call would use up the 500 ms warm-up budget and leave only the minimum.
    result.warmupInvocations should be > 2
  }

  "Benchmarks sharing a fixture" should "build it once, for whichever runs first" in {
    val events              = new Events
    val List(first, second) = family(events)
    val _                   = BenchmarkRunner.runBenchmark(second)
    events.count("fixture") shouldBe 1
    val _ = BenchmarkRunner.runBenchmark(first)
    events.count("fixture") shouldBe 1
  }

  they should "build nothing for a benchmark that is not selected" in {
    val events = new Events
    val _      = BenchmarkRunner.runMatching(List("none"), family(events))
    events.all shouldBe Nil
  }

  "A benchmark declared lazily" should "build its body once, when it runs and before it is verified" in {
    val events = new Events
    val benchmark = BenchmarkRunner.Benchmark.lazily("alone", 2, 3):
      events.add("body")
      BenchmarkRunner.Body(() => events.add("verify"), () => 1)
    events.all shouldBe Nil
    val _ = BenchmarkRunner.runBenchmark(benchmark)
    events.all.take(2) shouldBe List("body", "verify")
    events.count("body") shouldBe 1
  }

  "A benchmark with a fixture of its own" should "build it when it runs, and only then" in {
    val events = new Events
    val benchmark = BenchmarkRunner.Benchmark.building { events.add("fixture"); 1 }(
      BenchmarkRunner.Benchmark("alone", 2, 3, () => (), () => 1)
    )
    events.all shouldBe Nil
    val _ = BenchmarkRunner.runBenchmark(benchmark)
    events.count("fixture") shouldBe 1
  }
