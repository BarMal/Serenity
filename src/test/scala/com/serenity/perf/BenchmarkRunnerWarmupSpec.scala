package com.serenity.perf

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** The extension is part of `runBenchmark` only when a session is handed in, and never for a benchmark whose calls grow
  * the state it measures.
  */
class BenchmarkRunnerWarmupSpec extends AnyFlatSpec with Matchers:

  private val quick = WarmupExtension.Settings.default.copy(roundNanos = 1_000_000L, windowRounds = 10, runCapNanos = 2_000_000_000L, rowCapNanos = 300_000_000L)

  private def benchmark(name: String, fixedSampleCount: Boolean, calls: java.util.concurrent.atomic.AtomicInteger) =
    BenchmarkRunner.Benchmark(name, 2, 3, () => (), () => calls.incrementAndGet(), fixedSampleCount = fixedSampleCount)

  "runBenchmark" should "spend warm-up budget on an ordinary benchmark" in {
    val session = WarmupExtension.Session(quick)
    val _       = BenchmarkRunner.runBenchmark(benchmark("ordinary", false, new java.util.concurrent.atomic.AtomicInteger), session)
    session.budget.spentNanos should be > 0L
  }

  it should "leave a benchmark that grows its own state alone, so its p50 does not drift with the extra calls" in {
    val session = WarmupExtension.Session(quick)
    val _       = BenchmarkRunner.runBenchmark(benchmark("grows", true, new java.util.concurrent.atomic.AtomicInteger), session)
    session.budget.spentNanos shouldBe 0L
  }

  it should "do nothing extra when the session is off" in {
    val session = WarmupExtension.Session.off
    val _       = BenchmarkRunner.runBenchmark(benchmark("ordinary", false, new java.util.concurrent.atomic.AtomicInteger), session)
    session.budget.spentNanos shouldBe 0L
  }

  "runMatching" should "not extend anything unless it is given a session" in {
    val calls = new java.util.concurrent.atomic.AtomicInteger
    val _     = BenchmarkRunner.runMatching(Nil, List(benchmark("plain", false, calls)))
    WarmupExtension.Session.off.budget.spentNanos shouldBe 0L
  }
