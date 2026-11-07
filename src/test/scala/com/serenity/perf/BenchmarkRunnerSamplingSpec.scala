package com.serenity.perf

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class BenchmarkRunnerSamplingSpec extends AnyFlatSpec with Matchers:

  private val window = BenchmarkRunner.MinSamplingWindowNanos

  "enoughSamples" should "keep sampling a fast benchmark past its configured count until the window is covered" in {
    BenchmarkRunner.enoughSamples(collected = 8, minimum = 8, elapsedNanos = window / 10) shouldBe false
  }

  it should "stop once the window is covered and the configured count is met" in {
    BenchmarkRunner.enoughSamples(collected = 8, minimum = 8, elapsedNanos = window) shouldBe true
  }

  it should "never stop short of the configured count, however long the samples were" in {
    BenchmarkRunner.enoughSamples(collected = 7, minimum = 8, elapsedNanos = window * 10) shouldBe false
  }

  it should "stop at the sample cap even if the window is not covered" in {
    BenchmarkRunner.enoughSamples(collected = BenchmarkRunner.MaxSamples, minimum = 8, elapsedNanos = 0L) shouldBe true
  }

  it should "stop at the configured count for a benchmark whose every call changes what the next one measures" in {
    BenchmarkRunner.enoughSamples(collected = 8, minimum = 8, elapsedNanos = 0L, extendToWindow = false) shouldBe true
  }

  "the sampling window" should "span far longer than a stall the median must survive" in {
    BenchmarkRunner.MinSamplingWindowNanos should be >= 250_000_000L
  }

  "csvRow" should "print a nanosecond-scale p50 to the precision that tells 12 ns from 20 ns" in {
    val twelveNanos = result(p50Ms = 0.000012)
    val twentyNanos = result(p50Ms = 0.000020)
    BenchmarkRunner.csvRow(twelveNanos).split(',')(5).toDouble should not be BenchmarkRunner
      .csvRow(twentyNanos)
      .split(',')(5)
      .toDouble
    BenchmarkRunner.csvRow(twelveNanos).split(',')(5).toDouble shouldBe 0.000012 +- 1e-8
  }

  it should "report the number of samples actually taken" in {
    BenchmarkRunner.csvRow(result(p50Ms = 1.0, samples = 137)).split(',')(1) shouldBe "137"
  }

  "csvRow memory columns" should "append retained_bytes after the allocation columns" in {
    val row = BenchmarkRunner.csvRow(result(p50Ms = 1.0).copy(retainedBytes = Some(123456L)))
    row.split(",", -1).length shouldBe 13
    row.split(",", -1)(10) shouldBe "123456"
  }

  it should "leave retained_bytes empty for a timed benchmark" in {
    BenchmarkRunner.csvRow(result(p50Ms = 1.0)).split(",", -1)(10) shouldBe ""
  }

  "tracksAllocation" should "cover every laptop, render and command runner benchmark by prefix" in
    List(
      "laptop.render.new_frame",
      "laptop.input.anything",
      "render.markdown.live_preview",
      "command_runner.palette.keystroke",
      "layout.anchor_wrap.warm_caret_lookup",
      "layout.cold_font.glyph_advances"
    ).foreach(name => withClue(name)(BenchmarkRunner.tracksAllocation(name) shouldBe true))

  it should "keep the explicitly listed benchmarks and skip the rest" in {
    BenchmarkRunner.tracksAllocation("reducer.backspace") shouldBe true
    BenchmarkRunner.tracksAllocation("rope.large_json.search") shouldBe false
  }

  "the header" should "name the retained_bytes column last" in {
    BenchmarkRunner.csvHeader.split(',').last shouldBe "retained_bytes"
  }

  it should "append the all-threads allocation columns after retained_bytes, empty for a row measured on its own thread" in {
    val measured = BenchmarkRunner.csvRow(
      result(p50Ms = 1.0).copy(allocationAllThreadsP50Bytes = Some(700L), allocationAllThreadsP95Bytes = Some(900L))
    )
    measured.split(",", -1).slice(10, 13).toList shouldBe List("", "700", "900")
    BenchmarkRunner.csvRow(result(p50Ms = 1.0)).split(",", -1).slice(11, 13).toList shouldBe List("", "")
  }

  "the header" should "name the all-threads allocation columns after retained_bytes" in {
    BenchmarkRunner.csvHeader.split(',').toList.drop(10) shouldBe
      List("retained_bytes", "allocation_all_threads_p50_bytes", "allocation_all_threads_p95_bytes")
  }

  "allocatedAcrossThreads" should "sum what each thread allocated between the two readings" in {
    BenchmarkRunner.allocatedAcrossThreads(Map(1L -> 100L, 2L -> 50L), Map(1L -> 160L, 2L -> 50L)) shouldBe 60L
  }

  it should "count everything a thread started between the readings allocated" in {
    BenchmarkRunner.allocatedAcrossThreads(Map(1L -> 100L), Map(1L -> 110L, 7L -> 40L)) shouldBe 50L
  }

  it should "ignore a thread that ended between the readings" in {
    BenchmarkRunner.allocatedAcrossThreads(Map(1L -> 100L, 2L -> 500L), Map(1L -> 130L)) shouldBe 30L
  }

  "a benchmark measuring all threads" should "see what another thread allocates for it, which its own thread's count misses" in {
    val worker = java.util.concurrent.Executors.newSingleThreadExecutor()
    try
      val kept = java.util.concurrent.atomic.AtomicReference[Array[Byte]](Array.emptyByteArray)
      def offThread(): Unit =
        worker
          .submit(
            new Runnable:
              def run(): Unit = kept.set(new Array[Byte](200_000))
          )
          .get()
        ()
      def declared = BenchmarkRunner.Benchmark("laptop.test.off_thread", 2, 5, () => offThread(), () => offThread())
      val alone    = BenchmarkRunner.runBenchmark(declared)
      val all      = BenchmarkRunner.runBenchmark(declared.measuringAllThreads)
      alone.allocationAllThreadsP50Bytes shouldBe None
      alone.allocationP50Bytes.exists(_ < 200_000L) shouldBe true
      all.allocationAllThreadsP50Bytes.exists(_ >= 200_000L) shouldBe true
      all.allocationAllThreadsP95Bytes.exists(_ >= 200_000L) shouldBe true
      all.allocationP50Bytes shouldBe all.allocationP50Bytes.filter(_ < 200_000L)
    finally worker.shutdownNow()
  }

  it should "leave a benchmark that does not track allocation unmeasured" in {
    val declared = BenchmarkRunner.Benchmark("rope.test.untracked", 1, 3, () => (), () => 1).measuringAllThreads
    BenchmarkRunner.runBenchmark(declared).allocationAllThreadsP50Bytes shouldBe None
  }

  private def result(p50Ms: Double, samples: Int = 20): BenchmarkRunner.BenchmarkResult =
    BenchmarkRunner.BenchmarkResult(
      name = "x",
      iterations = samples,
      warmupInvocations = 100,
      batch = 1,
      minMs = p50Ms,
      p50Ms = p50Ms,
      p95Ms = p50Ms,
      maxMs = p50Ms,
      allocationP50Bytes = None,
      allocationP95Bytes = None
    )

  "slowdownFactor" should "read a factor above one" in {
    BenchmarkRunner.slowdownFactor(Some("2")) shouldBe Some(2.0)
  }

  it should "leave the benchmark untouched when unset, one, below one or not a number" in {
    List(None, Some("1"), Some("0.5"), Some("fast")).map(BenchmarkRunner.slowdownFactor) shouldBe List.fill(4)(None)
  }

end BenchmarkRunnerSamplingSpec
