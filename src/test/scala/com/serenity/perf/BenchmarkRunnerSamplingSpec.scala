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
    row.split(",", -1).length shouldBe 11
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

end BenchmarkRunnerSamplingSpec
