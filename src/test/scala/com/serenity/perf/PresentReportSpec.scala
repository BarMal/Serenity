package com.serenity.perf

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class PresentReportSpec extends AnyFlatSpec with Matchers:

  private val result = BenchmarkRunner.BenchmarkResult(
    name = "render.present.1080p_2x.typing_frame",
    iterations = 20,
    warmupInvocations = 100,
    batch = 1,
    minMs = 1.0,
    p50Ms = 2.0,
    p95Ms = 3.0,
    maxMs = 4.0,
    allocationP50Bytes = Some(1024L),
    allocationP95Bytes = Some(2048L)
  )

  private def frame(plan: Long, move: Long, shift: Long, copy: Long, total: Long) =
    PresentReport.FramePhases(plan, move, shift, copy, total)

  private val frames = Vector(
    frame(50_000, 0, 0, 1_000_000, 8_000_000),
    frame(70_000, 0, 0, 1_400_000, 8_200_000),
    frame(60_000, 0, 0, 1_200_000, 7_800_000)
  )

  "The breakdown of a row's frames" should "take each phase's median and leave the rest to the frame" in {
    val breakdown = PresentReport.PhaseBreakdown.median(frames)
    breakdown shouldBe PresentReport.PhaseBreakdown(0.06, 0.0, 0.0, 1.2, 8.0)
    breakdown.restMs shouldBe (8.0 - 0.06 - 1.2) +- 1e-9
  }

  it should "never report a negative rest when the phases add up to more than the frame" in {
    PresentReport.PhaseBreakdown(1.0, 1.0, 1.0, 1.0, 3.0).restMs shouldBe 0.0
  }

  it should "be empty for no frames" in {
    PresentReport.PhaseBreakdown.median(Vector.empty) shouldBe PresentReport.PhaseBreakdown(0.0, 0.0, 0.0, 0.0, 0.0)
  }

  "The present CSV" should "carry the phases in columns after the ones a reader of the plain layout parses" in {
    PresentReport.csvHeader shouldBe BenchmarkRunner.csvHeader +
      ",plan_ms,surface_move_ms,image_shift_ms,copy_out_ms,rest_ms"
  }

  it should "write a row as the plain row followed by the phases in milliseconds" in {
    val breakdown = PresentReport.PhaseBreakdown(0.06, 0.5, 2.25, 1.2, 8.0)
    PresentReport.csvRow(result, Some(breakdown)) shouldBe
      BenchmarkRunner.csvRow(result) + ",0.0600000,0.5000000,2.2500000,1.2000000,3.9900000"
  }

  it should "leave the phase columns empty for a row with no breakdown" in {
    PresentReport.csvRow(result, None) shouldBe BenchmarkRunner.csvRow(result) + ",,,,,"
  }

  "The present summary" should "name each phase of a row's frame in milliseconds" in {
    val breakdown = PresentReport.PhaseBreakdown(0.06, 0.5, 2.25, 1.2, 8.0)
    PresentReport.summaryLine(result.name, breakdown) shouldBe
      "phases render.present.1080p_2x.typing_frame: plan 0.06 ms, surface move 0.50 ms, image shift 2.25 ms, " +
      "copy-out 1.20 ms, rest 3.99 ms, frame 8.00 ms"
  }
