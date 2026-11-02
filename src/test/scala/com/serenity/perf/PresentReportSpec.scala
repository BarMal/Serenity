package com.serenity.perf

import com.serenity.diagnostics.{FrameCounter, FrameCounts}
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

  "A row's shape traffic" should "give the mean and the largest frame of shapings and promotions" in {
    val stats = PresentReport.ShapeTrafficStats.of(
      Vector(
        PresentReport.ShapeFigures(shapings = 0, promotions = 0),
        PresentReport.ShapeFigures(shapings = 3, promotions = 1),
        PresentReport.ShapeFigures(shapings = 1, promotions = 2)
      )
    )

    stats shouldBe PresentReport.ShapeTrafficStats(frames = 3, shapings = (4L, 3L), promotions = (3L, 2L))
  }

  it should "be empty for no frames" in {
    PresentReport.ShapeTrafficStats.of(Vector.empty) shouldBe
      PresentReport.ShapeTrafficStats(frames = 0, shapings = (0L, 0L), promotions = (0L, 0L))
  }

  "The shape traffic summary" should "name each figure's mean and largest frame over the frames it covers" in {
    val stats = PresentReport.ShapeTrafficStats(frames = 4, shapings = (6L, 3L), promotions = (0L, 0L))

    PresentReport.trafficLine(result.name, stats) shouldBe
      "shapes render.present.1080p_2x.typing_frame: shapings avg=1.5 max=3, promotions avg=0.0 max=0 over 4 frames"
  }

  "A phase log" should "keep the shapings and promotions of the frames it timed" in {
    val counts = scala.collection.mutable.Queue(
      FrameCounts(Map(FrameCounter.ShapingCalls -> 2L, FrameCounter.ShapePromotions -> 1L)),
      FrameCounts(Map(FrameCounter.ShapingCalls -> 0L))
    )
    val log = PresentBenchmarks.PhaseLog(() => Map.empty, () => counts.dequeue())

    val _ = log.timing(() => ())()
    val _ = log.timing(() => ())()

    log.traffic shouldBe PresentReport.ShapeTrafficStats(frames = 2, shapings = (2L, 2L), promotions = (1L, 1L))
  }
