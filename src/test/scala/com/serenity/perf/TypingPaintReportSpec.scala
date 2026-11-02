package com.serenity.perf

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class TypingPaintReportSpec extends AnyFlatSpec with Matchers:

  private val timings = List(
    TypingPaintReport.VariantTiming("full", 7.8, 9.1),
    TypingPaintReport.VariantTiming("minimal", 5.2, 6.0),
    TypingPaintReport.VariantTiming("no_scrollbar", 7.3, 8.8)
  )

  "The variant table" should "give each variant's p50 and p95 and its p50 against the first row" in {
    TypingPaintReport.timingTable(timings) shouldBe
      """variant | p50 ms | p95 ms | p50 vs first
        |full | 7.80 | 9.10 | 1.00x
        |minimal | 5.20 | 6.00 | 0.67x
        |no_scrollbar | 7.30 | 8.80 | 0.94x""".stripMargin
  }

  it should "be only the header for no variants" in {
    TypingPaintReport.timingTable(Nil) shouldBe "variant | p50 ms | p95 ms | p50 vs first"
  }

  "The draw-count line" should "list the kinds a frame used, most frequent first, as per-frame averages" in {
    val perFrame = Map(
      DrawKind.PutString -> 12.0,
      DrawKind.DrawRunPx -> 40.5,
      DrawKind.FillRect  -> 0.0,
      DrawKind.ClipPush  -> 12.0
    )
    TypingPaintReport.drawCountsLine("full", perFrame) shouldBe
      "draws full: total 64.5 per frame: DrawRunPx 40.5, ClipPush 12.0, PutString 12.0"
  }

  it should "say so when a frame drew nothing" in {
    TypingPaintReport.drawCountsLine("minimal", Map(DrawKind.PutString -> 0.0)) shouldBe
      "draws minimal: total 0.0 per frame: none"
  }

  "The floor line" should "split a Skia typing frame into the renderer's own work and the paint and present" in {
    TypingPaintReport.floorLine(skiaP50Ms = 7.8, floorP50Ms = 2.4) shouldBe
      "floor: renderer-only 2.40 ms of the 7.80 ms Skia typing frame (30.8%); paint and present 5.40 ms"
  }

  it should "never report a negative paint when the floor is slower than the frame" in {
    TypingPaintReport.floorLine(skiaP50Ms = 2.0, floorP50Ms = 3.0) shouldBe
      "floor: renderer-only 3.00 ms of the 2.00 ms Skia typing frame (150.0%); paint and present 0.00 ms"
  }
