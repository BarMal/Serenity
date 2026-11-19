package com.serenity.perf

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class PresentRowsSpec extends AnyFlatSpec with Matchers:

  private val Ms = 1_000_000L

  private def frame(start: Long, draw: Long, handoff: Long, present: Long, swap: Long): FrameStamps =
    val drawn        = start + draw * Ms
    val presentStart = drawn + handoff * Ms
    val presentEnd   = presentStart + present * Ms
    FrameStamps(start, drawn, presentStart, presentEnd, presentEnd + swap * Ms)

  "FrameStamps" should "split a frame into draw, handoff, present and swap that add up to draw-to-rendered" in {
    val times = frame(start = 0L, draw = 4, handoff = 1, present = 3, swap = 12).times
    times shouldBe FrameTimes(drawMs = 4.0, handoffMs = 1.0, presentMs = 3.0, swapMs = 12.0)
    times.drawToRenderedMs shouldBe 20.0
  }

  it should "say whether its stamps run in order" in {
    frame(0L, 1, 1, 1, 1).isOrdered shouldBe true
    frame(0L, 1, 1, 1, 1).copy(presentEnd = -1L).isOrdered shouldBe false
    frame(0L, 1, 1, 1, 1).copy(swapEnd = 0L).isOrdered shouldBe false
  }

  "PresentRows.of" should "take nearest-rank p50 and p95 of each row over the frames" in {
    val frames = (1 to 20).toVector.map(n => frame(start = n * 100 * Ms, draw = n, handoff = 1, present = 2, swap = 10))
    val rows   = PresentRows.of(frames)
    rows.draw.p50Ms shouldBe 10.0
    rows.draw.p95Ms shouldBe 19.0
    rows.present.p50Ms shouldBe 2.0
    rows.swap.p95Ms shouldBe 10.0
    rows.drawToRendered.p95Ms shouldBe 32.0
    rows.draw.count shouldBe 20
  }

  it should "measure the frame interval from one swap's end to the next's" in {
    val first  = frame(start = 0L, draw = 1, handoff = 0, present = 1, swap = 1)
    val second = frame(start = 100 * Ms, draw = 1, handoff = 0, present = 1, swap = 1)
    val third  = frame(start = 120 * Ms, draw = 1, handoff = 0, present = 1, swap = 1)
    val rows   = PresentRows.of(Vector(first, second, third))
    rows.interval.count shouldBe 2
    rows.interval.p50Ms shouldBe 20.0
    rows.interval.maxMs shouldBe 100.0
  }

  it should "have no rows for no frames" in {
    PresentRows.of(Vector.empty).drawToRendered.count shouldBe 0
    PresentRows.of(Vector.empty).interval.count shouldBe 0
  }

  it should "describe itself as one line of p50/p95 per row" in {
    val line = PresentRows.of(Vector(frame(0L, 4, 1, 3, 12))).describe("vsync=1 typing")
    line should startWith("vsync=1 typing")
    line should include("draw 4.00/4.00")
    line should include("present 3.00/3.00")
    line should include("swap 12.00/12.00")
    line should include("draw_to_rendered 20.00/20.00")
    line should endWith("(n=1)")
  }
