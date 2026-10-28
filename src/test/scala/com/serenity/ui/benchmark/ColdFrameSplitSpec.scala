package com.serenity.ui.benchmark

import com.serenity.diagnostics.FramePhaseClock
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

final class ColdFrameSplitSpec extends AnyFlatSpec with Matchers:

  private val Ms = 1_000_000L

  private def sample(renderMs: Long, presentedMs: Long, layoutMs: Long, drawMs: Long) =
    ColdFrameSplit.Sample(renderMs * Ms, presentedMs * Ms, FramePhaseClock.Snapshot(layoutMs * Ms, drawMs * Ms))

  "The split of cold frames" should "be absent when no cold frame was measured" in {
    ColdFrameSplit.of(Vector.empty) shouldBe None
  }

  it should "average the render call, its layout and drawing, and the wait to be presented" in {
    val split = ColdFrameSplit
      .of(Vector(sample(60, 90, 10, 40), sample(80, 100, 20, 50)))
      .getOrElse(fail("no split"))

    split.frames shouldBe 2
    split.renderMs shouldBe 70.0 +- 1e-9
    split.layoutMs shouldBe 15.0 +- 1e-9
    split.drawMs shouldBe 45.0 +- 1e-9
    split.presentMs shouldBe 25.0 +- 1e-9
  }

  it should "call what layout and drawing did not cover in the render call the rest" in {
    val split = ColdFrameSplit.of(Vector(sample(60, 70, 10, 40))).getOrElse(fail("no split"))

    split.otherMs shouldBe 10.0 +- 1e-9
  }

  it should "not count a frame presented before its render call returned as waiting" in {
    val split = ColdFrameSplit.of(Vector(sample(60, 55, 10, 40))).getOrElse(fail("no split"))

    split.presentMs shouldBe 0.0
  }

  it should "not report a negative rest when the phases overlap the clock's granularity" in {
    val split = ColdFrameSplit.of(Vector(sample(50, 60, 20, 31))).getOrElse(fail("no split"))

    split.otherMs shouldBe 0.0
  }
