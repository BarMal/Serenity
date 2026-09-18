package com.serenity.animation

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** A minimal, colour-free counterpart to [[ColorTimeline]]: a tick-driven scalar progress value in `[0.0, 1.0]`, for
  * animations that interpolate a plain number (a pixel/column offset, say) rather than a colour. `ColorTimeline` itself
  * doesn't generalise to this -- it is built entirely around `RgbInterpolator` -- so this is the "minimal primitive
  * this feature needs" rather than a reuse of it.
  */
class ScalarTimelineSpec extends AnyFlatSpec with Matchers:

  "ScalarTimeline" should "start at zero progress" in {
    ScalarTimeline(steps = 4).progress shouldBe 0.0
  }

  it should "advance progress by one step at a time" in {
    val timeline = ScalarTimeline(steps = 4)
    timeline.advance.progress shouldBe 0.25
    timeline.advance.advance.progress shouldBe 0.5
  }

  it should "never advance progress past 1.0" in {
    val timeline = ScalarTimeline(steps = 2)
    val advanced = timeline.advance.advance.advance.advance
    advanced.progress shouldBe 1.0
  }

  it should "be complete once its steps are exhausted" in {
    val timeline = ScalarTimeline(steps = 2)
    timeline.isComplete shouldBe false
    timeline.advance.isComplete shouldBe false
    timeline.advance.advance.isComplete shouldBe true
  }

  it should "treat zero or fewer steps as already complete, at full progress" in {
    ScalarTimeline(steps = 0).isComplete shouldBe true
    ScalarTimeline(steps = 0).progress shouldBe 1.0
    ScalarTimeline(steps = -3).isComplete shouldBe true
  }
