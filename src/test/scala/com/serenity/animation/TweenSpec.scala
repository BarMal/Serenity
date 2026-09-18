package com.serenity.animation

import com.serenity.ui.layout.{LayoutRect, PixelRect}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** [[Tween]] is the generalised, curve-aware replacement for [[ScalarTimeline]] (issue #1083): the same tick-driven,
  * fixed-step shape (`progress`/`advance`/`isComplete`), but generic over any `A` with an [[Interpolator]] instance,
  * and with mid-flight `retarget` support that `ScalarTimeline` never needed (nothing built on it retargeted).
  */
class TweenSpec extends AnyFlatSpec with Matchers:

  "a Tween[Double]" should "start at its start value, at zero progress" in {
    val tween = Tween(start = 0.0, end = 10.0, curve = EasingCurve.Linear, steps = 4)
    tween.progress shouldBe 0.0
    tween.currentValue shouldBe 0.0
  }

  it should "advance progress by one step at a time" in {
    val tween = Tween(start = 0.0, end = 10.0, curve = EasingCurve.Linear, steps = 4)
    tween.advance.progress shouldBe 0.25
    tween.advance.advance.progress shouldBe 0.5
  }

  it should "never advance progress past 1.0" in {
    val tween    = Tween(start = 0.0, end = 10.0, curve = EasingCurve.Linear, steps = 2)
    val advanced = tween.advance.advance.advance.advance
    advanced.progress shouldBe 1.0
    advanced.currentValue shouldBe 10.0
  }

  it should "be complete once its steps are exhausted" in {
    val tween = Tween(start = 0.0, end = 10.0, curve = EasingCurve.Linear, steps = 2)
    tween.isComplete shouldBe false
    tween.advance.isComplete shouldBe false
    tween.advance.advance.isComplete shouldBe true
  }

  it should "treat zero or fewer steps as already complete, at the end value" in {
    Tween(start = 0.0, end = 10.0, curve = EasingCurve.Linear, steps = 0).isComplete shouldBe true
    Tween(start = 0.0, end = 10.0, curve = EasingCurve.Linear, steps = 0).progress shouldBe 1.0
    Tween(start = 0.0, end = 10.0, curve = EasingCurve.Linear, steps = -3).isComplete shouldBe true
  }

  it should "linearly interpolate its value under the Linear curve" in {
    val tween = Tween(start = 0.0, end = 10.0, curve = EasingCurve.Linear, steps = 4, currentFrame = 1)
    tween.currentValue shouldBe 2.5
  }

  it should "apply its curve to progress before interpolating" in {
    val tween = Tween(start = 0.0, end = 10.0, curve = EasingCurve.EaseIn, steps = 4, currentFrame = 2)
    // progress = 0.5, EaseIn(0.5) = 0.125
    tween.currentValue shouldBe 1.25
  }

  "retarget" should "continue smoothly from the current value rather than snapping back to start" in {
    val tween           = Tween(start = 0.0, end = 10.0, curve = EasingCurve.Linear, steps = 4).advance.advance
    val valueAtRetarget = tween.currentValue
    valueAtRetarget shouldBe 5.0

    val retargeted = tween.retarget(20.0)

    retargeted.start shouldBe valueAtRetarget
    retargeted.end shouldBe 20.0
    retargeted.currentFrame shouldBe 0
    retargeted.currentValue shouldBe valueAtRetarget
  }

  it should "spend only the remaining steps, not the full original count, on the new leg" in {
    val tween      = Tween(start = 0.0, end = 10.0, curve = EasingCurve.Linear, steps = 10, currentFrame = 8)
    val retargeted = tween.retarget(20.0)
    retargeted.steps shouldBe 2 // 10 - 8 remaining
  }

  it should "give a completed tween a fresh full-length leg toward the new target" in {
    val completed =
      Tween(start = 0.0, end = 10.0, curve = EasingCurve.Linear, steps = 4).advance.advance.advance.advance
    completed.isComplete shouldBe true

    val retargeted = completed.retarget(20.0)

    retargeted.start shouldBe 10.0
    retargeted.end shouldBe 20.0
    retargeted.steps shouldBe 4
    retargeted.currentFrame shouldBe 0
  }

  "Interpolator[Double]" should "lerp linearly" in {
    summon[Interpolator[Double]].lerp(0.0, 10.0, 0.5) shouldBe 5.0
  }

  "Interpolator[Float]" should "lerp linearly, reusing Double's math" in {
    summon[Interpolator[Float]].lerp(0.0f, 10.0f, 0.5) shouldBe 5.0f
  }

  "Interpolator[LayoutRect]" should "lerp each field independently, rounding to the nearest cell" in {
    val start = LayoutRect(0, 0, 10, 10)
    val end   = LayoutRect(10, 20, 20, 30)
    val mid   = summon[Interpolator[LayoutRect]].lerp(start, end, 0.5)
    mid shouldBe LayoutRect(5, 10, 15, 20)
  }

  it should "reach start and end exactly at t=0 and t=1" in {
    val start = LayoutRect(1, 2, 3, 4)
    val end   = LayoutRect(9, 8, 7, 6)
    summon[Interpolator[LayoutRect]].lerp(start, end, 0.0) shouldBe start
    summon[Interpolator[LayoutRect]].lerp(start, end, 1.0) shouldBe end
  }

  "Interpolator[PixelRect]" should "lerp each field independently, rounding to the nearest pixel" in {
    val start = PixelRect(0, 0, 100, 100)
    val end   = PixelRect(100, 200, 300, 400)
    val mid   = summon[Interpolator[PixelRect]].lerp(start, end, 0.5)
    mid shouldBe PixelRect(50, 100, 200, 250)
  }

  "a Tween[LayoutRect]" should "interpolate a whole rect over its steps" in {
    val start = LayoutRect(0, 0, 10, 10)
    val end   = LayoutRect(20, 0, 10, 10)
    val tween = Tween(start = start, end = end, curve = EasingCurve.Linear, steps = 2, currentFrame = 1)
    tween.currentValue shouldBe LayoutRect(10, 0, 10, 10)
  }
