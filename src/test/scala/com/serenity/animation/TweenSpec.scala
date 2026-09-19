package com.serenity.animation

import java.awt.Color

import com.serenity.ui.layout.{LayoutRect, PixelPoint, PixelRect}
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

  "Interpolator[PixelPoint]" should "lerp each field independently, rounding to the nearest pixel" in {
    val start = PixelPoint(0, 0)
    val end   = PixelPoint(100, 200)
    val mid   = summon[Interpolator[PixelPoint]].lerp(start, end, 0.5)
    mid shouldBe PixelPoint(50, 100)
  }

  it should "reach start and end exactly at t=0 and t=1" in {
    val start = PixelPoint(3, 4)
    val end   = PixelPoint(9, 6)
    summon[Interpolator[PixelPoint]].lerp(start, end, 0.0) shouldBe start
    summon[Interpolator[PixelPoint]].lerp(start, end, 1.0) shouldBe end
  }

  "a Tween[PixelPoint]" should "interpolate caret glide's on-screen position over its steps" in {
    val start = PixelPoint(0, 0)
    val end   = PixelPoint(20, 0)
    val tween = Tween(start = start, end = end, curve = EasingCurve.Linear, steps = 2, currentFrame = 1)
    tween.currentValue shouldBe PixelPoint(10, 0)
  }

  // ── Interpolator[Color] (issue #1574: colour joins the generic Tween primitive) ────────────────

  "Interpolator[Color]" should "lerp each RGBA component independently" in {
    val start = new Color(0, 0, 0, 0)
    val end   = new Color(255, 100, 50, 200)
    val mid   = summon[Interpolator[Color]].lerp(start, end, 0.5)
    mid shouldBe new Color(128, 50, 25, 100)
  }

  it should "reach start and end exactly at t=0 and t=1" in {
    val start = new Color(10, 20, 30, 40)
    val end   = new Color(200, 150, 100, 250)
    summon[Interpolator[Color]].lerp(start, end, 0.0) shouldBe start
    summon[Interpolator[Color]].lerp(start, end, 1.0) shouldBe end
  }

  it should "round to the nearest component value, matching RgbInterpolator's old rounding" in {
    // t = 1/3 of 0..100 = 33.33 -> rounds to 33
    summon[Interpolator[Color]].lerp(new Color(0, 0, 0), new Color(100, 0, 0), 1.0 / 3.0) shouldBe new Color(33, 0, 0)
  }

  it should "clamp components to [0, 255] even if t overshoots" in {
    summon[Interpolator[Color]].lerp(new Color(0, 0, 0), new Color(255, 255, 255), 2.0) shouldBe
      new Color(255, 255, 255)
    summon[Interpolator[Color]].lerp(new Color(0, 0, 0), new Color(255, 255, 255), -1.0) shouldBe
      new Color(0, 0, 0)
  }

  "a Tween[Color]" should "interpolate through RGBA components over its steps" in {
    val start = new Color(0, 0, 0)
    val end   = new Color(100, 100, 100)
    val tween = Tween(start = start, end = end, curve = EasingCurve.Linear, steps = 2, currentFrame = 1)
    tween.currentValue shouldBe new Color(50, 50, 50)
  }

  it should "apply its curve to the interpolated colour (issue #1574, folded in from ColorTimelineSpec)" in {
    val start = new Color(0, 0, 0)
    val end   = new Color(100, 100, 100)
    val tween = Tween(start = start, end = end, curve = EasingCurve.EaseIn, steps = 2, currentFrame = 1)
    // progress = 0.5, EaseIn(0.5) = 0.125
    tween.currentValue shouldBe new Color(13, 13, 13)
  }

  it should "still land exactly on start and end regardless of curve" in {
    val start = new Color(0, 0, 0)
    val end   = new Color(100, 100, 100)
    val tween = Tween(start = start, end = end, curve = EasingCurve.EaseInOut, steps = 2)
    tween.currentValue shouldBe start
    tween.advance.advance.currentValue shouldBe end
  }

  // ── Tween delay (issue #1574: shared with ColorTimeline's retired `delayFrames`) ──────────────

  "a Tween with delayFrames" should "report the start value and make no progress while inside the delay window" in {
    val tween = Tween(start = 0.0, end = 10.0, curve = EasingCurve.Linear, steps = 4, delayFrames = 2)
    tween.currentValue shouldBe 0.0
    tween.advance.currentValue shouldBe 0.0
    tween.isComplete shouldBe false
    tween.advance.isComplete shouldBe false
  }

  it should "behave exactly like a delay-free tween once the delay has elapsed" in {
    val tween = Tween(start = 0.0, end = 10.0, curve = EasingCurve.Linear, steps = 4, delayFrames = 2)
    val atDelayElapsed = tween.advance.advance // currentFrame = 2, delay just elapsed
    atDelayElapsed.currentValue shouldBe 0.0
    atDelayElapsed.advance.currentValue shouldBe 2.5 // one step into a 4-step Linear tween
  }

  it should "become complete only after delayFrames + steps total advances" in {
    val tween = Tween(start = 0.0, end = 10.0, curve = EasingCurve.Linear, steps = 2, delayFrames = 3)
    val advanced = Iterator.iterate(tween)(_.advance).drop(4).next()
    advanced.isComplete shouldBe false
    advanced.advance.isComplete shouldBe true
    advanced.advance.currentValue shouldBe 10.0
  }

  it should "default delayFrames to zero, unchanged from before delay existed" in {
    Tween(start = 0.0, end = 10.0, curve = EasingCurve.Linear, steps = 4).delayFrames shouldBe 0
  }

  "Tween.remainingFrames" should "count delay and interpolation frames left, matching the old step-list length" in {
    val tween = Tween(start = 0.0, end = 10.0, curve = EasingCurve.Linear, steps = 3, delayFrames = 2)
    tween.remainingFrames shouldBe 5
    tween.advance.remainingFrames shouldBe 4
    Iterator.iterate(tween)(_.advance).drop(5).next().remainingFrames shouldBe 0
  }

  it should "be zero for an already-complete (steps <= 0) tween" in {
    Tween(start = 0.0, end = 10.0, curve = EasingCurve.Linear, steps = 0).remainingFrames shouldBe 0
  }
