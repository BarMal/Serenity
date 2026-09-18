package com.serenity.animation

import java.awt.Color

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** `ColorTimeline`'s `curve` field (issue #1082/#1083, item 3): defaults to `EasingCurve.Linear`, so every existing
  * caller is byte-for-byte unchanged, and threads through to `RgbInterpolator.interpolateRgbaAt` for callers that opt
  * into a different curve.
  */
class ColorTimelineSpec extends AnyFlatSpec with Matchers:

  private val black = new Color(0, 0, 0)
  private val white = new Color(100, 100, 100)

  "a ColorTimeline with no curve specified" should "interpolate linearly, as before curves existed" in {
    val timeline = ColorTimeline(black, white, steps = 3, currentFrame = 1)
    timeline.currentColor shouldBe Some(new Color(50, 50, 50))
  }

  "a ColorTimeline with an explicit curve" should "apply it to the interpolated colour" in {
    val timeline = ColorTimeline(black, white, steps = 3, curve = EasingCurve.EaseIn, currentFrame = 1)
    // t = 0.5, EaseIn(0.5) = 0.125
    timeline.currentColor shouldBe Some(new Color(13, 13, 13))
  }

  it should "still land exactly on start and end regardless of curve" in {
    val timeline = ColorTimeline(black, white, steps = 3, curve = EasingCurve.EaseInOut)
    timeline.currentColor shouldBe Some(black)
    timeline.advance.advance.currentColor shouldBe Some(white)
  }
