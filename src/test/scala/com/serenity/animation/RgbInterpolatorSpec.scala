package com.serenity.animation

import java.awt.Color

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class RgbInterpolatorSpec extends AnyFlatSpec with Matchers:

  "RgbInterpolator.interpolateRgba" should "interpolate between two colors" in {
    val startColor = new Color(0, 0, 0)
    val endColor   = new Color(255, 255, 255)
    val steps      = 6

    val interpolated = RgbInterpolator.interpolateRgba(startColor, endColor, steps)

    interpolated should have length steps
    interpolated.head shouldEqual startColor
    interpolated.last shouldEqual endColor
  }

  it should "handle single step interpolation" in {
    val startColor = new Color(100, 100, 100)
    val endColor   = new Color(200, 200, 200)

    val interpolated = RgbInterpolator.interpolateRgba(startColor, endColor, 1)

    interpolated should have length 1
    interpolated.head shouldEqual endColor
  }

  it should "create smooth transitions" in {
    val startColor = new Color(0, 0, 0)
    val endColor   = new Color(255, 0, 0)
    val steps      = 3

    val interpolated = RgbInterpolator.interpolateRgba(startColor, endColor, steps)

    interpolated should have length 3
    val middle = interpolated(1)
    middle.getRed should be > 100
    middle.getRed should be < 150
  }

  it should "handle same color interpolation" in {
    val color = new Color(255, 0, 0)

    val interpolated = RgbInterpolator.interpolateRgba(color, color, 5)

    interpolated should have length 5
    interpolated.forall(_ == color) should be(true)
  }

  it should "handle zero steps gracefully" in {
    val startColor = new Color(0, 0, 0)
    val endColor   = new Color(255, 255, 255)

    val interpolated = RgbInterpolator.interpolateRgba(startColor, endColor, 0)

    interpolated should be(empty)
  }

  it should "produce colors with correct alpha when both inputs are opaque" in {
    val startColor = new Color(0, 0, 0, 255)
    val endColor   = new Color(255, 255, 255, 255)

    val interpolated = RgbInterpolator.interpolateRgba(startColor, endColor, 4)

    interpolated.forall(_.getAlpha == 255) should be(true)
  }

  "RgbInterpolator.interpolateRgbaAt" should "default to Linear, unchanged from before curves existed" in {
    val startColor = new Color(0, 0, 0)
    val endColor   = new Color(100, 100, 100)

    RgbInterpolator.interpolateRgbaAt(startColor, endColor, steps = 5, step = 2) shouldEqual
      RgbInterpolator.interpolateRgbaAt(startColor, endColor, steps = 5, step = 2, curve = EasingCurve.Linear)
  }

  it should "apply the given curve to the normalized t before interpolating components" in {
    val startColor = new Color(0, 0, 0)
    val endColor   = new Color(100, 0, 0)

    // steps=3, step=1 -> linear t = 0.5; EaseIn(0.5) = 0.125
    val eased = RgbInterpolator.interpolateRgbaAt(startColor, endColor, steps = 3, step = 1, curve = EasingCurve.EaseIn)

    eased.map(_.getRed) shouldBe Some(math.round(100 * 0.125).toInt)
  }

  it should "still return the exact start/end colors at the first/last step under a non-linear curve" in {
    val startColor = new Color(10, 20, 30)
    val endColor   = new Color(200, 150, 100)

    RgbInterpolator.interpolateRgbaAt(
      startColor,
      endColor,
      steps = 4,
      step = 0,
      curve = EasingCurve.EaseInOut
    ) shouldEqual
      Some(startColor)
    RgbInterpolator.interpolateRgbaAt(
      startColor,
      endColor,
      steps = 4,
      step = 3,
      curve = EasingCurve.EaseInOut
    ) shouldEqual
      Some(endColor)
  }
