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
