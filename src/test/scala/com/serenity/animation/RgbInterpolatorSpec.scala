package com.serenity.animation

import com.googlecode.lanterna.TextColor
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class RgbInterpolatorSpec extends AnyFlatSpec with Matchers:

  "RgbInterpolator" should "interpolate between RGB colors" in {
    val startColor = new TextColor.RGB(0, 0, 0)       // Black
    val endColor   = new TextColor.RGB(255, 255, 255) // White
    val steps      = 6

    val interpolated = RgbInterpolator.interpolate(startColor, endColor, steps)

    interpolated should have length steps
    interpolated.head shouldEqual startColor
    interpolated.last shouldEqual endColor
  }

  it should "handle single step interpolation" in {
    val startColor = new TextColor.RGB(100, 100, 100)
    val endColor   = new TextColor.RGB(200, 200, 200)

    val interpolated = RgbInterpolator.interpolate(startColor, endColor, 1)

    interpolated should have length 1
    interpolated.head shouldEqual endColor
  }

  it should "create smooth transitions for RGB colors" in {
    val startColor = new TextColor.RGB(0, 0, 0)
    val endColor   = new TextColor.RGB(255, 0, 0) // Red
    val steps      = 3

    val interpolated = RgbInterpolator.interpolate(startColor, endColor, steps)

    interpolated should have length 3
    // Should be approximately: (0,0,0) -> (127,0,0) -> (255,0,0)
    val middle = interpolated(1).asInstanceOf[TextColor.RGB]
    middle.getRed should be > 100
    middle.getRed should be < 150
  }

  it should "handle non-RGB to RGB interpolation without assuming ANSI palette steps" in {
    val startColor = TextColor.Factory.fromString("#000000")
    val endColor   = new TextColor.RGB(255, 255, 255)

    val interpolated = RgbInterpolator.interpolate(startColor, endColor, 4)

    interpolated should have length 4
    interpolated.head shouldEqual startColor
    interpolated.last shouldEqual endColor
  }

  it should "handle RGB to non-RGB interpolation" in {
    val startColor = new TextColor.RGB(0, 0, 0)
    val endColor   = TextColor.Factory.fromString("#ffffff")

    val interpolated = RgbInterpolator.interpolate(startColor, endColor, 3)

    interpolated should have length 3
    interpolated.head shouldEqual startColor
    interpolated.last shouldEqual endColor
  }

  it should "handle interpolation between non-RGB endpoint colors" in {
    val startColor = TextColor.Factory.fromString("#000000")
    val endColor   = TextColor.Factory.fromString("#ffffff")

    val interpolated = RgbInterpolator.interpolate(startColor, endColor, 6)

    interpolated should have length 6
    interpolated.head shouldEqual startColor
    interpolated.last shouldEqual endColor
    // Should contain smooth RGB gradients between black and white
    val middle = interpolated(3).asInstanceOf[TextColor.RGB]
    middle.getRed should be >= 100
    middle.getRed should be <= 155
  }

  it should "handle same color interpolation" in {
    val color = TextColor.Factory.fromString("#ff0000")

    val interpolated = RgbInterpolator.interpolate(color, color, 5)

    interpolated should have length 5
    interpolated.forall(_ == color) should be(true)
  }

  it should "handle zero steps gracefully" in {
    val startColor = TextColor.Factory.fromString("#000000")
    val endColor   = TextColor.Factory.fromString("#ffffff")

    val interpolated = RgbInterpolator.interpolate(startColor, endColor, 0)

    interpolated should be(empty)
  }

  "RgbInterpolator.toRgb" should "convert non-RGB colors to RGB values" in {
    val black = RgbInterpolator.toRgb(TextColor.Factory.fromString("#000000"))
    black.getRed shouldEqual 0
    black.getGreen shouldEqual 0
    black.getBlue shouldEqual 0

    val white = RgbInterpolator.toRgb(TextColor.Factory.fromString("#ffffff"))
    white.getRed shouldEqual 255
    white.getGreen shouldEqual 255
    white.getBlue shouldEqual 255
  }

  it should "pass through RGB colors unchanged" in {
    val rgbColor  = new TextColor.RGB(123, 234, 45)
    val converted = RgbInterpolator.toRgb(rgbColor)

    converted shouldEqual rgbColor
  }
