package com.serenity.animation

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** [[EasingCurve]] is a pure `[0,1] => [0,1]` remapping applied to a tween's linear progress before interpolation
  * (issues #1082/#1083). Every named curve must pass through `(0,0)` and `(1,1)` exactly, or a tween built on it would
  * neither start at its `start` value nor land exactly on `end`.
  */
class EasingSpec extends AnyFlatSpec with Matchers:

  private val Tolerance = 1e-9

  "Linear" should "return t unchanged" in {
    EasingCurve.Linear(0.0) shouldBe 0.0
    EasingCurve.Linear(0.25) shouldBe 0.25
    EasingCurve.Linear(0.5) shouldBe 0.5
    EasingCurve.Linear(1.0) shouldBe 1.0
  }

  "every named curve" should "pass through (0,0) and (1,1)" in {
    val curves = List(EasingCurve.Linear, EasingCurve.EaseIn, EasingCurve.EaseOut, EasingCurve.EaseInOut)
    curves.foreach { curve =>
      curve(0.0) shouldBe 0.0 +- Tolerance
      curve(1.0) shouldBe 1.0 +- Tolerance
    }
  }

  "EaseIn" should "start slow: its output at t=0.5 is below the linear midpoint" in {
    EasingCurve.EaseIn(0.5) should be < 0.5
  }

  it should "match the standard cubic ease-in formula (t^3)" in {
    EasingCurve.EaseIn(0.5) shouldBe 0.125 +- Tolerance
    EasingCurve.EaseIn(0.25) shouldBe math.pow(0.25, 3) +- Tolerance
  }

  "EaseOut" should "end slow: its output at t=0.5 is above the linear midpoint" in {
    EasingCurve.EaseOut(0.5) should be > 0.5
  }

  it should "match the standard cubic ease-out formula (1-(1-t)^3)" in {
    EasingCurve.EaseOut(0.5) shouldBe 0.875 +- Tolerance
    EasingCurve.EaseOut(0.25) shouldBe (1 - math.pow(0.75, 3)) +- Tolerance
  }

  "EaseInOut" should "be symmetric about its own midpoint" in {
    EasingCurve.EaseInOut(0.5) shouldBe 0.5 +- Tolerance
  }

  it should "start slower than linear and finish slower than linear, meeting exactly at the midpoint" in {
    EasingCurve.EaseInOut(0.25) should be < 0.25
    EasingCurve.EaseInOut(0.75) should be > 0.75
  }

  it should "match the standard piecewise cubic ease-in-out formula" in {
    // t < 0.5: 4*t^3 ; t >= 0.5: 1 - (-2t+2)^3 / 2
    EasingCurve.EaseInOut(0.25) shouldBe (4 * math.pow(0.25, 3)) +- Tolerance
    EasingCurve.EaseInOut(0.75) shouldBe (1 - math.pow(-2 * 0.75 + 2, 3) / 2) +- Tolerance
  }

  "CubicBezier" should "behave like Linear when its control points sit on the diagonal" in {
    val curve = EasingCurve.CubicBezier(0.0, 0.0, 1.0, 1.0)
    curve(0.0) shouldBe 0.0 +- 1e-6
    curve(0.25) shouldBe 0.25 +- 1e-6
    curve(0.5) shouldBe 0.5 +- 1e-6
    curve(0.75) shouldBe 0.75 +- 1e-6
    curve(1.0) shouldBe 1.0 +- 1e-6
  }

  it should "match CSS's ease-in-out preset (cubic-bezier(0.42, 0, 0.58, 1)) closely enough to be interchangeable" in {
    val cssEaseInOut = EasingCurve.CubicBezier(0.42, 0.0, 0.58, 1.0)
    cssEaseInOut(0.0) shouldBe 0.0 +- 1e-6
    cssEaseInOut(1.0) shouldBe 1.0 +- 1e-6
    cssEaseInOut(0.5) shouldBe 0.5 +- 1e-3
    // Slow start, like every ease-in-out.
    cssEaseInOut(0.1) should be < 0.1
  }

  it should "be monotonically increasing for a well-formed (non-looping) set of control points" in {
    val curve  = EasingCurve.CubicBezier(0.25, 0.1, 0.25, 1.0)
    val values = (0 to 20).map(i => curve(i / 20.0))
    values.sliding(2).foreach { case Seq(a, b) => b should be >= a }
  }

  it should "clamp its input to [0,1] rather than extrapolate" in {
    EasingCurve.CubicBezier(0.25, 0.1, 0.25, 1.0)(-0.5) shouldBe 0.0 +- 1e-9
    EasingCurve.CubicBezier(0.25, 0.1, 0.25, 1.0)(1.5) shouldBe 1.0 +- 1e-9
  }
