package com.serenity.input

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class WheelScrollAccumulatorSpec extends AnyFlatSpec with Matchers:

  private def run(axis: WheelAxis, deltas: Seq[Double]): (WheelScrollState, Seq[Int]) =
    deltas.foldLeft((WheelScrollState.empty, Vector.empty[Int])) {
      case ((state, notches), delta) =>
        val step = WheelScrollAccumulator.accumulate(state, axis, delta)
        (step.state, notches :+ step.notches)
    }

  "a classic wheel's whole-notch rotation" should "pass through unchanged and leave nothing behind" in {
    val (state, notches) = run(WheelAxis.Vertical, Seq(1.0, -1.0, 3.0, -2.0))

    notches shouldBe Seq(1, -1, 3, -2)
    state shouldBe WheelScrollState.empty
  }

  "fractional trackpad deltas" should "emit exactly one notch once they sum to a whole notch" in {
    val (state, notches) = run(WheelAxis.Vertical, Seq.fill(10)(0.1))

    notches.sum shouldBe 1
    notches.last shouldBe 1
    state shouldBe WheelScrollState.empty
  }

  they should "emit nothing while the sum stays under a notch" in {
    val (state, notches) = run(WheelAxis.Vertical, Seq(0.25, 0.25, 0.25))

    notches shouldBe Seq(0, 0, 0)
    state.vertical shouldBe 0.75 +- 1e-12
  }

  they should "carry the part past a whole notch into the next one" in {
    val (state, notches) = run(WheelAxis.Vertical, Seq(0.75, 0.75, 0.75))

    notches shouldBe Seq(0, 1, 1)
    state.vertical shouldBe 0.25 +- 1e-12
  }

  they should "accumulate upward scrolling as negative notches" in {
    val (_, notches) = run(WheelAxis.Vertical, Seq(-0.4, -0.4, -0.4))

    notches shouldBe Seq(0, 0, -1)
  }

  "reversing direction mid-gesture" should "discard the leftover rotation the other way" in {
    val (state, notches) = run(WheelAxis.Vertical, Seq(0.9, -0.5, -0.5))

    notches shouldBe Seq(0, 0, -1)
    state shouldBe WheelScrollState.empty
  }

  "the vertical and horizontal axes" should "accumulate independently" in {
    val afterVertical = WheelScrollAccumulator.accumulate(WheelScrollState.empty, WheelAxis.Vertical, 0.6)
    val horizontal    = WheelScrollAccumulator.accumulate(afterVertical.state, WheelAxis.Horizontal, 0.6)
    val vertical      = WheelScrollAccumulator.accumulate(horizontal.state, WheelAxis.Vertical, 0.6)

    horizontal.notches shouldBe 0
    vertical.notches shouldBe 1
    vertical.state.horizontal shouldBe 0.6 +- 1e-12
  }

  "a non-finite rotation" should "be ignored rather than poison the remainder" in {
    val (state, notches) = run(WheelAxis.Vertical, Seq(0.5, Double.NaN, Double.PositiveInfinity, 0.5))

    notches shouldBe Seq(0, 0, 0, 1)
    state shouldBe WheelScrollState.empty
  }
