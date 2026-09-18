package com.serenity.state.models

import com.serenity.animation.{EasingCurve, Tween}
import com.serenity.ui.layout.LayoutRect
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Panel scale-in/out (issue #1085 phase 1): mid-flight geometry for a pinned/docked panel opening or closing. Built on
  * `Tween[LayoutRect]` (issue #1083) the same way `ColumnTransitionState` is built on `Tween[Double]`.
  */
class PanelGeometryStateSpec extends AnyFlatSpec with Matchers:

  private val collapsed = LayoutRect(0, 0, 0, 10)
  private val full      = LayoutRect(0, 0, 20, 10)

  "PanelGeometryState" should "start at the collapsed rect and not complete" in {
    val state = PanelGeometryState(Tween(start = collapsed, end = full, curve = EasingCurve.Linear, steps = 4))

    state.currentRect shouldBe collapsed
    state.isComplete shouldBe false
  }

  it should "advance its tween toward the full rect" in {
    val state = PanelGeometryState(Tween(start = collapsed, end = full, curve = EasingCurve.Linear, steps = 4))

    val advanced = state.advance

    advanced.currentRect shouldBe LayoutRect(0, 0, 5, 10)
    advanced.isComplete shouldBe false
  }

  it should "complete once its tween is exhausted, reaching the full rect" in {
    val state = PanelGeometryState(Tween(start = collapsed, end = full, curve = EasingCurve.Linear, steps = 2))

    val settled = state.advance.advance

    settled.isComplete shouldBe true
    settled.currentRect shouldBe full
  }

  "PanelGeometryState.seeded" should "start a fresh tween at the given start rect over the given step count" in {
    val state = PanelGeometryState.seeded(start = collapsed, end = full, curve = EasingCurve.Linear, steps = 4)

    state.currentRect shouldBe collapsed
    state.isComplete shouldBe false
  }
