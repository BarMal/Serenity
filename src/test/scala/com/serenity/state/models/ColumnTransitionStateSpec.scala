package com.serenity.state.models

import com.serenity.animation.{EasingCurve, Tween, TransitionDirection}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Column-based document layout (issue #1338, Phase 1 animation): mid-flight state for the transition between one
  * column's content and the next. Built on `Tween[Double]` (issue #1083) rather than the now-retired `ScalarTimeline`
  * -- see `retarget` below for the one behaviour `ScalarTimeline` never had.
  */
class ColumnTransitionStateSpec extends AnyFlatSpec with Matchers:

  "ColumnTransitionState" should "start at zero progress and not complete" in {
    val state = ColumnTransitionState(
      tween = Tween(start = 0.0, end = 1.0, curve = EasingCurve.Linear, steps = 4),
      direction = TransitionDirection.RightToLeft,
      previousTopLine = 0,
      previousTopVisualLine = 0
    )

    state.progress shouldBe 0.0
    state.isComplete shouldBe false
  }

  it should "advance its own tween without disturbing the rest of its state" in {
    val state = ColumnTransitionState(
      tween = Tween(start = 0.0, end = 1.0, curve = EasingCurve.Linear, steps = 2),
      direction = TransitionDirection.LeftToRight,
      previousTopLine = 12,
      previousTopVisualLine = 3
    )

    val advanced = state.advance

    advanced.progress shouldBe 0.5
    advanced.direction shouldBe TransitionDirection.LeftToRight
    advanced.previousTopLine shouldBe 12
    advanced.previousTopVisualLine shouldBe 3
  }

  it should "complete once its tween is exhausted" in {
    val state = ColumnTransitionState(
      tween = Tween(start = 0.0, end = 1.0, curve = EasingCurve.Linear, steps = 2),
      direction = TransitionDirection.RightToLeft,
      previousTopLine = 0,
      previousTopVisualLine = 0
    )

    state.advance.advance.isComplete shouldBe true
  }

  "retarget" should "continue smoothly from the current progress rather than resetting to zero" in {
    val state = ColumnTransitionState(
      tween = Tween(start = 0.0, end = 1.0, curve = EasingCurve.Linear, steps = 4),
      direction = TransitionDirection.RightToLeft,
      previousTopLine = 0,
      previousTopVisualLine = 0
    ).advance.advance

    state.progress shouldBe 0.5

    val retargeted = state.retarget

    retargeted.progress shouldBe 0.5
    retargeted.isComplete shouldBe false
    retargeted.direction shouldBe state.direction
    retargeted.previousTopLine shouldBe state.previousTopLine
    retargeted.previousTopVisualLine shouldBe state.previousTopVisualLine
  }

  it should "still reach full progress after enough further advances" in {
    val state = ColumnTransitionState(
      tween = Tween(start = 0.0, end = 1.0, curve = EasingCurve.Linear, steps = 4),
      direction = TransitionDirection.RightToLeft,
      previousTopLine = 0,
      previousTopVisualLine = 0
    ).advance.advance.retarget

    val settled = state.advance.advance
    settled.isComplete shouldBe true
    settled.progress shouldBe 1.0
  }

  "ColumnTransitionState.seeded" should "start a fresh transition at zero progress over the given step count" in {
    val state = ColumnTransitionState.seeded(
      steps = 4,
      curve = EasingCurve.Linear,
      direction = TransitionDirection.LeftToRight,
      previousTopLine = 5,
      previousTopVisualLine = 1
    )

    state.progress shouldBe 0.0
    state.direction shouldBe TransitionDirection.LeftToRight
    state.previousTopLine shouldBe 5
    state.previousTopVisualLine shouldBe 1
  }
