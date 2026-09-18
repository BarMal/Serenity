package com.serenity.state.models

import com.serenity.animation.{ScalarTimeline, TransitionDirection}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Column-based document layout (issue #1338, Phase 1 animation): mid-flight state for the transition between one
  * column's content and the next.
  */
class ColumnTransitionStateSpec extends AnyFlatSpec with Matchers:

  "ColumnTransitionState" should "start at zero progress and not complete" in {
    val state = ColumnTransitionState(
      timeline = ScalarTimeline(steps = 4),
      direction = TransitionDirection.RightToLeft,
      previousTopLine = 0,
      previousTopVisualLine = 0
    )

    state.progress shouldBe 0.0
    state.isComplete shouldBe false
  }

  it should "advance its own timeline without disturbing the rest of its state" in {
    val state = ColumnTransitionState(
      timeline = ScalarTimeline(steps = 2),
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

  it should "complete once its timeline is exhausted" in {
    val state = ColumnTransitionState(
      timeline = ScalarTimeline(steps = 2),
      direction = TransitionDirection.RightToLeft,
      previousTopLine = 0,
      previousTopVisualLine = 0
    )

    state.advance.advance.isComplete shouldBe true
  }
