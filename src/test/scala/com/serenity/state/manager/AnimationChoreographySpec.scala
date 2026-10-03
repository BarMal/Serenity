package com.serenity.state.manager

import com.serenity.animation.{AnimationOwner, AnimationState, SweepDirection}
import com.serenity.config.AppConfigMotionOps.*
import com.serenity.config.{AppConfig, MotionAccessibility}
import com.serenity.rope.Balance
import com.serenity.state.models.*
import com.serenity.ui.layout.ViewportSize
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** The tab-cycling pane sweep as a plain function of the state around an event. */
class AnimationChoreographySpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private val bufferId = BufferId(0)

  private def baseState(config: AppConfig): AppState =
    val initial = AppState.initial(config)
    initial.copy(
      persisted = initial.persisted
        .copy(buffers = initial.persisted.buffers.updated(bufferId, Buffer.fromString(bufferId, "hello\nworld"))),
      runtime = initial.runtime.copy(viewportSize = Some(ViewportSize(120, 40)))
    )

  private val animated = baseState(AppConfig.withTestAnimations)

  "withPaneFlowAnimation" should "sweep the active buffer with UiTransitions-owned animations" in {
    val swept = AnimationChoreography.withPaneFlowAnimation(animated, SweepDirection.Forward)(Map.empty)

    swept.get(bufferId).exists(_.hasActiveAnimations) shouldBe true
    swept.get(bufferId).map(_.animations.values.map(_.owner).toSet) shouldBe Some(Set(AnimationOwner.UiTransitions))
  }

  it should "leave the buffer animations untouched when motion is off" in {
    val off      = baseState(AppConfig.withTestAnimations.withMotionAccessibility(MotionAccessibility.Off))
    val existing = Map(bufferId -> AnimationState.empty)

    AnimationChoreography.withPaneFlowAnimation(off, SweepDirection.Forward)(existing) shouldBe existing
  }
