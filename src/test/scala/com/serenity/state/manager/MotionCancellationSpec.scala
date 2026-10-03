package com.serenity.state.manager

import java.awt.Color

import com.serenity.animation.{AnimatedCell, AnimationOwner, AnimationState, CharacterKey, EasingCurve, Tween}
import com.serenity.config.AppConfigMotionOps.*
import com.serenity.config.{AppConfig, MotionAccessibility, MotionFamily}
import com.serenity.state.models.BufferId
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Pure motion cancellation (#1697 wave 2): which in-flight buffer motion a config change cancels. */
class MotionCancellationSpec extends AnyFlatSpec with Matchers:

  private val bufferId = BufferId(0)

  private def tween(steps: Int): Tween[Color] =
    Tween(new Color(0, 0, 0), new Color(255, 255, 255), EasingCurve.Linear, steps)

  private val bufferAnimations: Map[BufferId, AnimationState] =
    Map(
      bufferId -> AnimationState(
        Map(
          CharacterKey(0, 0) -> AnimatedCell(Some('h'), Some(tween(4)), None, AnimationOwner.EditorText),
          CharacterKey(1, 0) -> AnimatedCell(Some('e'), Some(tween(4)), None, AnimationOwner.UiTransitions)
        )
      )
    )

  private def ownersOf(animations: Map[BufferId, AnimationState]): Set[AnimationOwner] =
    animations(bufferId).animations.values.map(_.owner).toSet

  "MotionCancellation.between" should "cancel everything when motion as a whole goes from on to off" in {
    val previous = AppConfig.default
    val current  = previous.withMotionAccessibility(MotionAccessibility.Off)

    MotionCancellation.between(previous, current) shouldBe MotionCancellation.Everything
  }

  it should "cancel only the families that went from enabled to disabled" in {
    val previous = AppConfig.default
    val current  = previous.withCursorTransitionSpeedScale(Some(0.0))

    MotionCancellation.between(previous, current) shouldBe MotionCancellation.Families(List(MotionFamily.Cursor))
  }

  it should "cancel nothing when no family changed" in {
    MotionCancellation.between(AppConfig.default, AppConfig.default).isEmpty shouldBe true
  }

  "cancelling everything" should "clear every buffer animation regardless of owner" in {
    val cancelled = MotionCancellation.Everything.cancelBufferAnimations(bufferAnimations)

    cancelled(bufferId).hasActiveAnimations shouldBe false
  }

  "cancelling the EditorText family" should "clear only EditorText-owned buffer animations" in {
    val cancellation = MotionCancellation.Families(List(MotionFamily.EditorText))

    ownersOf(cancellation.cancelBufferAnimations(bufferAnimations)) shouldBe Set(AnimationOwner.UiTransitions)
  }

  "cancelling the UiTransitions family" should "clear only UiTransitions-owned buffer animations" in {
    val cancellation = MotionCancellation.Families(List(MotionFamily.UiTransitions))

    ownersOf(cancellation.cancelBufferAnimations(bufferAnimations)) shouldBe Set(AnimationOwner.EditorText)
  }

  "cancelling a family with no buffer animations" should "leave buffer animations alone" in {
    val cancellation = MotionCancellation.Families(List(MotionFamily.CommandSurfaces, MotionFamily.PinnedPanels))

    cancellation.cancelBufferAnimations(bufferAnimations) shouldBe bufferAnimations
  }
