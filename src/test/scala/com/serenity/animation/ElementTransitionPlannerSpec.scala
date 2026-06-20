package com.serenity.animation

import com.serenity.ui.layout.PanelPosition
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ElementTransitionPlannerSpec extends AnyFlatSpec with Matchers:

  "ElementTransitionPlanner" should "snap to a disabled plan when motion is disabled" in {
    val plan = ElementTransitionPlanner.plan(
      ElementTransitionRequest(TransitionScope.PanelOpen, placement = Some(PanelPosition.Left)),
      ElementTransitionSettings.disabled
    )

    plan.kind shouldBe TransitionKind.Disabled
    plan.direction shouldBe TransitionDirection.LeftToRight
    plan.timing shouldBe TransitionTiming.immediate
  }

  it should "derive side-panel open direction from placement" in {
    val settings = ElementTransitionSettings.subtle

    ElementTransitionPlanner
      .plan(ElementTransitionRequest(TransitionScope.PanelOpen, Some(PanelPosition.Left)), settings)
      .direction shouldBe TransitionDirection.LeftToRight
    ElementTransitionPlanner
      .plan(ElementTransitionRequest(TransitionScope.PanelOpen, Some(PanelPosition.Right)), settings)
      .direction shouldBe TransitionDirection.RightToLeft
    ElementTransitionPlanner
      .plan(ElementTransitionRequest(TransitionScope.PanelOpen, Some(PanelPosition.Top)), settings)
      .direction shouldBe TransitionDirection.TopToBottom
    ElementTransitionPlanner
      .plan(ElementTransitionRequest(TransitionScope.PanelOpen, Some(PanelPosition.Bottom)), settings)
      .direction shouldBe TransitionDirection.BottomToTop
  }

  it should "plan outline-then-content for panel opens by default" in {
    val plan = ElementTransitionPlanner.plan(
      ElementTransitionRequest(TransitionScope.PanelOpen, placement = Some(PanelPosition.Right)),
      ElementTransitionSettings.subtle
    )

    plan.kind shouldBe TransitionKind.OutlineThenContent
    plan.timing shouldBe TransitionTiming(durationMs = 160, staggerMs = 12, delayMs = 0, speedScale = 1.0)
  }

  it should "allow per-scope transition kind overrides" in {
    val settings = ElementTransitionSettings.subtle.copy(
      overrides = Map(TransitionScope.Glyph -> TransitionKind.TypedText)
    )

    val plan = ElementTransitionPlanner.plan(ElementTransitionRequest(TransitionScope.Glyph), settings)

    plan.kind shouldBe TransitionKind.TypedText
    plan.direction shouldBe TransitionDirection.AnchorIn
  }

  it should "scale timing without changing deterministic ordering semantics" in {
    val settings = ElementTransitionSettings.subtle.copy(speedScale = 2.0)

    val plan = ElementTransitionPlanner.plan(ElementTransitionRequest(TransitionScope.Row), settings)

    plan.kind shouldBe TransitionKind.DirectionalSweep
    plan.timing shouldBe TransitionTiming(durationMs = 320, staggerMs = 24, delayMs = 0, speedScale = 2.0)
  }
