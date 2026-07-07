package com.serenity.animation

import com.serenity.config.{AppConfig, MotionPreset}
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

  it should "derive scaled element transition settings from app config" in {
    val config = AppConfig.default
      .withMotionPreset(MotionPreset.Subtle)
      .withElementTransitionSpeedScale(1.5)
      .withUiTransitionSpeedScale(Some(0.5))

    val plan =
      ElementTransitionPlanner.plan(ElementTransitionRequest(TransitionScope.Row), config.elementTransitionSettings)

    plan.timing shouldBe TransitionTiming(durationMs = 80, staggerMs = 6, delayMs = 0, speedScale = 0.5)
  }

  it should "derive editor insertion transition overrides from app config" in {
    val config = AppConfig.default
      .withMotionPreset(MotionPreset.Subtle)
      .withEditorInsertionTransitionKind(TransitionKind.TypedText)

    val plan = ElementTransitionPlanner.plan(
      ElementTransitionRequest(TransitionScope.EditorInsertion),
      config.elementTransitionSettings
    )

    plan.kind shouldBe TransitionKind.TypedText
  }

  it should "derive panel open and close transition overrides from app config" in {
    val config = AppConfig.default
      .withMotionPreset(MotionPreset.Subtle)
      .withPanelOpenTransitionKind(Some(TransitionKind.DirectionalSweep))
      .withPanelCloseTransitionKind(Some(TransitionKind.Disabled))

    val openPlan = ElementTransitionPlanner.plan(
      ElementTransitionRequest(TransitionScope.PanelOpen, Some(PanelPosition.Left)),
      config.elementTransitionSettings
    )
    val closePlan = ElementTransitionPlanner.plan(
      ElementTransitionRequest(TransitionScope.PanelClose, Some(PanelPosition.Left)),
      config.elementTransitionSettings
    )

    openPlan.kind shouldBe TransitionKind.DirectionalSweep
    closePlan.kind shouldBe TransitionKind.Disabled
  }

  it should "derive command runner transition overrides from app config" in {
    val config = AppConfig.default
      .withMotionPreset(MotionPreset.Subtle)
      .withCommandRunnerTransitionKind(Some(TransitionKind.OutlineThenContent))

    val plan = ElementTransitionPlanner.plan(
      ElementTransitionRequest(TransitionScope.CommandRunner),
      config.elementTransitionSettings
    )

    plan.kind shouldBe TransitionKind.OutlineThenContent
    plan.direction shouldBe TransitionDirection.AnchorIn
  }

  it should "derive general UI animation from its own config rather than editor text animation" in {
    val config = AppConfig.default
      .withMotionPreset(MotionPreset.Smooth)
      .withCharacterAnimation(AnimationConfig.quick.get)
      .withUiAnimation(AnimationConfig.subtle)

    config.scaledCharacterAnimation.map(_.steps) shouldBe AnimationConfig.quick.map(_.steps)
    config.scaledUiAnimation.map(_.steps) shouldBe AnimationConfig.subtle.map(_.steps)
  }

  it should "let per-family animation speed scales override the global fallback" in {
    val config = AppConfig.default
      .withMotionPreset(MotionPreset.Smooth)
      .withCharacterAnimation(AnimationConfig.smooth.get)
      .withCommandRunnerAnimation(AnimationConfig.smooth)
      .withUiAnimation(AnimationConfig.smooth)
      .withElementTransitionSpeedScale(2.0)
      .withEditorTextTransitionSpeedScale(Some(0.5))
      .withCommandRunnerTransitionSpeedScale(Some(1.5))
      .withUiTransitionSpeedScale(Some(0.0))
      .withCursorTransitionSpeedScale(Some(0.25))

    config.effectiveEditorTextTransitionSpeedScale shouldBe 0.5
    config.effectiveCommandRunnerTransitionSpeedScale shouldBe 1.5
    config.effectiveUiTransitionSpeedScale shouldBe 0.0
    config.effectiveCursorTransitionSpeedScale shouldBe 0.25
    config.scaledCharacterAnimation.map(_.steps) shouldBe Some(6)
    config.scaledCommandRunnerAnimation.map(_.steps) shouldBe Some(18)
    config.scaledUiAnimation shouldBe None
  }

  it should "use the global animation speed scale when family scales are unset" in {
    val config = AppConfig.default
      .withMotionPreset(MotionPreset.Smooth)
      .withCharacterAnimation(AnimationConfig.smooth.get)
      .withCommandRunnerAnimation(AnimationConfig.smooth)
      .withUiAnimation(AnimationConfig.smooth)
      .withElementTransitionSpeedScale(2.0)

    config.effectiveEditorTextTransitionSpeedScale shouldBe 2.0
    config.effectiveCommandRunnerTransitionSpeedScale shouldBe 2.0
    config.effectiveUiTransitionSpeedScale shouldBe 2.0
    config.effectiveCursorTransitionSpeedScale shouldBe 2.0
    config.scaledCharacterAnimation.map(_.steps) shouldBe Some(24)
    config.scaledCommandRunnerAnimation.map(_.steps) shouldBe Some(24)
    config.scaledUiAnimation.map(_.steps) shouldBe Some(24)
  }

  it should "preserve explicit family speed overrides when applying motion presets" in {
    val config = AppConfig.default
      .withElementTransitionSpeedScale(2.0)
      .withEditorTextTransitionSpeedScale(Some(0.5))
      .withCommandRunnerTransitionSpeedScale(Some(1.5))
      .withUiTransitionSpeedScale(Some(0.75))
      .withCursorTransitionSpeedScale(Some(0.25))
      .withMotionPreset(MotionPreset.Reduced)

    config.effectiveEditorTextTransitionSpeedScale shouldBe 0.5
    config.effectiveCommandRunnerTransitionSpeedScale shouldBe 1.5
    config.effectiveUiTransitionSpeedScale shouldBe 0.75
    config.effectiveCursorTransitionSpeedScale shouldBe 0.25
  }

  it should "keep reduced motion disabled even when app config has a custom speed scale" in {
    val config = AppConfig.default
      .withMotionPreset(MotionPreset.Reduced)
      .withElementTransitionSpeedScale(2.0)

    val plan = ElementTransitionPlanner.plan(
      ElementTransitionRequest(TransitionScope.PanelOpen),
      config.elementTransitionSettings
    )

    config.elementTransitionSettings shouldBe ElementTransitionSettings.disabled
    plan.kind shouldBe TransitionKind.Disabled
    plan.timing shouldBe TransitionTiming.immediate
  }

  it should "map reduced motion presets to disabled element transitions" in {
    val settings = MotionPreset.Reduced.elementTransitionSettings
    val plan     = ElementTransitionPlanner.plan(ElementTransitionRequest(TransitionScope.PanelOpen), settings)

    settings shouldBe ElementTransitionSettings.disabled
    plan.kind shouldBe TransitionKind.Disabled
    plan.timing shouldBe TransitionTiming.immediate
  }

  it should "map motion presets to deterministic element transition timings" in {
    MotionPreset.Subtle.elementTransitionSettings.baseTiming shouldBe TransitionTiming(
      durationMs = 160,
      staggerMs = 12,
      delayMs = 0,
      speedScale = 1.0
    )
    MotionPreset.Smooth.elementTransitionSettings.baseTiming shouldBe TransitionTiming(
      durationMs = 220,
      staggerMs = 16,
      delayMs = 0,
      speedScale = 1.0
    )
    MotionPreset.Expressive.elementTransitionSettings.baseTiming shouldBe TransitionTiming(
      durationMs = 280,
      staggerMs = 22,
      delayMs = 20,
      speedScale = 1.0
    )
    MotionPreset.Custom.elementTransitionSettings shouldBe MotionPreset.Smooth.elementTransitionSettings
  }
