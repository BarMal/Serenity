package com.serenity

import com.serenity.animation.{TransitionKind, TransitionScope}
import com.serenity.config.*
import com.serenity.config.AppConfigMotionOps.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class SurfaceConfigSpec extends AnyFlatSpec with Matchers:

  "PostProcessingEffect" should "parse supported configuration values" in {
    PostProcessingEffect.fromConfigKey("off") shouldBe Some(PostProcessingEffect.Off)
    PostProcessingEffect.fromConfigKey("crt") shouldBe Some(PostProcessingEffect.Scanlines)
    PostProcessingEffect.fromConfigKey("glow") shouldBe Some(PostProcessingEffect.Glow)
  }

  "SurfaceConfig.normalized" should "clamp rendererFrameStateCacheCapacity to AppConfig's configured bounds" in {
    SurfaceConfig(rendererFrameStateCacheCapacity = Int.MaxValue).normalized.rendererFrameStateCacheCapacity shouldBe
      AppConfig.MaxRendererFrameStateCacheCapacity
    SurfaceConfig(rendererFrameStateCacheCapacity = -100).normalized.rendererFrameStateCacheCapacity shouldBe
      AppConfig.MinRendererFrameStateCacheCapacity
    SurfaceConfig(rendererFrameStateCacheCapacity = 128).normalized.rendererFrameStateCacheCapacity shouldBe 128
  }

  "SurfaceConfig" should "own the motion-hierarchy schema metadata" in {
    // #1406: material, post-processing, display, command-runner, text-area and viewport keys used to be duplicated
    // here too, but `ConfigRegistry` already owned parsing/validation/writing for every one of them end-to-end, so
    // `ConfigManager.parseConfig`'s fallback to this schema was unreachable dead code for those keys. Only the motion
    // hierarchy has no `ConfigField` to read it back with, so it is the one thing left here.
    SurfaceConfigSchemaKeys.currentKeys.should(contain("ui.motion.cursor.speed_scale"))
    SurfaceConfigSchemaKeys.currentKeys.should(contain("ui.motion.panel_open"))
    SurfaceConfigSchemaKeys.currentKeys.should(contain("ui.motion.family.command_surfaces.transition"))
    SurfaceConfigSchemaKeys.currentKeys.shouldNot(contain("ui.material"))
    SurfaceConfigSchemaKeys.currentKeys.shouldNot(contain("display.contextual_toolbar_mode"))
    SurfaceConfigSchemaKeys.currentKeys.shouldNot(contain("viewport.height.max"))

    SurfaceConfigSchemaKeys.deprecatedKeys("ui_motion_cursor_speed_scale").shouldBe("ui.motion.cursor.speed_scale")
    SurfaceConfigSchemaKeys.deprecatedKeys("ui_motion_command_runner").shouldBe("ui.motion.command_runner")
    SurfaceConfigSchemaKeys.deprecatedKeys.shouldNot(contain key "viewport_width_percent")
    SurfaceConfigSchemaKeys.deprecatedKeys.shouldNot(contain key "display_contextual_toolbar_mode")

    // Retired keys are still known and parsed -- just through `ConfigRegistry` rather than this schema.
    ConfigRegistry.allKeys.should(contain("ui.material"))
    ConfigRegistry.allKeys.should(contain("display.contextual_toolbar_mode"))
    ConfigRegistry.allKeys.should(contain("viewport.height.max"))
  }

  it should "group motion, appearance, and text display settings under AppConfig" in {
    val config = AppConfig.default
      .withMaterialPreset(MaterialPreset.Crystal)
      .withMotionPreset(MotionPreset.Subtle)
      .withElementTransitionSpeedScale(1.75)
      .withCursorTransitionSpeedScale(Some(0.75))
      .withEditorInsertionTransitionKind(TransitionKind.TypedText)
      .withTextAreaInsets(TextAreaInsets.fromPercent(20.0, 10.0))
      .withViewportSizing(
        ViewportSizing(
          width = ViewportAxisSizing.fromPercent(80.0, Some(120)),
          height = ViewportAxisSizing.fromPercent(90.0, Some(40))
        )
      )
      .withWordWrap(false)
      .withFocusedTextBody(true)
      .withContextualToolbarDisplayMode(ToolbarDisplayMode.TextOnly)

    config.surfaceConfig.materialPreset.shouldBe(MaterialPreset.Crystal)
    config.surfaceConfig.motionPreset.shouldBe(MotionPreset.Subtle)
    config.surfaceConfig.elementTransitionSpeedScale.shouldBe(1.75)
    config.surfaceConfig.cursorTransitionSpeedScale.shouldBe(Some(0.75))
    config.surfaceConfig.editorInsertionTransitionKind.shouldBe(TransitionKind.TypedText)
    config.surfaceConfig.textAreaInsets.shouldBe(TextAreaInsets.fromPercent(20.0, 10.0))
    config.surfaceConfig.viewportSizing.shouldBe(
      ViewportSizing(
        width = ViewportAxisSizing.fromPercent(80.0, Some(120)),
        height = ViewportAxisSizing.fromPercent(90.0, Some(40))
      )
    )
    config.surfaceConfig.wordWrapEnabled.shouldBe(false)
    config.surfaceConfig.focusedTextBodyEnabled.shouldBe(true)
    config.surfaceConfig.contextualToolbarDisplayMode.shouldBe(ToolbarDisplayMode.TextOnly)
  }

  it should "default the command runner's persistent key-hint footer to on, and expose a with-helper to toggle it" in {
    AppConfig.default.surfaceConfig.commandRunnerShowKeyHints.shouldBe(true)

    val disabled = AppConfig.default.withCommandRunnerShowKeyHints(false)
    disabled.surfaceConfig.commandRunnerShowKeyHints.shouldBe(false)

    disabled.withCommandRunnerShowKeyHints(true).surfaceConfig.commandRunnerShowKeyHints.shouldBe(true)
  }

  it should "apply accessibility motion overrides after independent family settings" in {
    val editor = MotionFamilyConfig(true, TransitionKind.TypedText, None, 0.5)
    val panels = MotionFamilyConfig(true, TransitionKind.OutlineThenContent, None, 1.5)
    val config = AppConfig.default.withMotionConfiguration(
      MotionConfig(
        MotionAccessibility.Off,
        MotionPreset.Smooth,
        Map(MotionFamily.EditorText -> editor, MotionFamily.PinnedPanels -> panels)
      )
    )

    config.surfaceConfig.effectiveMotionConfiguration.family(MotionFamily.EditorText).enabled shouldBe false
    config.surfaceConfig.effectiveMotionConfiguration.family(MotionFamily.PinnedPanels).enabled shouldBe false
    config.scaledCharacterAnimation shouldBe None
    config.scaledCommandRunnerAnimation shouldBe None
    config.scaledUiAnimation shouldBe None
    config.elementTransitionSettings.enabled shouldBe false
    config.editorInsertionTransitionSettings.enabled shouldBe false
  }

  it should "preserve the accessibility override when applying a named motion preset" in {
    val config = AppConfig.default
      .withMotionConfiguration(
        MotionConfig(
          MotionAccessibility.Off,
          MotionPreset.Smooth,
          Map(
            MotionFamily.EditorText -> MotionFamilyConfig(
              enabled = true,
              transitionKind = TransitionKind.TypedText,
              animation = com.serenity.animation.AnimationConfig.subtle,
              speedScale = 0.5
            )
          )
        )
      )
      .withMotionPreset(MotionPreset.Expressive)

    val motion = config.surfaceConfig.motionConfiguration
      .getOrElse(fail("Expected authoritative motion configuration"))
    motion.accessibility shouldBe MotionAccessibility.Off
    motion.baseline shouldBe MotionPreset.Expressive
    motion.families(MotionFamily.EditorText).speedScale shouldBe 1.0
    config.scaledCharacterAnimation shouldBe None
  }

  it should "resolve omitted families from the legacy baseline and use family animation values" in {
    val commandAnimation = com.serenity.animation.AnimationConfig.subtle
    val config = AppConfig.default.withMotionConfiguration(
      MotionConfig(
        MotionAccessibility.Standard,
        MotionPreset.Smooth,
        Map(MotionFamily.CommandSurfaces -> MotionFamilyConfig(true, TransitionKind.TypedText, commandAnimation, 0.5))
      )
    )

    config.effectivePanelOpenTransitionKind shouldBe TransitionKind.OutlineThenContent
    config.scaledCommandRunnerAnimation shouldBe commandAnimation.map(_.scaledBy(0.5)).flatten
  }

  it should "use the hierarchy baseline and pinned-panel family timing for panel transitions" in {
    val panelAnimation = com.serenity.animation.AnimationConfig.subtle
    val config = AppConfig.default.withMotionConfiguration(
      MotionConfig(
        MotionAccessibility.Standard,
        MotionPreset.Expressive,
        Map(
          MotionFamily.PinnedPanels -> MotionFamilyConfig(
            enabled = true,
            transitionKind = TransitionKind.DirectionalSweep,
            animation = panelAnimation,
            speedScale = 0.5
          )
        )
      )
    )

    config.elementTransitionSettings.baseTiming shouldBe MotionPreset.Expressive.elementTransitionSettings.baseTiming
    config.pinnedPanelTransitionSettings.speedScale shouldBe 0.5
    config.pinnedPanelTransitionSettings.baseTiming.durationMs shouldBe panelAnimation.fold(
      fail("missing panel animation")
    )(_.durationMs)
    config.pinnedPanelTransitionSettings.overrides(TransitionScope.PanelOpen) shouldBe TransitionKind.DirectionalSweep
  }

  it should "update UI transition speed without changing pinned panel speed" in {
    val configured = AppConfig.default.withMotionPreset(MotionPreset.Smooth)
    val panelSpeed = configured.surfaceConfig.effectiveMotionConfiguration.family(MotionFamily.PinnedPanels).speedScale

    val updated = configured.withUiTransitionSpeedScale(Some(2.0))

    updated.surfaceConfig.effectiveMotionConfiguration.family(MotionFamily.UiTransitions).speedScale shouldBe 2.0
    updated.surfaceConfig.effectiveMotionConfiguration.family(MotionFamily.PinnedPanels).speedScale shouldBe panelSpeed
  }

  it should "update UI animation timing without changing pinned panel timing" in {
    val configured = AppConfig.default.withMotionPreset(MotionPreset.Smooth)
    val panelAnimation =
      configured.surfaceConfig.effectiveMotionConfiguration.family(MotionFamily.PinnedPanels).animation

    val updated = configured.withUiAnimation(com.serenity.animation.AnimationConfig.subtle)

    updated.surfaceConfig.effectiveMotionConfiguration.family(MotionFamily.UiTransitions).animation shouldBe
      com.serenity.animation.AnimationConfig.subtle
    updated.surfaceConfig.effectiveMotionConfiguration
      .family(MotionFamily.PinnedPanels)
      .animation shouldBe panelAnimation
  }

  it should "default the contextual toolbar display mode to icons and text" in
    AppConfig.default.surfaceConfig.contextualToolbarDisplayMode.shouldBe(ToolbarDisplayMode.IconAndText)

  it should "leave interface settings owned by InterfaceConfig" in {
    val config = AppConfig.default
      .withInterfaceDensity(InterfaceDensity.Spacious)
      .withUiElementGap(3)
      .withUiCornerRadiusPx(12)
      .withUiOutlineThicknessPx(4)

    config.interfaceConfig.shouldBe(
      InterfaceConfig(
        density = InterfaceDensity.Spacious,
        elementGap = 3,
        cornerRadiusPx = 12,
        outlineThicknessPx = 4
      )
    )
  }

  it should "parse surface motion config entries centrally" in {
    val motionConfig =
      SurfaceConfigSchemaParser
        .parse(AppConfig.default, "ui.motion", "reduced")
        .getOrElse(fail("motion preset parse"))
    val speedScaleConfig =
      SurfaceConfigSchemaParser
        .parse(AppConfig.default, "ui.motion.speed_scale", "1.75")
        .getOrElse(fail("motion speed scale parse"))
    val editorTextSpeedScaleConfig =
      SurfaceConfigSchemaParser
        .parse(AppConfig.default, "ui_motion_editor_text_speed_scale", "0.5")
        .getOrElse(fail("editor text speed scale parse"))
    val commandRunnerSpeedScaleConfig =
      SurfaceConfigSchemaParser
        .parse(AppConfig.default, "ui.motion.command.runner.speed_scale", "2.25")
        .getOrElse(fail("command runner speed scale parse"))
    val uiSpeedScaleConfig =
      SurfaceConfigSchemaParser
        .parse(AppConfig.default, "ui.motion.ui_elements.speed_scale", "1.25")
        .getOrElse(fail("ui speed scale parse"))
    val cursorSpeedScaleConfig =
      SurfaceConfigSchemaParser
        .parse(AppConfig.default, "ui.motion.cursor.speed.scale", "0.75")
        .getOrElse(fail("cursor speed scale parse"))
    val editorTextConfig =
      SurfaceConfigSchemaParser
        .parse(AppConfig.default, "ui.motion.editor_text", "typed")
        .getOrElse(fail("editor text transition parse"))
    val panelOpenConfig =
      SurfaceConfigSchemaParser
        .parse(AppConfig.default, "ui.motion.panel.open", "directional")
        .getOrElse(fail("panel open transition parse"))
    val panelCloseConfig =
      SurfaceConfigSchemaParser
        .parse(AppConfig.default, "ui_motion_panel_close", "off")
        .getOrElse(fail("panel close transition parse"))
    val commandRunnerRevealConfig =
      SurfaceConfigSchemaParser
        .parse(AppConfig.default, "ui.motion.command.runner.reveal", "outline")
        .getOrElse(fail("command runner reveal transition parse"))
    val commandRunnerAnimationConfig =
      SurfaceConfigSchemaParser
        .parse(AppConfig.default, "ui.motion.command_runner", "subtle")
        .getOrElse(fail("command runner animation parse"))
    val uiAnimationConfig =
      SurfaceConfigSchemaParser
        .parse(AppConfig.default, "ui_motion_ui", "smooth")
        .getOrElse(fail("ui animation parse"))

    motionConfig.surfaceConfig.motionPreset.shouldBe(MotionPreset.Reduced)
    speedScaleConfig.surfaceConfig.elementTransitionSpeedScale.shouldBe(1.75)
    editorTextSpeedScaleConfig.surfaceConfig.editorTextTransitionSpeedScale.shouldBe(Some(0.5))
    commandRunnerSpeedScaleConfig.surfaceConfig.commandRunnerTransitionSpeedScale.shouldBe(Some(2.25))
    uiSpeedScaleConfig.surfaceConfig.uiTransitionSpeedScale.shouldBe(Some(1.25))
    cursorSpeedScaleConfig.surfaceConfig.cursorTransitionSpeedScale.shouldBe(Some(0.75))
    editorTextConfig.surfaceConfig.editorInsertionTransitionKind.shouldBe(TransitionKind.TypedText)
    panelOpenConfig.surfaceConfig.panelOpenTransitionKind.shouldBe(Some(TransitionKind.DirectionalSweep))
    panelCloseConfig.surfaceConfig.panelCloseTransitionKind.shouldBe(Some(TransitionKind.Disabled))
    commandRunnerRevealConfig.surfaceConfig.commandRunnerTransitionKind.shouldBe(
      Some(TransitionKind.OutlineThenContent)
    )
    commandRunnerAnimationConfig.surfaceConfig.commandRunnerAnimation.shouldBe(
      com.serenity.animation.AnimationConfig.subtle
    )
    uiAnimationConfig.surfaceConfig.uiAnimation.shouldBe(com.serenity.animation.AnimationConfig.smooth)
    SurfaceConfigSchemaParser.parse(AppConfig.default, "ui.motion", "turbo").shouldBe(None)
  }

  it should "validate surface motion config entries centrally" in {
    SurfaceConfigSchemaParser.invalidValue("ui.motion", "reduced").shouldBe(false)
    SurfaceConfigSchemaParser.invalidValue("ui.motion", "turbo").shouldBe(true)
    SurfaceConfigSchemaParser.invalidValue("ui.motion.speed_scale", "1.75").shouldBe(false)
    SurfaceConfigSchemaParser.invalidValue("ui.motion.speed_scale", "5").shouldBe(true)
    SurfaceConfigSchemaParser.invalidValue("ui.motion.editor_text", "typed").shouldBe(false)
    SurfaceConfigSchemaParser.invalidValue("ui.motion.command_runner_reveal", "sideways").shouldBe(true)
  }
