package com.serenity

import com.serenity.animation.TransitionKind
import com.serenity.config.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class SurfaceConfigSpec extends AnyFlatSpec with Matchers:

  "PostProcessingEffect" should "parse supported configuration values" in {
    PostProcessingEffect.fromConfigKey("off") shouldBe Some(PostProcessingEffect.Off)
    PostProcessingEffect.fromConfigKey("crt") shouldBe Some(PostProcessingEffect.Scanlines)
    PostProcessingEffect.fromConfigKey("glow") shouldBe Some(PostProcessingEffect.Glow)
  }

  "SurfaceConfig" should "own the surface-related schema metadata" in {
    SurfaceConfig.Schema.currentKeys.should(contain("ui.material"))
    SurfaceConfig.Schema.currentKeys.should(contain("ui.motion.cursor.speed_scale"))
    SurfaceConfig.Schema.currentKeys.should(contain("display.contextual_toolbar_mode"))
    SurfaceConfig.Schema.currentKeys.should(contain("viewport.height.max"))

    SurfaceConfig.Schema.deprecatedKeys("ui_motion_cursor_speed_scale").shouldBe("ui.motion.cursor.speed_scale")
    SurfaceConfig.Schema.deprecatedKeys("viewport_width_percent").shouldBe("viewport.width.percent")
    SurfaceConfig.Schema.deprecatedKeys("display_contextual_toolbar_mode").shouldBe("display.contextual_toolbar_mode")
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

  it should "parse surface display config entries centrally" in {
    val commandRunnerConfig =
      SurfaceConfig.Schema
        .parse(AppConfig.default, "command.runner.visible.rows", "7")
        .getOrElse(fail("command runner visible rows parse"))
    val renderFpsConfig =
      SurfaceConfig.Schema
        .parse(AppConfig.default, "render_fps", "uncapped")
        .getOrElse(fail("render fps parse"))
    val wordWrapConfig =
      SurfaceConfig.Schema
        .parse(AppConfig.default, "display.word.wrap", "off")
        .getOrElse(fail("word wrap parse"))
    val focusedTextBodyConfig =
      SurfaceConfig.Schema
        .parse(AppConfig.default, "display_focused_text_body", "on")
        .getOrElse(fail("focused text body parse"))
    val contextualToolbarConfig =
      SurfaceConfig.Schema
        .parse(AppConfig.default, "display.contextual_toolbar", "disabled")
        .getOrElse(fail("contextual toolbar parse"))
    val toolbarModeConfig =
      SurfaceConfig.Schema
        .parse(AppConfig.default, "display.contextual.toolbar.mode", "both")
        .getOrElse(fail("contextual toolbar mode parse"))

    commandRunnerConfig.surfaceConfig.commandRunnerVisibleRows.shouldBe(Some(7))
    SurfaceConfig.Schema
      .parse(AppConfig.default, "command_runner.visible_rows", "auto")
      .map(_.surfaceConfig.commandRunnerVisibleRows)
      .shouldBe(Some(None))
    SurfaceConfig.Schema
      .parse(AppConfig.default, "command_runner.item_gap_rows", "1")
      .map(_.surfaceConfig.commandRunnerItemGapRows)
      .shouldBe(Some(1))
    SurfaceConfig.Schema
      .parse(AppConfig.default, "command_runner.cursor_gap_rows", "3")
      .map(_.surfaceConfig.commandRunnerCursorGapRows)
      .shouldBe(Some(Some(3)))
    renderFpsConfig.surfaceConfig.renderFpsTarget.shouldBe(RenderFpsTarget.Uncapped)
    wordWrapConfig.surfaceConfig.wordWrapEnabled.shouldBe(false)
    focusedTextBodyConfig.surfaceConfig.focusedTextBodyEnabled.shouldBe(true)
    contextualToolbarConfig.surfaceConfig.contextualToolbarEnabled.shouldBe(false)
    toolbarModeConfig.surfaceConfig.contextualToolbarDisplayMode.shouldBe(ToolbarDisplayMode.IconAndText)
    SurfaceConfig.Schema.parse(AppConfig.default, "render.fps", "turbo").shouldBe(None)
  }

  it should "parse surface motion config entries centrally" in {
    val materialConfig =
      SurfaceConfig.Schema
        .parse(AppConfig.default, "ui_material", "crystal")
        .getOrElse(fail("material preset parse"))
    val motionConfig =
      SurfaceConfig.Schema
        .parse(AppConfig.default, "ui.motion", "reduced")
        .getOrElse(fail("motion preset parse"))
    val speedScaleConfig =
      SurfaceConfig.Schema
        .parse(AppConfig.default, "ui.motion.speed_scale", "1.75")
        .getOrElse(fail("motion speed scale parse"))
    val editorTextSpeedScaleConfig =
      SurfaceConfig.Schema
        .parse(AppConfig.default, "ui_motion_editor_text_speed_scale", "0.5")
        .getOrElse(fail("editor text speed scale parse"))
    val commandRunnerSpeedScaleConfig =
      SurfaceConfig.Schema
        .parse(AppConfig.default, "ui.motion.command.runner.speed_scale", "2.25")
        .getOrElse(fail("command runner speed scale parse"))
    val uiSpeedScaleConfig =
      SurfaceConfig.Schema
        .parse(AppConfig.default, "ui.motion.ui_elements.speed_scale", "1.25")
        .getOrElse(fail("ui speed scale parse"))
    val cursorSpeedScaleConfig =
      SurfaceConfig.Schema
        .parse(AppConfig.default, "ui.motion.cursor.speed.scale", "0.75")
        .getOrElse(fail("cursor speed scale parse"))
    val editorTextConfig =
      SurfaceConfig.Schema
        .parse(AppConfig.default, "ui.motion.editor_text", "typed")
        .getOrElse(fail("editor text transition parse"))
    val panelOpenConfig =
      SurfaceConfig.Schema
        .parse(AppConfig.default, "ui.motion.panel.open", "directional")
        .getOrElse(fail("panel open transition parse"))
    val panelCloseConfig =
      SurfaceConfig.Schema
        .parse(AppConfig.default, "ui_motion_panel_close", "off")
        .getOrElse(fail("panel close transition parse"))
    val commandRunnerRevealConfig =
      SurfaceConfig.Schema
        .parse(AppConfig.default, "ui.motion.command.runner.reveal", "outline")
        .getOrElse(fail("command runner reveal transition parse"))
    val commandRunnerAnimationConfig =
      SurfaceConfig.Schema
        .parse(AppConfig.default, "ui.motion.command_runner", "subtle")
        .getOrElse(fail("command runner animation parse"))
    val uiAnimationConfig =
      SurfaceConfig.Schema
        .parse(AppConfig.default, "ui_motion_ui", "smooth")
        .getOrElse(fail("ui animation parse"))

    materialConfig.surfaceConfig.materialPreset.shouldBe(MaterialPreset.Crystal)
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
    SurfaceConfig.Schema.parse(AppConfig.default, "ui.motion", "turbo").shouldBe(None)
  }

  it should "validate surface display config entries centrally" in {
    SurfaceConfig.Schema.invalidValue("command_runner.visible_rows", "7").shouldBe(false)
    SurfaceConfig.Schema.invalidValue("command_runner.visible_rows", "0").shouldBe(true)
    SurfaceConfig.Schema.invalidValue("render.fps", "uncapped").shouldBe(false)
    SurfaceConfig.Schema.invalidValue("render.fps", "turbo").shouldBe(true)
    SurfaceConfig.Schema.invalidValue("display.word_wrap", "off").shouldBe(false)
    SurfaceConfig.Schema.invalidValue("display.word_wrap", "maybe").shouldBe(true)
    SurfaceConfig.Schema.invalidValue("display.focused_text_body", "on").shouldBe(false)
    SurfaceConfig.Schema.invalidValue("display.contextual_toolbar", "disabled").shouldBe(false)
    SurfaceConfig.Schema.invalidValue("display.contextual_toolbar_mode", "both").shouldBe(false)
    SurfaceConfig.Schema.invalidValue("display.contextual_toolbar_mode", "pictures").shouldBe(true)
  }

  it should "parse surface layout config entries centrally" in {
    val leftInsetConfig =
      SurfaceConfig.Schema
        .parse(AppConfig.default, "text_area.left.percent", "20")
        .getOrElse(fail("left inset parse"))
    val rightInsetConfig =
      SurfaceConfig.Schema
        .parse(AppConfig.default, "text.area.right.percent", "10")
        .getOrElse(fail("right inset parse"))
    val topInsetConfig =
      SurfaceConfig.Schema
        .parse(AppConfig.default, "text_area_top_percent", "5")
        .getOrElse(fail("top inset parse"))
    val bottomInsetConfig =
      SurfaceConfig.Schema
        .parse(AppConfig.default, "text_area.bottom.percent", "15")
        .getOrElse(fail("bottom inset parse"))
    val widthPercentConfig =
      SurfaceConfig.Schema
        .parse(AppConfig.default, "viewport_width_percent", "80")
        .getOrElse(fail("viewport width percent parse"))
    val widthMaxConfig =
      SurfaceConfig.Schema
        .parse(AppConfig.default, "viewport.width.max", "")
        .getOrElse(fail("viewport width max parse"))
    val heightPercentConfig =
      SurfaceConfig.Schema
        .parse(AppConfig.default, "viewport.height.percent", "100")
        .getOrElse(fail("viewport height percent parse"))
    val heightMaxConfig =
      SurfaceConfig.Schema
        .parse(AppConfig.default, "viewport_height_max", "50")
        .getOrElse(fail("viewport height max parse"))

    leftInsetConfig.surfaceConfig.textAreaInsets.leftPercent.shouldBe(20.0)
    rightInsetConfig.surfaceConfig.textAreaInsets.rightPercent.shouldBe(10.0)
    topInsetConfig.surfaceConfig.textAreaInsets.topPercent.shouldBe(5.0)
    bottomInsetConfig.surfaceConfig.textAreaInsets.bottomPercent.shouldBe(15.0)
    widthPercentConfig.surfaceConfig.viewportSizing.width.percentValue.shouldBe(80.0)
    widthMaxConfig.surfaceConfig.viewportSizing.width.maxCells.shouldBe(None)
    heightPercentConfig.surfaceConfig.viewportSizing.height.percentValue.shouldBe(100.0)
    heightMaxConfig.surfaceConfig.viewportSizing.height.maxCells.shouldBe(Some(50))
    SurfaceConfig.Schema.parse(AppConfig.default, "viewport.width.percent", "0").shouldBe(None)
  }

  it should "validate surface layout config entries centrally" in {
    SurfaceConfig.Schema.invalidValue("text_area.left.percent", "20").shouldBe(false)
    SurfaceConfig.Schema.invalidValue("text_area.left.percent", "60").shouldBe(true)
    SurfaceConfig.Schema.invalidValue("viewport.width.percent", "80").shouldBe(false)
    SurfaceConfig.Schema.invalidValue("viewport.width.percent", "0").shouldBe(true)
    SurfaceConfig.Schema.invalidValue("viewport.width.max", "").shouldBe(false)
    SurfaceConfig.Schema.invalidValue("viewport.width.max", "0").shouldBe(true)
    SurfaceConfig.Schema.invalidValue("viewport.height.percent", "100").shouldBe(false)
    SurfaceConfig.Schema.invalidValue("viewport.height.max", "50").shouldBe(false)
    SurfaceConfig.Schema.invalidValue("viewport.height.max", "0").shouldBe(true)
  }

  it should "validate surface motion config entries centrally" in {
    SurfaceConfig.Schema.invalidValue("ui.material", "crystal").shouldBe(false)
    SurfaceConfig.Schema.invalidValue("ui.material", "neon").shouldBe(true)
    SurfaceConfig.Schema.invalidValue("ui.motion", "reduced").shouldBe(false)
    SurfaceConfig.Schema.invalidValue("ui.motion", "turbo").shouldBe(true)
    SurfaceConfig.Schema.invalidValue("ui.motion.speed_scale", "1.75").shouldBe(false)
    SurfaceConfig.Schema.invalidValue("ui.motion.speed_scale", "5").shouldBe(true)
    SurfaceConfig.Schema.invalidValue("ui.motion.editor_text", "typed").shouldBe(false)
    SurfaceConfig.Schema.invalidValue("ui.motion.command_runner_reveal", "sideways").shouldBe(true)
  }
