package com.serenity

import com.serenity.animation.TransitionKind
import com.serenity.config.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class SurfaceConfigSpec extends AnyFlatSpec with Matchers:

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
    renderFpsConfig.surfaceConfig.renderFpsTarget.shouldBe(RenderFpsTarget.Uncapped)
    wordWrapConfig.surfaceConfig.wordWrapEnabled.shouldBe(false)
    focusedTextBodyConfig.surfaceConfig.focusedTextBodyEnabled.shouldBe(true)
    contextualToolbarConfig.surfaceConfig.contextualToolbarEnabled.shouldBe(false)
    toolbarModeConfig.surfaceConfig.contextualToolbarDisplayMode.shouldBe(ToolbarDisplayMode.IconAndText)
    SurfaceConfig.Schema.parse(AppConfig.default, "render.fps", "turbo").shouldBe(None)
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
