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
