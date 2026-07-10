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
