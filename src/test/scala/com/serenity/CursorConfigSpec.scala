package com.serenity

import java.awt.Color

import com.serenity.config.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class CursorConfigSpec extends AnyFlatSpec with Matchers:

  "CursorConfig" should "own cursor schema metadata" in {
    ConfigKeySchema.isKnownKey("editor.cursor.mode") shouldBe true
    ConfigKeySchema.isKnownKey("editor.cursor.active_color") shouldBe true
    ConfigKeySchema.isKnownKey("editor.cursor.inactive_color") shouldBe true
    ConfigKeySchema.isKnownKey("cursor.info_bar") shouldBe true
    ConfigKeySchema.isKnownKey("cursor.info.bar") shouldBe true
    ConfigKeySchema.isKnownKey("status.placement") shouldBe true
    ConfigKeySchema.isKnownKey("cursor.info.bar.placement") shouldBe true
    ConfigKeySchema.isKnownKey("status.foreground_color") shouldBe true
    ConfigKeySchema.isKnownKey("status.background_color") shouldBe true

    ConfigKeySchema.deprecatedKeys.should(
      contain allOf (
        "cursor_mode"               -> "editor.cursor.mode",
        "cursor_active_color"       -> "editor.cursor.active_color",
        "cursor_info_bar_placement" -> "status.placement"
      )
    )
  }

  it should "group cursor mode and colours under AppConfig, and the status line beside it" in {
    val active     = new Color(0x22, 0x44, 0x88)
    val inactive   = new Color(0x88, 0x44, 0x22, 0x99)
    val foreground = new Color(0x11, 0x22, 0x33)
    val background = new Color(0x44, 0x55, 0x66, 0x77)
    val config = AppConfig.default
      .withCursorMode(CursorMode.Breathe)
      .withCursorColors(CursorColorConfig(Some(active), Some(inactive)))
      .withStatusLineSegments(List(StatusSegment.Position, StatusSegment.Title))
      .withStatusLinePlacement(StatusLinePlacement.Pinned)
      .withStatusLineColors(StatusLineColors(Some(foreground), Some(background)))

    config.cursorConfig.shouldBe(
      CursorConfig(mode = CursorMode.Breathe, colors = CursorColorConfig(Some(active), Some(inactive)))
    )
    config.statusLine.shouldBe(
      StatusLineConfig(
        segments = List(StatusSegment.Position, StatusSegment.Title),
        placement = StatusLinePlacement.Pinned,
        colors = StatusLineColors(Some(foreground), Some(background))
      )
    )
  }

  it should "default the cursor info bar's foreground/background colours to the active theme (no override)" in {
    AppConfig.default.statusLine.colors shouldBe StatusLineColors(None, None, None)
  }

  it should "parse cursor config values centrally" in {
    CursorMode.fromConfigKey("breathing").shouldBe(Some(CursorMode.Breathe))
    StatusSegment.parseList("minimal").shouldBe(Some(List(StatusSegment.Position)))
    StatusLinePlacement.fromConfigKey("bottom").shouldBe(Some(StatusLinePlacement.Pinned))
    CursorMode.fromConfigKey("unknown").shouldBe(None)
  }

  it should "parse cursor config entries centrally" in {
    val active     = new Color(0x33, 0x66, 0xcc)
    val inactive   = new Color(0xcc, 0x66, 0x33, 0x80)
    val foreground = new Color(0x11, 0x22, 0x33)
    val background = new Color(0x44, 0x55, 0x66, 0x80)
    val modeConfig =
      ConfigRegistry
        .read(AppConfig.default, "cursor_mode", "breathing")
        .getOrElse(fail("cursor mode parse"))
    val activeColorConfig =
      ConfigRegistry
        .read(AppConfig.default, "editor.cursor.active_color", "#3366CC")
        .getOrElse(fail("active cursor colour parse"))
    val inactiveColorConfig =
      ConfigRegistry
        .read(AppConfig.default, "cursor_inactive_color", "#CC663380")
        .getOrElse(fail("inactive cursor colour parse"))
    val segmentsConfig =
      ConfigRegistry
        .read(AppConfig.default, "status.segments", "minimal")
        .getOrElse(fail("status segments parse"))
    val placementConfig =
      ConfigRegistry
        .read(AppConfig.default, "status_placement", "bottom")
        .getOrElse(fail("status placement parse"))
    val infoBarForegroundConfig =
      ConfigRegistry
        .read(AppConfig.default, "status.foreground_color", "#112233")
        .getOrElse(fail("info bar foreground colour parse"))
    val infoBarBackgroundConfig =
      ConfigRegistry
        .read(AppConfig.default, "status.background_color", "#44556680")
        .getOrElse(fail("info bar background colour parse"))

    modeConfig.cursorConfig.mode.shouldBe(CursorMode.Breathe)
    activeColorConfig.cursorConfig.colors.active.shouldBe(Some(active))
    inactiveColorConfig.cursorConfig.colors.inactive.shouldBe(Some(inactive))
    segmentsConfig.statusLine.segments.shouldBe(List(StatusSegment.Position))
    placementConfig.statusLine.placement.shouldBe(StatusLinePlacement.Pinned)
    infoBarForegroundConfig.statusLine.colors.foreground.shouldBe(Some(foreground))
    infoBarBackgroundConfig.statusLine.colors.background.shouldBe(Some(background))
    ConfigRegistry
      .read(AppConfig.default, "editor.cursor.active_color", "")
      .map(_.cursorConfig.colors.active)
      .shouldBe(Some(None))
    ConfigRegistry.read(AppConfig.default, "editor.cursor.mode", "unknown").shouldBe(None)
  }

  it should "validate cursor config entries centrally" in {
    ConfigRegistry.rejects("editor.cursor.mode", "breathing").shouldBe(false)
    ConfigRegistry.rejects("editor.cursor.mode", "unknown").shouldBe(true)
    ConfigRegistry.rejects("editor.cursor.active_color", "#3366CC").shouldBe(false)
    ConfigRegistry.rejects("editor.cursor.active_color", "").shouldBe(false)
    ConfigRegistry.rejects("editor.cursor.active_color", "not-a-colour").shouldBe(true)
    ConfigRegistry.rejects("status.segments", "minimal").shouldBe(false)
    ConfigRegistry.rejects("status.segments", "sideways").shouldBe(true)
    ConfigRegistry.rejects("status.placement", "bottom").shouldBe(false)
    ConfigRegistry.rejects("status.placement", "sideways").shouldBe(true)
    ConfigRegistry.rejects("status.foreground_color", "#112233").shouldBe(false)
    ConfigRegistry.rejects("status.foreground_color", "").shouldBe(false)
    ConfigRegistry.rejects("status.foreground_color", "not-a-colour").shouldBe(true)
    ConfigRegistry.rejects("status.background_color", "#44556680").shouldBe(false)
    ConfigRegistry.rejects("status.background_color", "not-a-colour").shouldBe(true)
  }
