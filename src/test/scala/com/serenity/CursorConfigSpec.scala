package com.serenity

import java.awt.Color

import com.serenity.config.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class CursorConfigSpec extends AnyFlatSpec with Matchers:

  "CursorConfig" should "own cursor schema metadata" in {
    ConfigKeySchema.isKnownKey("cursor.mode") shouldBe true
    ConfigKeySchema.isKnownKey("cursor.active.color") shouldBe true
    ConfigKeySchema.isKnownKey("cursor.inactive.color") shouldBe true
    ConfigKeySchema.isKnownKey("cursor.info_bar") shouldBe true
    ConfigKeySchema.isKnownKey("cursor.info.bar") shouldBe true
    ConfigKeySchema.isKnownKey("cursor.info_bar.placement") shouldBe true
    ConfigKeySchema.isKnownKey("cursor.info.bar.placement") shouldBe true
    ConfigKeySchema.isKnownKey("cursor.info_bar.foreground_color") shouldBe true
    ConfigKeySchema.isKnownKey("cursor.info_bar.background_color") shouldBe true

    ConfigKeySchema.deprecatedKeys.should(
      contain allOf (
        "cursor_mode"               -> "cursor.mode",
        "cursor_active_color"       -> "cursor.active.color",
        "cursor_info_bar_placement" -> "cursor.info_bar.placement"
      )
    )
  }

  it should "group cursor mode, colours, and info-bar settings under AppConfig" in {
    val active     = new Color(0x22, 0x44, 0x88)
    val inactive   = new Color(0x88, 0x44, 0x22, 0x99)
    val foreground = new Color(0x11, 0x22, 0x33)
    val background = new Color(0x44, 0x55, 0x66, 0x77)
    val config = AppConfig.default
      .withCursorMode(CursorMode.Breathe)
      .withCursorColors(CursorColorConfig(Some(active), Some(inactive)))
      .withCursorInfoBarSegments(List(CursorInfoBarSegment.Position, CursorInfoBarSegment.Title))
      .withCursorInfoBarPlacement(CursorInfoBarPlacement.PinnedBottom)
      .withCursorInfoBarColors(CursorInfoBarColorConfig(Some(foreground), Some(background)))

    config.cursorConfig.shouldBe(
      CursorConfig(
        mode = CursorMode.Breathe,
        colors = CursorColorConfig(Some(active), Some(inactive)),
        infoBarSegments = List(CursorInfoBarSegment.Position, CursorInfoBarSegment.Title),
        infoBarPlacement = CursorInfoBarPlacement.PinnedBottom,
        infoBarColors = CursorInfoBarColorConfig(Some(foreground), Some(background))
      )
    )
  }

  it should "default the cursor info bar's foreground/background colours to the active theme (no override)" in {
    AppConfig.default.cursorInfoBarColors shouldBe CursorInfoBarColorConfig(None, None)
  }

  it should "parse cursor config values centrally" in {
    CursorMode.fromConfigKey("breathing").shouldBe(Some(CursorMode.Breathe))
    CursorInfoBarSegment.parseList("minimal").shouldBe(Some(List(CursorInfoBarSegment.Position)))
    CursorInfoBarPlacement.fromConfigKey("bottom").shouldBe(Some(CursorInfoBarPlacement.PinnedBottom))
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
        .read(AppConfig.default, "cursor.active.color", "#3366CC")
        .getOrElse(fail("active cursor colour parse"))
    val inactiveColorConfig =
      ConfigRegistry
        .read(AppConfig.default, "cursor_inactive_color", "#CC663380")
        .getOrElse(fail("inactive cursor colour parse"))
    val infoBarModeConfig =
      ConfigRegistry
        .read(AppConfig.default, "cursor.info.bar", "minimal")
        .getOrElse(fail("cursor info-bar mode parse"))
    val placementConfig =
      ConfigRegistry
        .read(AppConfig.default, "cursor_info_bar_placement", "bottom")
        .getOrElse(fail("cursor info-bar placement parse"))
    val infoBarForegroundConfig =
      ConfigRegistry
        .read(AppConfig.default, "cursor.info_bar.foreground_color", "#112233")
        .getOrElse(fail("info bar foreground colour parse"))
    val infoBarBackgroundConfig =
      ConfigRegistry
        .read(AppConfig.default, "cursor.info_bar.background_color", "#44556680")
        .getOrElse(fail("info bar background colour parse"))

    modeConfig.cursorConfig.mode.shouldBe(CursorMode.Breathe)
    activeColorConfig.cursorConfig.colors.active.shouldBe(Some(active))
    inactiveColorConfig.cursorConfig.colors.inactive.shouldBe(Some(inactive))
    infoBarModeConfig.cursorConfig.infoBarSegments.shouldBe(List(CursorInfoBarSegment.Position))
    placementConfig.cursorConfig.infoBarPlacement.shouldBe(CursorInfoBarPlacement.PinnedBottom)
    infoBarForegroundConfig.cursorConfig.infoBarColors.foreground.shouldBe(Some(foreground))
    infoBarBackgroundConfig.cursorConfig.infoBarColors.background.shouldBe(Some(background))
    ConfigRegistry
      .read(AppConfig.default, "cursor.active.color", "")
      .map(_.cursorConfig.colors.active)
      .shouldBe(Some(None))
    ConfigRegistry.read(AppConfig.default, "cursor.mode", "unknown").shouldBe(None)
  }

  it should "validate cursor config entries centrally" in {
    ConfigRegistry.rejects("cursor.mode", "breathing").shouldBe(false)
    ConfigRegistry.rejects("cursor.mode", "unknown").shouldBe(true)
    ConfigRegistry.rejects("cursor.active.color", "#3366CC").shouldBe(false)
    ConfigRegistry.rejects("cursor.active.color", "").shouldBe(false)
    ConfigRegistry.rejects("cursor.active.color", "not-a-colour").shouldBe(true)
    ConfigRegistry.rejects("cursor.info_bar", "minimal").shouldBe(false)
    ConfigRegistry.rejects("cursor.info_bar", "sideways").shouldBe(true)
    ConfigRegistry.rejects("cursor.info_bar.placement", "bottom").shouldBe(false)
    ConfigRegistry.rejects("cursor.info_bar.placement", "sideways").shouldBe(true)
    ConfigRegistry.rejects("cursor.info_bar.foreground_color", "#112233").shouldBe(false)
    ConfigRegistry.rejects("cursor.info_bar.foreground_color", "").shouldBe(false)
    ConfigRegistry.rejects("cursor.info_bar.foreground_color", "not-a-colour").shouldBe(true)
    ConfigRegistry.rejects("cursor.info_bar.background_color", "#44556680").shouldBe(false)
    ConfigRegistry.rejects("cursor.info_bar.background_color", "not-a-colour").shouldBe(true)
  }
