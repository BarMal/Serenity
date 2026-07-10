package com.serenity

import java.awt.Color

import com.serenity.config.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class CursorConfigSpec extends AnyFlatSpec with Matchers:

  "CursorConfig" should "own cursor schema metadata" in {
    CursorConfig.Schema.currentKeys.should(
      contain allOf (
        "cursor.mode",
        "cursor.active.color",
        "cursor.inactive.color",
        "cursor.info_bar",
        "cursor.info.bar",
        "cursor.info_bar.placement",
        "cursor.info.bar.placement"
      )
    )

    CursorConfig.Schema.deprecatedKeys.should(
      contain allOf (
        "cursor_mode"               -> "cursor.mode",
        "cursor_active_color"       -> "cursor.active.color",
        "cursor_info_bar_placement" -> "cursor.info_bar.placement"
      )
    )
  }

  it should "group cursor mode, colours, and info-bar settings under AppConfig" in {
    val active   = new Color(0x22, 0x44, 0x88)
    val inactive = new Color(0x88, 0x44, 0x22, 0x99)
    val config = AppConfig.default
      .withCursorMode(CursorMode.Breathe)
      .withCursorColors(CursorColorConfig(Some(active), Some(inactive)))
      .withCursorInfoBarMode(CursorInfoBarMode.Detailed)
      .withCursorInfoBarPlacement(CursorInfoBarPlacement.PinnedBottom)

    config.cursorConfig.shouldBe(
      CursorConfig(
        mode = CursorMode.Breathe,
        colors = CursorColorConfig(Some(active), Some(inactive)),
        infoBarMode = CursorInfoBarMode.Detailed,
        infoBarPlacement = CursorInfoBarPlacement.PinnedBottom
      )
    )
  }

  it should "parse cursor config values centrally" in {
    CursorMode.fromConfigKey("breathing").shouldBe(Some(CursorMode.Breathe))
    CursorInfoBarMode.fromConfigKey("minimal").shouldBe(Some(CursorInfoBarMode.Position))
    CursorInfoBarPlacement.fromConfigKey("bottom").shouldBe(Some(CursorInfoBarPlacement.PinnedBottom))
    CursorMode.fromConfigKey("unknown").shouldBe(None)
  }
