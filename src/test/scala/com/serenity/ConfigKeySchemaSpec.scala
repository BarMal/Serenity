package com.serenity

import com.serenity.config.{ConfigKeySchema, ConfigManager}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ConfigKeySchemaSpec extends AnyFlatSpec with Matchers:

  "ConfigKeySchema" should "classify current, deprecated, and dynamic config keys" in {
    ConfigKeySchema.isKnownKey("font.code.family") shouldBe true
    ConfigKeySchema.isKnownKey("font_code_family") shouldBe true
    ConfigKeySchema.isKnownKey("lsp.scala.command") shouldBe true
    ConfigKeySchema.isKnownKey("hotkey.command_palette") shouldBe true
    ConfigKeySchema.isKnownKey("keymap.command_runner.submit") shouldBe true
    ConfigKeySchema.isKnownKey("ui.motion.cursor.speed_scale") shouldBe true
    ConfigKeySchema.isKnownKey("ui_motion_cursor_speed_scale") shouldBe true
    ConfigKeySchema.isKnownKey("text_area.top.percent") shouldBe true
    ConfigKeySchema.isKnownKey("text_area_bottom_percent") shouldBe true
    ConfigKeySchema.isKnownKey("unknown.setting") shouldBe false
  }

  it should "report deprecated key replacements from the central schema" in {
    ConfigKeySchema.deprecatedReplacement("font_size") shouldBe Some("font.code.size and font.text.size")
    ConfigKeySchema.deprecatedReplacement("cursor_info_bar") shouldBe Some("cursor.info_bar")
    ConfigKeySchema.deprecatedReplacement("ui_motion_cursor_speed_scale") shouldBe Some(
      "ui.motion.cursor.speed_scale"
    )
    ConfigKeySchema.deprecatedReplacement("font.code.family") shouldBe None
  }

  it should "know every static key rendered by ConfigManager" in {
    val renderedKeys = ConfigManager
      .configToString(com.serenity.config.AppConfig.default)
      .split("\n")
      .toList
      .map(_.trim)
      .filter(line => line.nonEmpty && !line.startsWith("#"))
      .flatMap(_.split("=", 2).headOption)
      .map(_.trim.toLowerCase)

    renderedKeys.filterNot(ConfigKeySchema.isKnownKey) shouldBe Nil
  }
