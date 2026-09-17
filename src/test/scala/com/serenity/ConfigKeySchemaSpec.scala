package com.serenity

import com.serenity.config.{ConfigKeySchema, ConfigManager}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ConfigKeySchemaSpec extends AnyFlatSpec with Matchers:

  "ConfigKeySchema" should "classify current, deprecated, and dynamic config keys" in {
    ConfigKeySchema.isKnownKey("typography.code.family") shouldBe true
    ConfigKeySchema.isKnownKey("font_code_family") shouldBe true
    ConfigKeySchema.isKnownKey("lsp.scala.command") shouldBe true
    ConfigKeySchema.isKnownKey("hotkey.command_palette") shouldBe true
    ConfigKeySchema.isKnownKey("keymap.command_runner.submit") shouldBe true
    ConfigKeySchema.isKnownKey("editor.cursor.mode") shouldBe true
    ConfigKeySchema.isKnownKey("cursor_mode") shouldBe true
    ConfigKeySchema.isKnownKey("character.animation.duration_ms") shouldBe true
    ConfigKeySchema.isKnownKey("character.animation.steps") shouldBe true
    ConfigKeySchema.isKnownKey("motion.cursor.speed_scale") shouldBe true
    ConfigKeySchema.isKnownKey("ui_motion_cursor_speed_scale") shouldBe true
    ConfigKeySchema.isKnownKey("editor.markdown_view") shouldBe true
    ConfigKeySchema.isKnownKey("document_markdown_view") shouldBe true
    ConfigKeySchema.isKnownKey("editor.text_area.top") shouldBe true
    ConfigKeySchema.isKnownKey("text_area_bottom_percent") shouldBe true
    ConfigKeySchema.isKnownKey("unknown.setting") shouldBe false
  }

  it should "report deprecated key replacements from the central schema" in {
    ConfigKeySchema.deprecatedReplacement("cursor_mode").shouldBe(Some("editor.cursor.mode"))
    ConfigKeySchema.deprecatedReplacement("cursor_info_bar") shouldBe Some("status.segments")
    ConfigKeySchema.deprecatedReplacement("document_markdown_view").shouldBe(Some("editor.markdown_view"))
    ConfigKeySchema.deprecatedReplacement("ui_motion_cursor_speed_scale") shouldBe Some(
      "motion.cursor.speed_scale"
    )
    ConfigKeySchema.deprecatedReplacement("typography.code.family") shouldBe None
  }

  // The namespaces are workspace, editor, status, typography, ui, motion and window; every spelling from before the
  // rename still parses and is pointed at its new home.
  it should "map every pre-namespace spelling to its current key" in {
    ConfigKeySchema.deprecatedReplacement("display.word_wrap") shouldBe Some("editor.word_wrap")
    ConfigKeySchema.deprecatedReplacement("font.code.family") shouldBe Some("typography.code.family")
    ConfigKeySchema.deprecatedReplacement("cursor.mode") shouldBe Some("editor.cursor.mode")
    ConfigKeySchema.deprecatedReplacement("interface.density") shouldBe Some("ui.density")
    ConfigKeySchema.deprecatedReplacement("command_runner.visible_rows") shouldBe Some("ui.command_runner.visible_rows")
    ConfigKeySchema.deprecatedReplacement("render.fps") shouldBe Some("ui.render.fps")
    ConfigKeySchema.deprecatedReplacement("app.mode") shouldBe Some("workspace.mode")
    ConfigKeySchema.deprecatedReplacement("document.default_mode") shouldBe Some("editor.default_document_mode")
    ConfigKeySchema.deprecatedReplacement("companion.sprite.enabled") shouldBe Some("ui.companion_sprite.enabled")
    ConfigKeySchema.deprecatedReplacement("ui.motion.preset") shouldBe Some("motion.preset")
    ConfigKeySchema.deprecatedReplacement("ui.motion.family.cursor.transition") shouldBe
      Some("motion.family.cursor.transition")
    ConfigKeySchema.deprecatedReplacement("character.animation.preset") shouldBe Some("motion.character.preset")
    ConfigKeySchema.deprecatedReplacement("viewport.width.max") shouldBe Some("window.viewport.width_max")
    ConfigKeySchema.isKnownKey("ui.motion.family.cursor.transition") shouldBe true
  }

  it should "know every static key rendered by ConfigManager" in {
    val renderedKeys = ConfigManager
      .configToString(com.serenity.config.AppConfig.default)
      .split("\n")
      .toList
      .map(_.trim)
      .filter(line => line.nonEmpty && !line.startsWith("#"))
      .flatMap(_.split("=", 2).headOption)
      .map(_.trim.stripPrefix("\"").stripSuffix("\"").toLowerCase)

    renderedKeys.filterNot(ConfigKeySchema.isKnownKey) shouldBe Nil
  }
