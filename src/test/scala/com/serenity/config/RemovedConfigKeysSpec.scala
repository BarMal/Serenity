package com.serenity.config

import java.nio.file.{Files, Paths}

import com.serenity.session.given
import com.serenity.session.{SessionConfigCodec, SessionState}
import io.circe.Json
import org.scalatest.OptionValues
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Settings that were deleted outright must not break a config file or session that still names them: the file loads,
  * every other setting in it still applies, and each removed key is reported once as removed and ignored.
  */
class RemovedConfigKeysSpec extends AnyFlatSpec with Matchers with OptionValues:

  private val keptSetting = "editor.word_wrap = false"

  private def load(lines: Seq[String]): ConfigLoadResult =
    val file = Files.createTempFile("serenity-removed-keys", ".conf")
    Files.writeString(file, lines.mkString("", "\n", "\n"))
    ConfigManagerTestSupport.loadConfigResult(Some(file.toString))

  private def assertRemovedAndIgnored(entries: (String, String)*): Unit =
    val withoutRemoved = load(Seq(keptSetting))
    val result         = load(keptSetting +: entries.map((key, value) => s"$key = $value"))
    val removedKeys    = entries.map(_._1).toList

    result.config shouldBe withoutRemoved.config
    result.report.removedKeys should contain theSameElementsAs removedKeys
    result.report.unknownKeys shouldBe Nil
    result.report.invalidEntries shouldBe Nil
    result.report.deprecatedEntries shouldBe Nil

    val messageLines =
      ConfigMigrationWarning.message(Paths.get("config.conf"), result.report).value.linesIterator.toList
    removedKeys.foreach { key =>
      val mentions = messageLines.filter(_.trim.startsWith(s"- $key "))
      withClue(s"warning lines naming $key: ")(mentions should have size 1)
      mentions.headOption.value should include("removed")
      mentions.headOption.value should include("ignored")
    }

  private def assertSessionIgnores(fields: (String, Json)*): Unit =
    val encoded = SessionConfigCodec.encode(AppConfig.default).asObject.value
    val withRemoved = Json.fromJsonObject(fields.foldLeft(encoded) {
      case (json, (key, value)) => json.add(key, value)
    })
    SessionConfigCodec.decode(withRemoved.hcursor) shouldBe AppConfig.default

  "A config file naming a removed setting" should "load and report it once as removed" in
    assertRemovedAndIgnored("ui.post_processing" -> "scanlines-glow")

  it should "not report a removed key as unknown when it is the only entry" in {
    val result = load(Seq("ui.post_processing = glow"))
    result.config shouldBe AppConfig.default
    result.report.removedKeys shouldBe List("ui.post_processing")
    result.report.hasWarnings shouldBe true
  }

  "A session written before a setting was removed" should "still decode, ignoring the removed field" in
    assertSessionIgnores("postProcessingEffect" -> Json.fromString("Glow"))

  "A config file naming the removed panel material, shadow and corner settings" should "load and report each once" in
    assertRemovedAndIgnored(
      "ui.material"         -> "crystal",
      "ui.shadows"          -> "false",
      "ui.background_style" -> "glass-like",
      "ui.blur_radius"      -> "0.42",
      "ui.corner_radius"    -> "12"
    )

  it should "treat their older spellings as removed too" in
    assertRemovedAndIgnored(
      "ui_material"         -> "clear",
      "material.preset"     -> "solid",
      "material_preset"     -> "frosted",
      "ui_shadows"          -> "true",
      "ui.background.style" -> "solid",
      "ui_background_style" -> "transparent",
      "ui.blur.radius"      -> "0.1",
      "ui_blur_radius"      -> "0.2",
      "ui.corner.radius"    -> "4",
      "ui_corner_radius"    -> "6"
    )

  "A session saved with panel material, shadow and corner settings" should "still decode, ignoring them" in
    assertSessionIgnores(
      "materialPreset"   -> Json.fromString("crystal"),
      "uiShadowsEnabled" -> Json.fromBoolean(false),
      "backgroundStyle"  -> Json.fromString("GlassLike"),
      "blurRadius"       -> Json.fromDoubleOrNull(0.42),
      "uiCornerRadiusPx" -> Json.fromInt(12)
    )

  "A config file naming the removed companion sprite and visual flair settings" should "load and report each once" in
    assertRemovedAndIgnored(
      "ui.companion_sprite.enabled"                  -> "true",
      "ui.companion_sprite.character"                -> "pixel-wizard",
      "ui.companion_sprite.position"                 -> "left",
      "ui.companion_sprite.size"                     -> "12",
      "ui.companion_sprite.typing_cycle"             -> "pulse",
      "ui.companion_sprite.typing_active_ticks"      -> "4",
      "ui.companion_sprite.typing_fast_active_ticks" -> "2",
      "ui.companion_sprite.typing_fast_threshold_ms" -> "90",
      "ui.visual_flair"                              -> "reduced"
    )

  it should "treat their older spellings as removed too" in
    assertRemovedAndIgnored(
      "companion.sprite.enabled"      -> "true",
      "companion.sprite.typing.cycle" -> "blink",
      "companion.sprite.size"         -> "8",
      "visual.flair.level"            -> "off",
      "ui.companion_sprite.frames"    -> "4"
    )

  "A session saved with companion sprite settings" should "still decode, ignoring them" in
    assertSessionIgnores(
      "ui.companion_sprite.enabled"   -> Json.True,
      "ui.companion_sprite.character" -> Json.fromString("pixel-wizard"),
      "ui.companion_sprite.size"      -> Json.fromInt(12),
      "ui.visual_flair"               -> Json.fromString("reduced")
    )

  "A config file naming the removed window translucency setting" should "load and report it once" in
    assertRemovedAndIgnored("window.translucent" -> "true")

  it should "treat its older spelling as removed too" in
    assertRemovedAndIgnored("window_translucent" -> "false")

  "A session saved with window translucency" should "still decode, ignoring it" in
    assertSessionIgnores("window.translucent" -> Json.False)

  // ---- the motion and animation settings -------------------------------------------------------------------------

  private val motionFamilies = List(
    "cursor",
    "editor_text",
    "command_surfaces",
    "pinned_panels",
    "ui_transitions",
    "column_transitions",
    "panel_geometry",
    "selection_geometry"
  )

  private val motionFamilyFields =
    List(
      "enabled",
      "transition",
      "animation",
      "animation.preset",
      "animation.duration_ms",
      "animation.steps",
      "speed_scale"
    )

  private def familyKeys(prefix: String): List[String] =
    motionFamilies.flatMap(family => motionFamilyFields.map(field => s"$prefix$family.$field")) ++
      List(s"${prefix}pinned_panels.open_transition", s"${prefix}pinned_panels.close_transition")

  private val currentMotionKeys = List(
    "motion.preset",
    "motion.accessibility",
    "motion.speed_scale",
    "motion.editor_text.speed_scale",
    "motion.command_runner.speed_scale",
    "motion.ui.speed_scale",
    "motion.cursor.speed_scale",
    "motion.command_runner",
    "motion.command_runner_reveal",
    "motion.ui",
    "motion.editor_text",
    "motion.panel_open",
    "motion.panel_close",
    "motion.character.preset",
    "motion.character.duration_ms",
    "motion.character.steps"
  ) ++ familyKeys("motion.family.")

  private val olderMotionKeys = List(
    "ui.motion.preset",
    "ui.motion",
    "ui_motion",
    "motion_preset",
    "ui.motion.accessibility",
    "ui.motion.speed_scale",
    "ui_motion_speed_scale",
    "motion_speed_scale",
    "ui.motion.editor_text.speed_scale",
    "ui.motion.editor.text.speed_scale",
    "ui_motion_editor_text_speed_scale",
    "ui.motion.command_runner.speed_scale",
    "ui.motion.command.runner.speed_scale",
    "ui_motion_command_runner_speed_scale",
    "ui.motion.ui.speed_scale",
    "ui.motion.ui_elements.speed_scale",
    "ui.motion.ui.elements.speed_scale",
    "ui_motion_ui_speed_scale",
    "ui.motion.cursor.speed_scale",
    "ui.motion.cursor_speed_scale",
    "ui.motion.cursor.speed.scale",
    "ui_motion_cursor_speed_scale",
    "ui.motion.command_runner",
    "ui.motion.command.runner",
    "ui_motion_command_runner",
    "ui.motion.command_runner_reveal",
    "ui.motion.command.runner.reveal",
    "ui_motion_command_runner_reveal",
    "ui.motion.ui",
    "ui.motion.ui_elements",
    "ui.motion.ui.elements",
    "ui_motion_ui",
    "ui.motion.editor_text",
    "ui.motion.editor.text",
    "ui_motion_editor_text",
    "ui.motion.panel_open",
    "ui.motion.panel.open",
    "ui_motion_panel_open",
    "ui.motion.panel_close",
    "ui.motion.panel.close",
    "ui_motion_panel_close",
    "character.animation",
    "character.animation.preset",
    "character.animation.duration_ms",
    "character.animation.duration.ms",
    "character.animation.steps",
    "character_animation",
    "character_animation_duration_ms",
    "character_animation_steps"
  ) ++ familyKeys("ui.motion.family.")

  "A config file naming the removed motion settings" should "load and report each current key once" in
    assertRemovedAndIgnored(currentMotionKeys.map(_ -> "smooth")*)

  it should "treat every older spelling as removed too" in
    assertRemovedAndIgnored(olderMotionKeys.map(_ -> "smooth")*)

  it should "load the old default config, motion block included, as the default config" in {
    val oldDefault =
      scala.io.Source.fromResource("compat/default-config-with-motion.conf")(using scala.io.Codec.UTF8).mkString
    val file = Files.createTempFile("serenity-old-default", ".conf")
    Files.writeString(file, oldDefault)
    val result = ConfigManagerTestSupport.loadConfigResult(Some(file.toString))

    result.config shouldBe AppConfig.default
    result.report.unknownKeys shouldBe Nil
    result.report.invalidEntries shouldBe Nil
    result.report.deprecatedEntries shouldBe Nil
    result.report.removedKeys should not be empty
    all(result.report.removedKeys.map(key => key.startsWith("motion."))) shouldBe true

    val messageLines =
      ConfigMigrationWarning.message(Paths.get("config.conf"), result.report).value.linesIterator.toList
    result.report.removedKeys.foreach { key =>
      withClue(s"warning lines naming $key: ")(messageLines.filter(_.trim.startsWith(s"- $key ")) should have size 1)
    }
  }

  "A session saved before the motion settings were removed" should "still decode, keeping every other setting" in {
    val stored =
      scala.io.Source.fromResource("compat/session-with-motion-fields.json")(using scala.io.Codec.UTF8).mkString
    val json = _root_.io.circe.parser.parse(stored).getOrElse(fail("the stored session is not JSON"))
    json.hcursor.downField("config").downField("motionConfiguration").succeeded shouldBe true

    val decoded = json.as[SessionState].getOrElse(fail("the stored session no longer decodes"))

    decoded.config shouldBe AppConfig.default.withWordWrap(false).withLineNumbers(false)
  }
