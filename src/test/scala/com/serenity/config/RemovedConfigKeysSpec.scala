package com.serenity.config

import java.nio.file.{Files, Paths}

import com.serenity.session.SessionConfigCodec
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
      val mentions = messageLines.filter(_.contains(key))
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
