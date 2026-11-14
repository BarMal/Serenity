package com.serenity.config

import java.nio.file.{Files, Paths}

import com.serenity.TestTemp
import com.serenity.session.given
import com.serenity.session.SessionConfigCodec
import io.circe.Json
import org.scalatest.OptionValues
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** `typography.prose.measure` and `editor.column_target_width` are one setting now, `editor.wrap_column`. A file or a
  * session that still names either loads as what it showed before, and the old key is reported once as renamed.
  */
class LegacyWrapColumnKeysSpec extends AnyFlatSpec with Matchers with OptionValues:

  private def load(lines: String*): ConfigLoadResult =
    val file = TestTemp.file("serenity-wrap-column", ".conf")
    Files.writeString(file, lines.mkString("", "\n", "\n"))
    ConfigManagerTestSupport.loadConfigResult(Some(file.toString))

  private def wrapColumnOf(lines: String*): WrapColumn = load(lines*).config.surfaceConfig.wrapColumn

  private val columns = "editor.column_mode = true"
  private val prose   = "workspace.mode = prose"

  "typography.prose.measure" should "become the wrap column in ch" in {
    wrapColumnOf("typography.prose.measure = 66ch") shouldBe WrapColumn.Ch(66)
    wrapColumnOf("prose.measure = 72ch") shouldBe WrapColumn.Ch(72)
  }

  it should "read a bare number as ch, as it always did, and write it back as ch" in
    wrapColumnOf("typography.prose.measure = 66") shouldBe WrapColumn.Ch(66)

  it should "turn off and unlimited into the edge" in {
    wrapColumnOf("typography.prose.measure = off") shouldBe WrapColumn.Edge
    wrapColumnOf("typography.prose.measure = unlimited") shouldBe WrapColumn.Edge
  }

  it should "apply in a code workspace too, which is the point of the one setting" in
    wrapColumnOf("workspace.mode = code", "typography.prose.measure = 66ch") shouldBe WrapColumn.Ch(66)

  "editor.column_target_width" should "become the wrap column in cells while column mode is on" in {
    wrapColumnOf(columns, "editor.column_target_width = 80") shouldBe WrapColumn.Cells(80)
    wrapColumnOf(columns, "editor.column_target_width = unlimited") shouldBe WrapColumn.Edge
  }

  it should "be dropped while column mode is off, where it never had any effect" in {
    wrapColumnOf("editor.column_target_width = 80") shouldBe WrapColumn.Edge
    wrapColumnOf("editor.column_mode = false", "editor.column_target_width = 80") shouldBe WrapColumn.Edge
  }

  it should "still be read from its older spellings" in
    wrapColumnOf(columns, "display.column_target_width = 60") shouldBe WrapColumn.Cells(60)

  "A file naming both old keys" should "take the measure in a prose workspace and the target in column mode" in {
    wrapColumnOf(prose, "typography.prose.measure = 66ch", "editor.column_target_width = 80") shouldBe
      WrapColumn.Ch(66)
    wrapColumnOf(columns, "typography.prose.measure = 66ch", "editor.column_target_width = 80") shouldBe
      WrapColumn.Cells(80)
    wrapColumnOf(prose, columns, "typography.prose.measure = 66ch", "editor.column_target_width = 80") shouldBe
      WrapColumn.Cells(80)
  }

  it should "report both keys once as renamed, and say which value was not carried over" in {
    val result = load(prose, "typography.prose.measure = 66ch", "editor.column_target_width = 80")

    result.report.deprecatedEntries.map(_.key) should contain theSameElementsAs
      List("typography.prose.measure", "editor.column_target_width")
    result.report.deprecatedEntries.foreach(_.replacement should startWith("editor.wrap_column"))
    result.report.deprecatedEntries.find(_.key == "editor.column_target_width").value.replacement should
      include("not carried over")
    result.report.unknownKeys shouldBe Nil
    result.report.removedKeys shouldBe Nil
    result.report.invalidEntries shouldBe Nil
  }

  "editor.wrap_column" should "win over either old key" in {
    wrapColumnOf(prose, "editor.wrap_column = 72", "typography.prose.measure = 66ch") shouldBe WrapColumn.Cells(72)
    wrapColumnOf(columns, "editor.wrap_column = edge", "editor.column_target_width = 80") shouldBe WrapColumn.Edge
  }

  it should "leave the old keys unreported when only it is set" in {
    val result = load("editor.wrap_column = 66ch")

    result.config.surfaceConfig.wrapColumn shouldBe WrapColumn.Ch(66)
    result.report.deprecatedEntries shouldBe Nil
  }

  "An old key with a value it would have refused" should "be reported as invalid and leave the edge" in {
    val result = load("typography.prose.measure = 3ch")

    result.config.surfaceConfig.wrapColumn shouldBe WrapColumn.Edge
    result.report.invalidEntries.map(_.key) should contain("typography.prose.measure")
  }

  "Saving after loading an old key" should "write only editor.wrap_column" in {
    val written = ConfigManager.configToString(
      load("workspace.mode = prose", "typography.prose.measure = 66ch").config
    )

    written should include("editor.wrap_column = 66ch")
    written should not include "typography.prose.measure"
    written should not include "editor.column_target_width"
  }

  "A session written before the wrap column" should "decode the old keys as it would a file, by mode" in {
    def sessionWith(mode: AppMode, columnMode: Boolean, fields: (String, Json)*): AppConfig =
      val encoded = SessionConfigCodec.encode(AppConfig.default.withAppMode(mode).withColumnMode(columnMode)).asObject.value
      val withOld = fields.foldLeft(encoded.remove("editor.wrap_column")) { case (json, (key, value)) =>
        json.add(key, value)
      }
      SessionConfigCodec.decode(Json.fromJsonObject(withOld).hcursor)

    sessionWith(AppMode.Prose, columnMode = false, "typography.prose.measure" -> Json.fromInt(66)).surfaceConfig.wrapColumn shouldBe
      WrapColumn.Ch(66)
    sessionWith(AppMode.Code, columnMode = true, "columnTargetWidthCells" -> Json.fromInt(80)).surfaceConfig.wrapColumn shouldBe
      WrapColumn.Cells(80)
    sessionWith(AppMode.Code, columnMode = false, "columnTargetWidthCells" -> Json.fromInt(80)).surfaceConfig.wrapColumn shouldBe
      WrapColumn.Edge
  }

  it should "round-trip a wrap column written now" in {
    val config = AppConfig.default.withWrapColumn(WrapColumn.Ch(72))

    SessionConfigCodec.decode(SessionConfigCodec.encode(config).hcursor) shouldBe config
  }
