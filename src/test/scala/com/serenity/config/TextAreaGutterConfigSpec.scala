package com.serenity.config

import java.nio.file.Files

import com.serenity.TestTemp
import com.serenity.session.SessionConfigCodec
import io.circe.Json
import org.scalatest.OptionValues
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** `editor.text_area.{left,right,top,bottom}` are gutters: a bare number or N% is a share of the width (as it always
  * was), and a side may also be given in cells, in ch, or (left and right) as auto.
  */
class TextAreaGutterConfigSpec extends AnyFlatSpec with Matchers with OptionValues:

  private def loaded(content: String): ConfigLoadResult =
    val file = TestTemp.file("serenity-text-area-gutters", ".conf")
    Files.writeString(file, content)
    ConfigManagerTestSupport.loadConfigResult(Some(file.toString))

  "The text area settings" should "read units, and write them back as they were given" in {
    val result = loaded(
      """editor.text_area.left = 12cells
        |editor.text_area.right = auto
        |editor.text_area.top = 3 cells
        |editor.text_area.bottom = 7.5%
        |""".stripMargin
    )
    val insets = result.config.surfaceConfig.textAreaInsets

    result.report.invalidEntries shouldBe Nil
    insets.left shouldBe Gutter.Cells(12)
    insets.right shouldBe Gutter.Auto
    insets.top shouldBe Gutter.Cells(3)
    insets.bottom shouldBe Gutter.Percent(0.075)

    val written = ConfigManager.configToString(result.config)
    written should include("editor.text_area.left = 12cells")
    written should include("editor.text_area.right = auto")
    written should include("editor.text_area.top = 3cells")
    written should include("editor.text_area.bottom = 7.5")
  }

  it should "read ch on the sides that run along the line" in {
    val insets =
      loaded("editor.text_area.left = 4ch\neditor.text_area.right = 4ch\n").config.surfaceConfig.textAreaInsets

    insets.left shouldBe Gutter.Ch(4)
    insets.right shouldBe Gutter.Ch(4)
  }

  it should "still read a bare number as a percentage and write it as one" in {
    val config = loaded("editor.text_area.left = 12.5\neditor.text_area.top = 7.5\n").config

    config.surfaceConfig.textAreaInsets.left shouldBe Gutter.Percent(0.125)
    ConfigManager.configToString(config) should include("editor.text_area.left = 12.5")
  }

  it should "report auto on a top or bottom side, and ch there, as invalid, keeping the default" in {
    val result = loaded("editor.text_area.top = auto\neditor.text_area.bottom = 2ch\n")

    result.report.invalidEntries.map(_.key) should contain allOf ("editor.text_area.top", "editor.text_area.bottom")
    result.config.surfaceConfig.textAreaInsets.top shouldBe Gutter.Percent(0.0)
    result.config.surfaceConfig.textAreaInsets.bottom shouldBe Gutter.Percent(0.0)
  }

  it should "name the units it accepts when it refuses a value" in {
    val reason = loaded("editor.text_area.left = wide\n").report.invalidEntries
      .find(_.key == "editor.text_area.left")
      .map(_.reason)
      .getOrElse("")

    reason should include("%")
    reason should include("cells")
    reason should include("ch")
    reason should include("auto")
  }

  "A saved session" should "still read the insets it saved as fractions of the width" in {
    val old = Json.obj(
      "left"   -> Json.fromDoubleOrNull(0.1),
      "right"  -> Json.fromDoubleOrNull(0.2),
      "top"    -> Json.fromDoubleOrNull(0.05),
      "bottom" -> Json.fromDoubleOrNull(0.0)
    )
    val stored = SessionConfigCodec.encode(AppConfig.default).asObject.value.add("textAreaInsets", old)

    val decoded = SessionConfigCodec.decode(Json.fromJsonObject(stored).hcursor)

    decoded.surfaceConfig.textAreaInsets shouldBe
      TextAreaInsets.percent(left = 0.1, right = 0.2, top = 0.05, bottom = 0.0)
  }

  it should "keep gutters in every unit across a save and a load" in {
    val insets = TextAreaInsets(Gutter.Cells(12), Gutter.Auto, Gutter.Cells(3), Gutter.Percent(0.075))
    val config = AppConfig.default.withTextAreaInsets(insets)

    val encoded = SessionConfigCodec.encode(config)
    val decoded = SessionConfigCodec.decode(encoded.hcursor)

    decoded.surfaceConfig.textAreaInsets shouldBe insets
  }

  it should "keep a ch gutter across a save and a load" in {
    val insets = TextAreaInsets(Gutter.Ch(4), Gutter.Ch(4), Gutter.Percent(0.0), Gutter.Percent(0.0))
    val config = AppConfig.default.withTextAreaInsets(insets)

    SessionConfigCodec.decode(SessionConfigCodec.encode(config).hcursor).surfaceConfig.textAreaInsets shouldBe insets
  }
