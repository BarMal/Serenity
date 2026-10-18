package com.serenity.config

import com.serenity.ui.color.RenderColor
import com.serenity.ui.theme.DropCapColour
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class DropCapConfigSpec extends AnyFlatSpec with Matchers:

  private val keys = List("document.drop_cap_gap", "document.drop_cap_colour")

  private def read(settings: (String, String)*): DocumentConfig =
    settings
      .foldLeft(AppConfig.default) {
        case (acc, (key, value)) =>
          ConfigRegistry.read(acc, key, value).getOrElse(fail(s"$key = $value was refused"))
      }
      .documentConfig

  "the drop cap look settings" should "be registered under document, and written to the config file" in {
    val written = ConfigFileFormat.settings(AppConfig.default).map(_._1)

    keys.foreach { key =>
      withClue(key) {
        ConfigRegistry.find(key) should not be empty
        written should contain(key)
      }
    }
    ConfigFileFormat.missingFromLayout shouldBe Nil
  }

  it should "ship with a half-em gap and the accent colour" in {
    ConfigRegistry.defaultFor("document.drop_cap_gap").map(_.rendered) shouldBe Some("0.5")
    ConfigRegistry.defaultFor("document.drop_cap_colour").map(_.rendered) shouldBe Some("accent")
    AppConfig.default.documentConfig.dropCapGapEm shouldBe 0.5f
    AppConfig.default.documentConfig.dropCapColour shouldBe DropCapColour.Accent
  }

  it should "read the gap as a decimal number of em from 0 to 2" in {
    read("document.drop_cap_gap" -> "0").dropCapGapEm shouldBe 0.0f
    read("document.drop_cap_gap" -> "0.75").dropCapGapEm shouldBe 0.75f
    read("document.drop_cap_gap" -> "2").dropCapGapEm shouldBe 2.0f
  }

  it should "read every colour the file can name" in {
    read("document.drop_cap_colour" -> "text").dropCapColour shouldBe DropCapColour.Text
    read("document.drop_cap_colour" -> "heading").dropCapColour shouldBe DropCapColour.Heading
    read("document.drop_cap_colour" -> "accent").dropCapColour shouldBe DropCapColour.Accent
    read("document.drop_cap_colour" -> "#c8501e").dropCapColour shouldBe
      DropCapColour.Fixed(RenderColor.fromRgba(0xc8, 0x50, 0x1e))
  }

  it should "leave the other document settings alone when one is read" in {
    read("document.drop_cap_gap" -> "1.5") shouldBe DocumentConfig().copy(dropCapGapEm = 1.5f)
  }

  it should "refuse a value outside the range or naming no colour" in
    List(
      "document.drop_cap_gap"    -> "-0.1",
      "document.drop_cap_gap"    -> "2.5",
      "document.drop_cap_gap"    -> "wide",
      "document.drop_cap_colour" -> "auto",
      "document.drop_cap_colour" -> "red",
      "document.drop_cap_colour" -> "#12345"
    ).foreach { case (key, value) => withClue(s"$key = $value")(ConfigRegistry.rejects(key, value) shouldBe true) }

  it should "survive being written to the config file and read back" in {
    val configured = AppConfig.default.withDropCapGapEm(1.25f).withDropCapColour(DropCapColour.Heading)
    val fixed      = configured.withDropCapColour(DropCapColour.Fixed(RenderColor.fromRgba(0x12, 0x34, 0x56)))

    List(configured, fixed).foreach { config =>
      val rendered = ConfigFileFormat.settings(config).toMap
      val reread = keys.foldLeft(AppConfig.default) { (acc, key) =>
        ConfigRegistry
          .read(acc, key, rendered(key).rendered.stripPrefix("\"").stripSuffix("\""))
          .getOrElse(fail(s"$key was refused"))
      }
      reread.documentConfig shouldBe config.documentConfig
    }
  }
