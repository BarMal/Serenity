package com.serenity.config

import com.serenity.manuscript.PaperSize
import com.serenity.manuscript.typography.{LineSpacing, PageMargins, PageTypography, TypographyPreset}
import com.typesafe.config.ConfigFactory
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ExportTypographyConfigSpec extends AnyFlatSpec with Matchers:

  private val keys = List(
    "export.typography.preset",
    "export.typography.paper",
    "export.typography.font_size",
    "export.typography.line_spacing",
    "export.typography.margin",
    "export.typography.first_line_indent"
  )

  private def read(config: AppConfig, settings: (String, String)*): AppConfig =
    settings.foldLeft(config) {
      case (acc, (key, value)) =>
        ConfigRegistry.read(acc, key, value).getOrElse(fail(s"$key = $value was refused"))
    }

  "the export typography settings" should "default to Standard Manuscript Format Courier on Letter" in {
    AppConfig.default.exportTypographyConfig.pageTypography shouldBe PageTypography.StandardManuscriptCourier
  }

  it should "be registered under export.typography, and written to the config file" in {
    keys.foreach(key => withClue(key)(ConfigRegistry.find(key) should not be empty))
    val written = ConfigFileFormat.settings(AppConfig.default).map(_._1)

    keys.filterNot(written.contains) shouldBe empty
  }

  it should "choose the A4 paper" in {
    read(AppConfig.default, "export.typography.paper" -> "A4").exportTypographyConfig.pageTypography shouldBe
      PageTypography.StandardManuscriptCourierA4
  }

  it should "apply overrides on top of the preset" in {
    val configured = read(
      AppConfig.default,
      "export.typography.font_size"         -> "11",
      "export.typography.line_spacing"      -> "1.5",
      "export.typography.margin"            -> "54",
      "export.typography.first_line_indent" -> "18"
    ).exportTypographyConfig.pageTypography

    configured.body.sizePoints shouldBe 11f
    configured.lineSpacing shouldBe LineSpacing.Multiple(1.5)
    configured.margins shouldBe PageMargins.uniform(54f)
    configured.firstLineIndent shouldBe 18f
    configured.copy(
      body = PageTypography.StandardManuscriptCourier.body,
      lineSpacing = PageTypography.StandardManuscriptCourier.lineSpacing,
      margins = PageTypography.StandardManuscriptCourier.margins,
      firstLineIndent = PageTypography.StandardManuscriptCourier.firstLineIndent
    ) shouldBe PageTypography.StandardManuscriptCourier
  }

  it should "read auto as no override" in {
    val configured = read(
      AppConfig.default,
      "export.typography.font_size" -> "11",
      "export.typography.font_size" -> "auto"
    )

    configured.exportTypographyConfig.fontSize shouldBe None
  }

  it should "refuse a preset, paper or number it cannot honour" in
    List(
      "export.typography.preset"            -> "smf-serif",
      "export.typography.paper"             -> "tabloid",
      "export.typography.font_size"         -> "2",
      "export.typography.font_size"         -> "huge",
      "export.typography.line_spacing"      -> "0",
      "export.typography.margin"            -> "-1",
      "export.typography.margin"            -> "400",
      "export.typography.first_line_indent" -> "-3"
    ).foreach((key, value) => withClue(s"$key = $value")(ConfigRegistry.rejects(key, value) shouldBe true))

  it should "round-trip through the rendered config file" in {
    val configured = AppConfig.default.copy(exportTypographyConfig =
      ExportTypographyConfig(
        preset = TypographyPreset.StandardManuscriptCourier,
        paper = PaperSize.A4,
        fontSize = Some(11.5f),
        lineSpacing = Some(1.5),
        margin = Some(54f),
        firstLineIndent = Some(18f)
      )
    )
    val parsed = ConfigFactory.parseString(ConfigFileFormat.render(configured))

    val restored = ConfigRegistry.fields
      .filter(field => keys.contains(field.key))
      .foldLeft(AppConfig.default)((acc, field) =>
        field.readValue(acc, parsed.getValue(field.key)).getOrElse(fail(s"${field.key} did not read back"))
      )

    restored.exportTypographyConfig shouldBe configured.exportTypographyConfig
  }
