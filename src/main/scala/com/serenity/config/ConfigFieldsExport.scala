package com.serenity.config

import java.util.Locale

import com.serenity.manuscript.PaperSize
import com.serenity.manuscript.typography.TypographyPreset

/** How exported and printed manuscripts are set. */
private[config] object ConfigFieldsExport:

  import ConfigFieldSyntax.*
  import FieldCodec.*

  private val preset: FieldCodec[TypographyPreset] =
    enumerated(TypographyPreset.fromKey, _.key)

  private val paper: FieldCodec[PaperSize] =
    enumerated(text => PaperSize.fromKey(text.toLowerCase(Locale.ROOT)), _.key)

  private def points(range: (Float, Float)): FieldCodec[Option[Float]] =
    float.filtered(value => value >= range._1 && value <= range._2).orAuto

  private val lineSpacing: FieldCodec[Option[Double]] =
    val (min, max) = ExportTypographyConfig.LineSpacingRange
    double.filtered(value => value >= min && value <= max).orAuto

  val fields: List[ConfigField[?]] = List(
    field("export.typography.preset")(preset)(
      _.exportTypographyConfig.preset,
      (config, value) => config.withExportTypography(config.exportTypographyConfig.copy(preset = value))
    ),
    field("export.typography.paper")(paper)(
      _.exportTypographyConfig.paper,
      (config, value) => config.withExportTypography(config.exportTypographyConfig.copy(paper = value))
    ),
    field("export.typography.font_size")(points(ExportTypographyConfig.FontSizeRange))(
      _.exportTypographyConfig.fontSize,
      (config, value) => config.withExportTypography(config.exportTypographyConfig.copy(fontSize = value))
    ),
    field("export.typography.line_spacing")(lineSpacing)(
      _.exportTypographyConfig.lineSpacing,
      (config, value) => config.withExportTypography(config.exportTypographyConfig.copy(lineSpacing = value))
    ),
    field("export.typography.margin")(points(ExportTypographyConfig.MarginRange))(
      _.exportTypographyConfig.margin,
      (config, value) => config.withExportTypography(config.exportTypographyConfig.copy(margin = value))
    ),
    field("export.typography.first_line_indent")(points(ExportTypographyConfig.FirstLineIndentRange))(
      _.exportTypographyConfig.firstLineIndent,
      (config, value) => config.withExportTypography(config.exportTypographyConfig.copy(firstLineIndent = value))
    )
  )
