package com.serenity.config

import com.serenity.manuscript.PaperSize
import com.serenity.manuscript.typography.{LineSpacing, PageMargins, PageTypography, TypographyPreset}

/** How exported and printed manuscripts are set, under `export.typography.*`. It is separate from `typography.*`, which
  * is the screen: zoom and themes never reach a submission. Each override is `None` ("auto") to keep the preset's own
  * value.
  */
final case class ExportTypographyConfig(
    preset: TypographyPreset = TypographyPreset.StandardManuscriptCourier,
    paper: PaperSize = PaperSize.Letter,
    fontSize: Option[Float] = None,
    lineSpacing: Option[Double] = None,
    margin: Option[Float] = None,
    firstLineIndent: Option[Float] = None
):

  def pageTypography: PageTypography =
    val base = preset.typography(paper)
    base.copy(
      body = base.body.copy(sizePoints = fontSize.getOrElse(base.body.sizePoints)),
      lineSpacing = lineSpacing.fold(base.lineSpacing)(LineSpacing.Multiple(_)),
      margins = margin.fold(base.margins)(PageMargins.uniform),
      firstLineIndent = firstLineIndent.getOrElse(base.firstLineIndent)
    )

object ExportTypographyConfig:
  val FontSizeRange: (Float, Float)        = (6f, 72f)
  val LineSpacingRange: (Double, Double)   = (0.5, 4.0)
  val MarginRange: (Float, Float)          = (0f, 200f)
  val FirstLineIndentRange: (Float, Float) = (0f, 144f)
