package com.serenity.manuscript.typography

import com.serenity.manuscript.{ManuscriptFormat, PaperSize}

enum LineSpacing:
  /** A multiple of the font's natural line height: 2.0 is double spacing. */
  case Multiple(factor: Double)

  /** A fixed distance between baselines, in points. */
  case Exact(points: Float)

enum LineAlignment:
  case Ragged, Justified

final case class PageMargins(top: Float, right: Float, bottom: Float, left: Float)

object PageMargins:
  def uniform(points: Float): PageMargins = PageMargins(points, points, points, points)

/** How a manuscript is set on the page, in points. This is the print counterpart of [[ManuscriptFormat]] and is derived
  * from it, so the DOCX and the paginated PDF cannot disagree about a margin or an indent. It is deliberately separate
  * from the screen typography settings: zoom and themes never reach a submission.
  */
final case class PageTypography(
    paper: PaperSize,
    margins: PageMargins,
    body: FontSpec,
    lineSpacing: LineSpacing,
    firstLineIndent: Float,
    paragraphGap: Float,
    alignment: LineAlignment,
    widows: Int,
    orphans: Int,
    runningHead: String,
    sceneBreak: String,
    chapterDrop: Double
):
  def pageWidth: Float  = PageTypography.points(paper.widthTwips)
  def pageHeight: Float = PageTypography.points(paper.heightTwips)

  def textWidth: Float  = pageWidth - margins.left - margins.right
  def textHeight: Float = pageHeight - margins.top - margins.bottom

object PageTypography:

  private val TwipsPerPoint = 20f

  private[typography] def points(twips: Int): Float = twips / TwipsPerPoint

  def fromFormat(format: ManuscriptFormat, family: FontFamily): PageTypography =
    PageTypography(
      paper = format.paper,
      margins = PageMargins.uniform(points(format.marginTwips)),
      body = FontSpec(family, FaceStyle.Regular, format.fontSizePoints.toFloat),
      lineSpacing = LineSpacing.Multiple(format.lineSpacing / 240.0),
      firstLineIndent = points(format.firstLineIndentTwips),
      paragraphGap = 0f,
      alignment = LineAlignment.Ragged,
      widows = 2,
      orphans = 2,
      runningHead = format.runningHeader,
      sceneBreak = format.sceneBreak,
      chapterDrop = format.chapterDrop
    )

  /** Shunn's Standard Manuscript Format in its Courier variant, on US Letter. */
  val StandardManuscriptCourier: PageTypography =
    fromFormat(ManuscriptFormat.Classic, FontFamily.CourierPrime)

  val StandardManuscriptCourierA4: PageTypography =
    StandardManuscriptCourier.copy(paper = PaperSize.A4)

/** The named typography presets. Paper is chosen separately, since every preset exists on Letter and A4. */
enum TypographyPreset(val key: String, private val base: PageTypography):
  case StandardManuscriptCourier extends TypographyPreset("smf-courier", PageTypography.StandardManuscriptCourier)

  def typography(paper: PaperSize): PageTypography = base.copy(paper = paper)

object TypographyPreset:
  def fromKey(key: String): Option[TypographyPreset] =
    TypographyPreset.values.find(_.key == key.trim.toLowerCase)
