package com.serenity.ui.layout

import java.awt.Font
import java.awt.font.TextAttribute

import com.serenity.ui.layout.TextMeasureCorpus.PinnedFont
import com.serenity.ui.layout.TextMeasureParity.Compared
import com.serenity.ui.layout.TextMeasurer.LineFonts
import com.serenity.ui.renderer.FontSpec
import com.serenity.ui.skia.{SkiaFontCache, SkiaTextMeasurer}

/** Why a Skia number differs from the AWT baseline, as information: the report lists each class, and nothing fails on a
  * flagged difference. A difference is accepted when it is one the owner chose: Skia kerns where AWT does not, a caret
  * inside a ligature is spread across it, a different typeface is drawn.
  */
private[layout] object TextMeasureClassification:

  enum Reason(val label: String, val accepted: Boolean):
    case Typeface       extends Reason("accepted: a different typeface", true)
    case Kerning        extends Reason("accepted: Skia kerns, AWT does not", true)
    case LigatureSpread extends Reason("accepted: a caret spread inside a cluster", true)
    case Rounding
        extends Reason("accepted: Skia rounds up the sum of ascent, descent and leading, AWT each of them", true)
    case Flagged extends Reason("flagged", false)

  final case class Classified(compared: Compared, reason: Reason)

  private val Tolerance = TextMeasureBaseline.FloatTolerance

  def classify(
    compared: Seq[Compared],
    measurer: SkiaTextMeasurer,
    faces: SkiaFontCache
  ): Vector[Classified] =
    val facts = TextMeasureCorpus.fonts.map(pinned => pinned.label -> pinned).toMap
    compared.toVector.map { difference =>
      val parts = difference.key.split('.')
      val text  = parts.lift(1).flatMap(id => TextMeasureCorpus.lines.find(_.id == id)).map(_.text).getOrElse("")
      val found = facts
        .get(parts.headOption.getOrElse(""))
        .map(pinned => reasonFor(difference, parts.lastOption.getOrElse(""), pinned, text, measurer, faces))
      Classified(difference, found.getOrElse(Reason.Flagged))
    }

  private def reasonFor(
    difference: Compared,
    kind: String,
    pinned: PinnedFont,
    text: String,
    measurer: SkiaTextMeasurer,
    faces: SkiaFontCache
  ): Reason =
    val family = faces.faceFor(FontSpec.fromAwt(pinned.font)).font.getTypeface.getFamilyName
    if !family.equalsIgnoreCase(pinned.font.getFamily) then Reason.Typeface
    else if isLineHeight(difference, kind) then Reason.Rounding
    else if kind != "carets" then Reason.Flagged
    else if insideCluster(difference.index, pinned, text, measurer) then Reason.LigatureSpread
    else if matchesKerningAwt(difference, pinned, text) then Reason.Kerning
    else Reason.Flagged

  /** The cell's line height and the one-row heights, which differ by at most the pixel the two roundings can part by.
    */
  private def isLineHeight(difference: Compared, kind: String): Boolean =
    ((kind == "cell" && difference.index == 1) || kind.startsWith("heights")) && difference.size <= 1.0

  private def insideCluster(index: Int, pinned: PinnedFont, text: String, measurer: SkiaTextMeasurer): Boolean =
    val fonts = LineFonts(FontSpec.fromAwt(pinned.font), Vector.empty)
    val starts = measurer
      .itemise(text, 0, text.length, 0, fonts)
      .flatMap(run => measurer.shape(text, 0, run, 0.toByte).clusters.map(_ + run.startColumn))
      .toSet
    index > 0 && index < text.length && !starts.contains(index)

  private def matchesKerningAwt(difference: Compared, pinned: PinnedFont, text: String): Boolean =
    val kerning = pinned.font.deriveFont(java.util.Map.of(TextAttribute.KERNING, TextAttribute.KERNING_ON))
    val vector = kerning.layoutGlyphVector(
      TextLayoutSnapshot.defaultFontRenderContext(),
      text.toCharArray,
      0,
      text.length,
      Font.LAYOUT_LEFT_TO_RIGHT
    )
    vector.getNumGlyphs == text.length &&
    difference.index <= text.length &&
    math.abs(vector.getGlyphPosition(difference.index).getX - difference.skia) <= Tolerance
