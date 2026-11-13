package com.serenity.ui.layout

import java.awt.Font
import java.awt.font.{GlyphVector, TextAttribute}

import com.serenity.ui.layout.TextMeasurer.LineFonts
import com.serenity.ui.layout.TextMeasureCorpus.PinnedFont
import com.serenity.ui.renderer.FontSpec
import com.serenity.ui.skia.{SkiaFontCache, SkiaTextMeasurer}

/** Why Skia and AWT disagree for a bundled font: which face and size each side measures with, and where along one line
  * their caret positions part. Written into the parity report so a CI run shows what a local run cannot.
  */
private[layout] object TextMeasureDiagnosis:

  private val ProbeText   = "Buffer 1 - unsaved Hamburgefonstiv"
  private val DiagnosedId = "kerning_prose"
  private val Shown       = 24

  def lines(pinned: PinnedFont, measurer: SkiaTextMeasurer, faces: SkiaFontCache): Vector[String] =
    val awt      = pinned.font
    val frc      = TextLayoutSnapshot.defaultFontRenderContext()
    val skia     = faces.faceFor(awt).font
    val typeface = skia.getTypeface
    val header = Vector(
      s"### ${pinned.label}",
      "",
      s"- AWT: family `${awt.getFamily}`, name `${awt.getFontName}`, ps `${awt.getPSName}`, size ${awt.getSize2D}, " +
        f"probe width ${awt.getStringBounds(ProbeText, frc).getWidth}%.4f",
      s"- Skia: family `${typeface.getFamilyName}`, bold ${typeface.isBold}, italic ${typeface.isItalic}, " +
        s"size ${skia.getSize}, scaleX ${skia.getScaleX}, subpixel ${skia.isSubpixel}, linear ${skia.isLinearMetrics}, " +
        f"probe width ${skia.measureTextWidth(ProbeText)}%.4f"
    )
    header ++ TextMeasureCorpus.lines
      .find(_.id == DiagnosedId)
      .toVector
      .flatMap(line => positions(pinned, line.text, measurer))

  private def positions(pinned: PinnedFont, text: String, measurer: SkiaTextMeasurer): Vector[String] =
    val frc   = TextLayoutSnapshot.defaultFontRenderContext()
    val chars = text.toCharArray
    val plain = glyphXs(pinned.font, chars, frc)
    val kerned =
      glyphXs(pinned.font.deriveFont(java.util.Map.of(TextAttribute.KERNING, TextAttribute.KERNING_ON)), chars, frc)
    val skiaXs = measurer.rawCaretXs(text, 0, LineFonts(FontSpec.fromAwt(pinned.font), Vector.empty))
    val rows = (0 until math.min(Shown, chars.length)).map { index =>
      val character = if chars(index) == ' ' then "space" else chars(index).toString
      f"| $index | $character | ${plain(index)}%.4f | ${kerned(index)}%.4f | ${skiaXs(index)}%.4f | ${skiaXs(index) - plain(index)}%+.4f |"
    }
    Vector(
      "",
      s"Caret x of the first $Shown characters of `$DiagnosedId`:",
      "",
      "| i | char | AWT | AWT kerning on | Skia | Skia - AWT |",
      "|---|---|---|---|---|---|"
    ) ++ rows :+ ""

  private def glyphXs(font: Font, chars: Array[Char], frc: java.awt.font.FontRenderContext): Vector[Float] =
    val vector: GlyphVector = font.layoutGlyphVector(frc, chars, 0, chars.length, Font.LAYOUT_LEFT_TO_RIGHT)
    (0 to vector.getNumGlyphs).map(index => vector.getGlyphPosition(index).getX.toFloat).toVector
