package com.serenity.ui.layout

import java.awt.Font
import java.awt.font.{TextAttribute, TextHitInfo, TextLayout}
import java.text.AttributedString

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class TmpKernProbeSpec extends AnyFlatSpec with Matchers:

  private val frc = TextLayoutSnapshot.defaultFontRenderContext()

  private def kerned(face: Font): Font =
    face.deriveFont(java.util.Map.of(TextAttribute.KERNING, TextAttribute.KERNING_ON))

  "TMPPROBE" should "report what each face does" in {
    val text = "Hello, wide world: AVAWAY To WWWW Ty Yo iiii"
    val faces = List(
      "serif"      -> Font(Font.SERIF, Font.PLAIN, 14),
      "inter"      -> BundledInterFont.plain(14f),
      "liberation" -> Font("Liberation Serif", Font.PLAIN, 15)
    )
    val report = faces.map {
      case (name, plain) =>
        val face    = kerned(plain)
        val nominal = face.createGlyphVector(frc, text)
        val shaped  = face.layoutGlyphVector(frc, text.toCharArray, 0, text.length, Font.LAYOUT_LEFT_TO_RIGHT)
        val unkernedShaped = plain.layoutGlyphVector(frc, text.toCharArray, 0, text.length, Font.LAYOUT_LEFT_TO_RIGHT)
        def kerning(gv: java.awt.font.GlyphVector)(index: Int): Float =
          if index == 0 || index == text.length then 0.0f
          else gv.getGlyphPosition(index).getX.toFloat - gv.getGlyphPosition(index - 1).getX.toFloat - nominal.getGlyphMetrics(index - 1).getAdvance
        val attributed = AttributedString(text)
        attributed.addAttribute(TextAttribute.FONT, face)
        val layout = TextLayout(attributed.getIterator, frc)
        val carets = Vector.tabulate(text.length)(i => layout.getCaretInfo(TextHitInfo.leading(i))(0)) :+ layout.getAdvance
        val stops  = TextLayoutSnapshot.caretXsForText(text, face, frc)
        val kernedPairs = text.indices.filter(i => kerning(shaped)(i) != 0.0f).map(i => s"$i:${kerning(shaped)(i)}")
        val maxDeviation = stops.indices.map(i => math.abs(stops(i) - (carets(i) + kerning(shaped)(i) / 2.0f))).max
        s"$name family=${face.getFamily} fontName=${face.getFontName} glyphs=${shaped.getNumGlyphs} kernedPairs(shaped,KERNING_ON)=${kernedPairs.take(10)} " +
          s"kernedPairs(layoutGlyphVector of plain font)=${text.indices.count(i => kerning(unkernedShaped)(i) != 0.0f)} maxDeviation=$maxDeviation " +
          s"textLayoutAdvance=${layout.getAdvance} nominalAdvance=${nominal.getLogicalBounds.getWidth}"
    }
    fail("TMPPROBE\n" + report.mkString("\n"))
  }
