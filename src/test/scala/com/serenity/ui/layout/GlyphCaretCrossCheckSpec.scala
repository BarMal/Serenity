package com.serenity.ui.layout

import java.awt.Font
import java.awt.font.{TextAttribute, TextHitInfo, TextLayout}
import java.text.AttributedString

import com.serenity.ui.renderer.ShapedTextCache
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** A caret stop used to be what `TextLayout` reports, which puts a kerned character half way between its kerned and
  * unkerned positions. Stops now sit at the glyph, so against `TextLayout` they differ by exactly half the pair's
  * kerning, and nowhere else: the move changes nothing but that.
  */
class GlyphCaretCrossCheckSpec extends AnyFlatSpec with Matchers:

  private val frc = TextLayoutSnapshot.defaultFontRenderContext()

  private def kerned(face: Font): Font =
    face.deriveFont(java.util.Map.of(TextAttribute.KERNING, TextAttribute.KERNING_ON))

  private val faces = List(
    "kerned platform serif" -> kerned(Font(Font.SERIF, Font.PLAIN, 14)),
    "kerned Inter"          -> kerned(BundledInterFont.plain(14f)),
    "kerned Liberation"     -> kerned(Font("Liberation Serif", Font.PLAIN, 15))
  )

  private def textLayoutLeadingCarets(text: String, face: Font): Vector[Float] =
    val attributed = AttributedString(text)
    attributed.addAttribute(TextAttribute.FONT, face)
    val layout = TextLayout(attributed.getIterator, frc)
    Vector.tabulate(text.length)(index => layout.getCaretInfo(TextHitInfo.leading(index))(0)) :+ layout.getAdvance

  "Caret stops at the glyph" should "differ from TextLayout's by half the kerning of the pair before, and by nothing else" in
    faces.foreach {
      case (name, face) =>
        val text    = "Hello, wide world: AVAWAY To WWWW Ty Yo iiii"
        val stops   = TextLayoutSnapshot.caretXsForText(text, face, ShapedTextCache(), frc)
        val layout  = textLayoutLeadingCarets(text, face)
        val nominal = face.createGlyphVector(frc, text)
        val shaped  = face.layoutGlyphVector(frc, text.toCharArray, 0, text.length, Font.LAYOUT_LEFT_TO_RIGHT)
        def kerning(index: Int): Float =
          if index == 0 || index == text.length then 0.0f
          else
            shaped.getGlyphPosition(index).getX.toFloat - shaped.getGlyphPosition(index - 1).getX.toFloat -
              nominal.getGlyphMetrics(index - 1).getAdvance

        shaped.getNumGlyphs shouldBe text.length
        stops.indices.foreach { index =>
          withClue(s"$name, column $index: ")(stops(index) shouldBe layout(index) + kerning(index) / 2.0f +- 0.01f)
        }
        text.indices.map(kerning).exists(_ != 0.0f) shouldBe true
    }
