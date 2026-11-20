package com.serenity.ui.skia

import java.awt.Font

import com.serenity.ui.layout.TextMeasurer.LineFonts
import com.serenity.ui.renderer.FontSpec
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Counting a long prose document measures a paragraph at a time, and a paragraph of printable ASCII in a font that
  * sets it glyph for glyph is what the font's advances and the kerning of each pair say: the font is shaped once for
  * them, not every paragraph in it. What is measured must still be what shaping the paragraph says.
  */
class SkiaAsciiAdvancesSpec extends AnyFlatSpec with Matchers:

  private val Paragraphs = 150

  private val Families = Vector("Inter", "DejaVu Sans", "Liberation Serif")

  private def paragraph(seed: Long): String =
    val random = new scala.util.Random(seed)
    val words  = Vector.fill(60)(random.alphanumeric.take(1 + random.nextInt(9)).mkString)
    val marked = words.map(word => if random.nextInt(9) == 0 then word.capitalize + "," else word)
    marked.mkString(" ").capitalize + "."

  private def measurerOver(ligatures: Boolean): (SkiaTextMeasurer, String => LineFonts) =
    val faces     = new SkiaFontCache()
    val stretches = new SkiaShapedTextCache(ligaturesOf = faces.ligaturesOf)
    val measurer  = new SkiaTextMeasurer(faces, stretches, memos = new ShapingMemos())
    (
      measurer,
      family =>
        LineFonts(FontSpec.fromAwt(Font(family, Font.PLAIN, 14)).copy(ligatures = ligatures)(None), Vector.empty)
    )

  Families.foreach { family =>
    s"A paragraph of printable ASCII in $family" should "be measured without shaping it, once the font has been read" in {
      val (measurer, fontsOf) = measurerOver(ligatures = false)
      val fonts               = fontsOf(family)
      val _                   = measurer.advances(paragraph(0L), 0, paragraph(0L).length, 0, fonts)
      val before              = measurer.shaperRuns

      (1L to Paragraphs).foreach(seed => measurer.advances(paragraph(seed), 0, paragraph(seed).length, 0, fonts))

      (measurer.shaperRuns - before) should be <= 10L
    }

    it should "be measured to the carets shaping it gives" in {
      val (measurer, fontsOf) = measurerOver(ligatures = false)
      val fonts               = fontsOf(family)

      (1L to 40L).foreach { seed =>
        val text     = paragraph(seed)
        val measured = measurer.advances(text, 0, text.length, 0, fonts)
        val shaped   = measurer.exactCaretXs(text, 0, fonts)
        val carets   = measured.caretsFrom(0, text.length)

        withClue(s"$family paragraph $seed: ") {
          (0 until text.length).foreach(index => math.abs(carets(index) - shaped(index)) should be <= 0.02f)
        }
      }
    }
  }
