package com.serenity.ui.layout

import java.awt.Font

import com.serenity.ui.fonts.FontLoader
import com.serenity.ui.renderer.ShapedTextCache
import org.scalatest.OptionValues
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** The per-font table of printable ASCII advances a row count may read in place of wrapping a line (#2447 B2). A font
  * has one only when nothing can set a character at an advance other than its own: no ligatures or kerning asked for,
  * no substitution in the font's own tables, and the layout's advance agreeing with the sum of the glyphs'.
  */
class NominalAdvancesSpec extends AnyFlatSpec with Matchers with OptionValues:

  private val frc       = TextLayoutSnapshot.defaultFontRenderContext()
  private val config    = FontLoader.FontConfig()
  private val printable = (0x20 to 0x7e).map(_.toChar)

  private def codeFont(sizePt: Float, ligatures: Boolean): Font =
    FontLoader.previewCodeFont(config.copy(fontSize = sizePt, enableLigatures = ligatures))

  private val sizes = List(11.0f, 12.0f, 13.0f, 14.0f, 15.0f, 16.0f, 18.0f)

  "A code font set without ligatures" should "have a table, at every size, including those the cell grid drifts from" in
    sizes.foreach { size =>
      withClue(s"$size pt: ")(NominalAdvances.of(codeFont(size, ligatures = false), frc) shouldBe defined)
    }

  it should "list, for each printable character, the advance the measured path gives it" in {
    val shapedText = ShapedTextCache()
    sizes.foreach { size =>
      val font     = codeFont(size, ligatures = false)
      val resolver = TextCaretMeasurement.singleFontResolver(font)
      val table    = NominalAdvances.of(font, frc).value
      printable.foreach { char =>
        val measured = GlyphAdvances.measure(char.toString, 0, 1, 0, resolver, frc, shapedText).caretsFrom(0, 1)(1)
        withClue(s"$size pt, '$char': ")(table.advanceOf(char) shouldBe measured)
      }
    }
  }

  "A code font set with ligatures" should "have no table, since shaping may change what a character advances" in
    sizes.foreach { size =>
      withClue(s"$size pt: ")(NominalAdvances.of(codeFont(size, ligatures = true), frc) shouldBe None)
    }

  "The proportional text font" should "have no table" in {
    NominalAdvances.of(FontLoader.previewTextFont(config), frc) shouldBe None
  }

  "A line of printable ASCII" should "fit one row when its summed advance plus the wrap's margin is under the width" in {
    val font  = codeFont(12.0f, ligatures = false)
    val table = NominalAdvances.of(font, frc).value
    val width = table.advanceOf('a') * 10

    table.fitsOneRow("a" * 9, math.round(width)) shouldBe true
    table.fitsOneRow("a" * 10, math.round(width) + 1) shouldBe true
    table.fitsOneRow("a" * 11, math.round(width)) shouldBe false
  }

  it should "never be called one row when it holds a character outside printable ASCII" in {
    val table = NominalAdvances.of(codeFont(12.0f, ligatures = false), frc).value

    List("\tx", "é", "日本語", "á", "x​y").foreach { text =>
      withClue(s"'$text': ")(table.fitsOneRow(text, 100_000) shouldBe false)
    }
  }

  "An empty line" should "fit one row" in {
    NominalAdvances.of(codeFont(12.0f, ligatures = false), frc).value.fitsOneRow("", 1) shouldBe true
  }
