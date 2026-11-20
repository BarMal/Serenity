package com.serenity.ui.layout

import java.awt.Font

import com.serenity.testkit.TestMeasure
import com.serenity.ui.layout.TextCaretMeasurement.singleFontResolver
import com.serenity.ui.renderer.FontSpec
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** An italic the font family has no face for is drawn by Skia with a skew, and its advances are the upright ones: the
  * slant overhangs the glyph box, not the advance. So layout measures such a font as it draws it, row by row and caret
  * by caret, and it is as measurable as the upright face.
  */
class SlantedFontMeasurementSpec extends AnyFlatSpec with Matchers with SkiaRowOracle:

  // The bundled Inter subset, which no machine needs to have installed and which has no italic of its own.
  private val upright = BundledInterFont.plain(13f)
  private val slanted = upright.deriveFont(Font.ITALIC)

  private val prose =
    "The quick brown fox jumps over the lazy dog, again and again, until the row finally has to wrap somewhere."

  "An italic font" should "be drawn with a skew" in {
    TestMeasure.faces.faceFor(FontSpec.fromAwt(slanted)).font.getSkewX should be < 0.0f
    TestMeasure.faces.faceFor(FontSpec.fromAwt(upright)).font.getSkewX shouldBe 0.0f
  }

  it should "be as measurable as the upright face: the skew adds no context to an advance" in {
    def advancesIn(font: Font) =
      GlyphAdvances.measure(
        prose,
        0,
        prose.length,
        0,
        singleFontResolver(FontSpec.fromAwt(font)),
        TestMeasure.measurer
      )
    val (plain, skewed) = (advancesIn(upright), advancesIn(slanted))
    skewed.isMeasurable(0, prose.length) shouldBe true
    prose.indices.foreach(index =>
      withClue(s"at $index: ")(skewed.isContextFree(index, index + 1) shouldBe plain.isContextFree(index, index + 1))
    )
  }

  it should "cut cleanly at a space, as the upright face does" in {
    ShapingBarriers.cutAtOrAfter("a T", 2, FontSpec.fromAwt(slanted), TestMeasure.measurer) shouldBe 2
  }

  it should "wrap into rows as wide as they are drawn" in
    Seq(40, 97, 160, 233, 480).foreach { widthPx =>
      val rows =
        TextLayoutSnapshot.boundedVisualLinesForText(prose, 3, widthPx, slanted, wrapCache = TestMeasure.wrapCache())
      assertSkiaRows(rows, prose, widthPx, slanted)
    }

  it should "measure caret positions where the glyphs are drawn" in {
    val carets = TextLayoutSnapshot.caretXsForText(prose, slanted, TestMeasure.layoutMemos())
    assertSkiaCarets(carets, prose, slanted)
  }
