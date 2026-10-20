package com.serenity.ui.renderer

import java.awt.Font
import java.awt.font.FontRenderContext
import java.awt.geom.AffineTransform

import com.serenity.ui.layout.BundledInterFont
import org.scalatest.OptionValues
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** A glyph's advance is read without asking AWT for the glyph's metrics: `GlyphVector.getGlyphMetrics` also measures the
  * glyph's outline, which costs a native call per glyph on macOS and made the column-paging specs run past forty
  * minutes there. What is read must be what the metrics report, on every face and at every scale.
  */
class GlyphAdvanceSpec extends AnyFlatSpec with Matchers with OptionValues:

  private val sample =
    "Hello, wide world: AVAWAY To WWWW Ty Yo iiii 0123456789 {}[]()<>/\\|~`!@#$%^&*-_=+;:'\",.? " +
      "éèüñç ÅØ ß fi fl ffi —–… αβγ " +
      "Жд 中文 ́ \t"

  private val renderContexts = List(
    "1x"         -> FontRenderContext(null, true, true),
    "2x"         -> FontRenderContext(AffineTransform.getScaleInstance(2.0, 2.0), true, true),
    "whole-pixel" -> FontRenderContext(null, false, false),
    "1.5x"       -> FontRenderContext(AffineTransform.getScaleInstance(1.5, 1.5), true, true)
  )

  private val faces = List(
    "platform serif"  -> Font(Font.SERIF, Font.PLAIN, 14),
    "large serif"     -> Font(Font.SERIF, Font.PLAIN, 37),
    "bold sans"       -> Font(Font.SANS_SERIF, Font.BOLD, 13),
    "italic"          -> Font(Font.SERIF, Font.ITALIC, 14),
    "monospaced"      -> Font(Font.MONOSPACED, Font.PLAIN, 12),
    "bundled Inter"   -> BundledInterFont.plain(14f)
  )

  for
    (faceName, face)    <- faces
    (contextName, frc)  <- renderContexts
  do
    s"The advance of a glyph of $faceName at $contextName" should "equal the advance its glyph metrics report" in {
      val nominal = face.createGlyphVector(frc, sample)
      (0 until nominal.getNumGlyphs).foreach { glyph =>
        val code = nominal.getGlyphCode(glyph)
        withClue(s"glyph $glyph (code $code): ")(
          GlyphAdvance.of(face, frc, code) shouldBe nominal.getGlyphMetrics(glyph).getAdvance
        )
      }
    }

    s"A shaped stretch of $faceName at $contextName" should "report each glyph's advance as its glyph metrics do" in {
      val stretch = ShapedStretch.shape(sample, face, frc)
      val whole   = stretch.glyphsOf(0, sample.length).value
      (0 until stretch.glyphCount).foreach { glyph =>
        withClue(s"glyph $glyph: ")(stretch.advanceAt(glyph) shouldBe whole.getGlyphMetrics(glyph).getAdvance)
      }
    }
