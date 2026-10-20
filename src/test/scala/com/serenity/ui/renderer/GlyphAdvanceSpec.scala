package com.serenity.ui.renderer

import java.awt.Font
import java.awt.font.FontRenderContext
import java.awt.geom.AffineTransform
import java.util.concurrent.{Callable, Executors}

import com.serenity.ui.layout.BundledInterFont
import org.scalatest.OptionValues
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** A glyph's advance is read without asking AWT for the glyph's metrics: `GlyphVector.getGlyphMetrics` also measures
  * the glyph's outline, which costs a native call per glyph on macOS and made the column-paging specs run past forty
  * minutes there. What is read must be what the metrics report, on every face and at every scale.
  */
class GlyphAdvanceSpec extends AnyFlatSpec with Matchers with OptionValues:

  private val sample =
    "Hello, wide world: AVAWAY To WWWW Ty Yo iiii 0123456789 {}[]()<>/\\|~`!@#$%^&*-_=+;:'\",.? " +
      "éèüñç ÅØ ß fi fl ffi —–… αβγ " +
      "Жд 中文 ́ \t"

  private val renderContexts = List(
    "1x"          -> FontRenderContext(null, true, true),
    "2x"          -> FontRenderContext(AffineTransform.getScaleInstance(2.0, 2.0), true, true),
    "whole-pixel" -> FontRenderContext(null, false, false),
    "1.5x"        -> FontRenderContext(AffineTransform.getScaleInstance(1.5, 1.5), true, true)
  )

  private val faces = List(
    "platform serif" -> Font(Font.SERIF, Font.PLAIN, 14),
    "large serif"    -> Font(Font.SERIF, Font.PLAIN, 37),
    "bold sans"      -> Font(Font.SANS_SERIF, Font.BOLD, 13),
    "italic"         -> Font(Font.SERIF, Font.ITALIC, 14),
    "monospaced"     -> Font(Font.MONOSPACED, Font.PLAIN, 12),
    "bundled Inter"  -> BundledInterFont.plain(14f)
  )

  for
    (faceName, face)   <- faces
    (contextName, frc) <- renderContexts
  do
    s"The advance of a glyph of $faceName at $contextName" should "equal the advance its glyph metrics report" in {
      val nominal = face.createGlyphVector(frc, sample)
      (0 until nominal.getNumGlyphs).foreach { glyph =>
        val code = nominal.getGlyphCode(glyph)
        withClue(s"glyph $glyph (code $code): ")(
          GlyphAdvance().of(face, frc, code) shouldBe nominal.getGlyphMetrics(glyph).getAdvance
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

  "A stretch shaped through a cache" should "measure a glyph once, however many stretches contain it" in {
    val frc   = FontRenderContext(null, true, true)
    val font  = Font(Font.SERIF, Font.PLAIN, 14)
    val cache = ShapedTextCache()
    val words = List("alpha beta", "beta gamma", "gamma alpha")
    val measured = words.map { word =>
      val stretch = cache.stretchOf(word, font, frc)
      (0 until stretch.glyphCount).foreach(stretch.advanceAt)
      cache.glyphAdvances.measurements
    }
    val distinct = words.mkString.distinct.length
    measured.last should be <= distinct.toLong
    cache.stretchOf("alpha beta", font, frc)
    cache.glyphAdvances.measurements shouldBe measured.last
  }

  "A glyph advance cache" should "measure a glyph once and forget nothing below its bound" in {
    val frc     = FontRenderContext(null, true, true)
    val font    = Font(Font.SERIF, Font.PLAIN, 14)
    val advance = GlyphAdvance(maxTables = 8)
    val code    = font.createGlyphVector(frc, "a").getGlyphCode(0)
    val first   = advance.of(font, frc, code)
    advance.of(font, frc, code) shouldBe first
    advance.measurements shouldBe 1L
  }

  "A table of one font's advances" should "answer each glyph as the advance cache does and measure it once" in {
    val frc     = FontRenderContext(null, true, true)
    val font    = Font(Font.SERIF, Font.PLAIN, 14)
    val advance = GlyphAdvance()
    val table   = advance.table(font, frc)
    val codes   = font.createGlyphVector(frc, sample).getGlyphCodes(0, sample.length, null).toVector
    codes.foreach(code => table.of(code) shouldBe GlyphAdvance().of(font, frc, code))
    val measured = advance.measurements
    codes.foreach(table.of)
    advance.measurements shouldBe measured
    advance.table(font, frc) should be theSameInstanceAs table
  }

  it should "fill the advances of a run of glyph codes in order" in {
    val frc   = FontRenderContext(null, true, true)
    val font  = Font(Font.SERIF, Font.PLAIN, 14)
    val table = GlyphAdvance().table(font, frc)
    val codes = font.createGlyphVector(frc, sample).getGlyphCodes(0, sample.length, null)
    table.advancesOf(codes).toVector shouldBe codes.toVector.map(table.of)
  }

  for (faceName, face) <- faces do
    it should s"read glyph codes far above 255 as $faceName's glyph metrics report them" in {
      val frc    = FontRenderContext(null, true, true)
      val count  = face.getNumGlyphs
      val codes  = (List(256, 1000, 4096, 40000, 65534) ++ (count - 40 until count)).filter(_ < count).distinct.toArray
      val table  = GlyphAdvance().table(face, frc)
      val glyphs = face.createGlyphVector(frc, codes)
      codes.indices.foreach { index =>
        withClue(s"glyph code ${codes(index)}: ")(
          table.of(codes(index)) shouldBe glyphs.getGlyphMetrics(index).getAdvance
        )
      }
    }

  it should "answer a code no glyph carries the way the advance cache does" in {
    val frc   = FontRenderContext(null, true, true)
    val font  = Font(Font.SERIF, Font.PLAIN, 14)
    val table = GlyphAdvance().table(font, frc)
    val alone = GlyphAdvance()
    List(0xffff, 0x7fffffff, Int.MinValue, -1, 0x01000000 | 5).foreach { code =>
      withClue(s"glyph code $code: ")(table.of(code) shouldBe alone.of(font, frc, code))
      table.of(code) shouldBe table.of(code)
    }
  }

  it should "keep each glyph's own advance where many glyphs share a slot" in {
    val frc   = FontRenderContext(null, true, true)
    val font  = Font(Font.SERIF, Font.PLAIN, 14)
    val table = GlyphAdvance().table(font, frc)
    val alone = GlyphAdvance()
    val codes = (0 until 3000).toVector
    val first = codes.map(table.of)
    codes.map(table.of) shouldBe first
    codes.foreach(code => first(code) shouldBe alone.of(font, frc, code))
  }

  it should "give every thread the advances it would measure alone" in {
    val frc      = FontRenderContext(null, true, true)
    val font     = Font(Font.SERIF, Font.PLAIN, 14)
    val shared   = GlyphAdvance().table(font, frc)
    val alone    = GlyphAdvance()
    val codes    = (0 until 600).toVector
    val expected = codes.map(alone.of(font, frc, _))
    val pool     = Executors.newFixedThreadPool(8)
    val answers =
      try
        val pending = (0 until 8).toVector.map { _ =>
          pool.submit(
            new Callable[Vector[Float]]:
              def call(): Vector[Float] = codes.reverse.map(shared.of).reverse
          )
        }
        pending.map(_.get())
      finally pool.shutdown()
    answers.foreach(_ shouldBe expected)
  }
