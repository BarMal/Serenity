package com.serenity.ui.layout

import java.awt.Font
import java.awt.font.{FontRenderContext, TextAttribute}
import java.awt.geom.AffineTransform
import java.util.concurrent.atomic.AtomicInteger

import com.serenity.ui.layout.TextCaretMeasurement.{ColumnFontRun, LineFontResolver, singleFontResolver}
import org.scalatest.Assertion
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Glyph advances read from a long-lived table must equal, bit for bit, what one glyph vector per paragraph measured
  * ([[LegacyGlyphAdvances]]), and a repeated measurement of plain text must not ask AWT for a glyph vector again.
  */
class GlyphAdvancesEquivalenceSpec extends AnyFlatSpec with Matchers:

  private def frcOf(antialias: Boolean, fractional: Boolean, scale: Double = 1.0): FontRenderContext =
    FontRenderContext(AffineTransform.getScaleInstance(scale, scale), antialias, fractional)

  private val frcs = Vector(
    frcOf(antialias = true, fractional = true),
    frcOf(antialias = true, fractional = false),
    frcOf(antialias = false, fractional = true),
    frcOf(antialias = false, fractional = false),
    frcOf(antialias = true, fractional = true, scale = 2.0),
    frcOf(antialias = true, fractional = false, scale = 1.25)
  )

  private def withAttribute(font: Font, attribute: TextAttribute, value: AnyRef): Font =
    font.deriveFont(java.util.Map.of(attribute, value))

  private val fonts = Vector(
    Font(Font.MONOSPACED, Font.PLAIN, 13),
    Font(Font.SERIF, Font.PLAIN, 14),
    Font(Font.SANS_SERIF, Font.BOLD, 12),
    Font(Font.SERIF, Font.ITALIC, 17),
    Font(Font.DIALOG, Font.PLAIN, 1).deriveFont(14.95f),
    withAttribute(Font(Font.SANS_SERIF, Font.PLAIN, 12), TextAttribute.LIGATURES, TextAttribute.LIGATURES_ON),
    withAttribute(Font(Font.SERIF, Font.PLAIN, 15), TextAttribute.KERNING, TextAttribute.KERNING_ON)
  )

  private val fragments = Vector(
    "a",
    "e",
    "Zq",
    "the",
    "quick",
    "fi",
    "fl",
    "ffi",
    "WAVE",
    "AV",
    "To",
    "0123456789",
    ".,;:!?'\"()[]{}<>-_=+*/\\|@#$%^&~`",
    " ",
    "  ",
    "\t",
    " ",
    "éèüñßåø",
    "Αβγω",
    "Ждя",
    "中文字",
    "あいう",
    "가나",
    "—–…€™",
    "é",
    "ạ̈",
    "​",
    "‌",
    "‍",
    "﻿",
    "­",
    "️",
    "\u0000",
    "\u0007",
    "\u001b",
    "\u007f",
    "\u0085",
    "אב",
    "مرحبا",
    "กา",
    "क्ष",
    "👍",
    "👍🏽",
    "👩‍💻",
    "𝐀",
    0xd800.toChar.toString,
    0xdc00.toChar.toString,
    "🇺🇸"
  )

  private val plainFragments = fragments.take(14)

  private def randomText(random: java.util.Random, pool: Vector[String], pieces: Int): String =
    Vector.fill(pieces)(pool(random.nextInt(pool.size))).mkString

  private def bits(advance: Float): Int = java.lang.Float.floatToRawIntBits(advance)

  private def assertSameAsLegacy(
    text: String,
    from: Int,
    until: Int,
    absoluteStartColumn: Int,
    resolver: LineFontResolver,
    frc: FontRenderContext
  ): Assertion =
    val expected = LegacyGlyphAdvances.measure(text, from, until, absoluteStartColumn, resolver, frc)
    val actual   = GlyphAdvances.measure(text, from, until, absoluteStartColumn, resolver, frc)
    val count    = until - from
    val actualAdvanceBits =
      Vector.tabulate(count)(index => bits(actual.caretsFrom(index, 1)(1)))
    val expectedAdvanceBits = expected.advances.toVector.map(advance => bits(0.0f + advance))
    actualAdvanceBits shouldBe expectedAdvanceBits
    Vector.tabulate(count)(index => actual.isContextFree(index, index + 1)) shouldBe expected.contextFree.toVector

  private def assertSameAsLegacy(text: String, font: Font, frc: FontRenderContext): Assertion =
    assertSameAsLegacy(text, 0, text.length, 0, singleFontResolver(font), frc)

  "Glyph advances" should "equal the glyph-vector measurement for every category of text" in:
    for
      font     <- fonts
      frc      <- frcs
      fragment <- fragments
      framed   <- Vector(fragment, s"plain $fragment plain", s"$fragment$fragment")
    do assertSameAsLegacy(framed, font, frc)

  it should "equal the glyph-vector measurement for random mixed text" in:
    val random = java.util.Random(1798L)
    for
      font <- fonts
      frc  <- frcs
      _    <- 0 until 40
    do assertSameAsLegacy(randomText(random, fragments, 1 + random.nextInt(25)), font, frc)

  it should "equal the glyph-vector measurement for random plain text, the table's own territory" in:
    val random = java.util.Random(1812L)
    for
      font <- fonts
      frc  <- frcs
      _    <- 0 until 40
    do assertSameAsLegacy(randomText(random, plainFragments, 1 + random.nextInt(60)), font, frc)

  it should "cover every code unit of the basic multilingual plane exactly as the glyph vector does" in:
    val everyUnit = (0 until 0x10000).map(_.toChar).grouped(97).map(_.mkString).toVector
    for
      font <- Vector(fonts(0), fonts(1), fonts(5))
      frc  <- Vector(frcs(0), frcs(3))
      page <- everyUnit
    do assertSameAsLegacy(page, font, frc)

  it should "measure every code unit of the basic multilingual plane alone and between plain letters" in:
    for
      font <- Vector(fonts(0), fonts(1))
      frc  <- Vector(frcs(0), frcs(3))
      unit <- (0 until 0x10000).map(_.toChar)
    do
      assertSameAsLegacy(unit.toString, font, frc)
      assertSameAsLegacy(s"ab$unit ba", font, frc)

  it should "measure an empty span" in:
    fonts.foreach(font => assertSameAsLegacy("", font, frcs.head))
    assertSameAsLegacy("abc", 3, 3, 0, singleFontResolver(fonts.head), frcs.head)

  it should "measure windows of a longer paragraph with an absolute start column" in:
    val random = java.util.Random(7L)
    val text   = randomText(random, plainFragments ++ fragments.drop(14).take(10), 400)
    for
      font <- fonts
      frc  <- frcs.take(3)
    do
      assertSameAsLegacy(text, 0, text.length, 0, singleFontResolver(font), frc)
      assertSameAsLegacy(text, 5, math.min(text.length, 5 + 16384), 12, singleFontResolver(font), frc)

  it should "measure a paragraph longer than the 16384-character window" in:
    val text = "The quick brown fox jumps over the lazy dog, 0123456789. " * 400
    text.length should be > 16384
    for
      font <- fonts.take(3)
      frc  <- frcs.take(2)
    do assertSameAsLegacy(text, 0, text.length, 0, singleFontResolver(font), frc)

  it should "measure rich lines whose columns resolve to different fonts" in:
    val random = java.util.Random(99L)
    for
      frc <- frcs.take(3)
      _   <- 0 until 20
    do
      val text = randomText(random, plainFragments ++ fragments.slice(14, 24), 30)
      val resolver = LineFontResolver(
        fonts(0),
        Vector(
          ColumnFontRun(2, text.length / 2, fonts(2)),
          ColumnFontRun(text.length / 2, text.length, fonts(5))
        )
      )
      assertSameAsLegacy(text, 0, text.length, 0, resolver, frc)

  private val countedFrc = frcOf(antialias = true, fractional = true, scale = 1.37)

  final private class CountingFont(
      attributes: java.util.Map[TextAttribute, AnyRef],
      val calls: AtomicInteger,
      val shapings: AtomicInteger
  ) extends Font(attributes):

    override def createGlyphVector(frc: FontRenderContext, chars: Array[Char]) =
      calls.incrementAndGet()
      super.createGlyphVector(frc, chars)

    override def layoutGlyphVector(frc: FontRenderContext, text: Array[Char], start: Int, limit: Int, flags: Int) =
      shapings.incrementAndGet()
      super.layoutGlyphVector(frc, text, start, limit, flags)

  private def countingFont(size: Float, extra: (TextAttribute, AnyRef)*): CountingFont =
    val attributes = new java.util.HashMap[TextAttribute, AnyRef]()
    attributes.put(TextAttribute.FAMILY, Font.SANS_SERIF)
    attributes.put(TextAttribute.SIZE, Float.box(size))
    extra.foreach((key, value) => attributes.put(key, value))
    CountingFont(attributes, AtomicInteger(), AtomicInteger())

  it should "not ask AWT for a glyph vector when plain text is measured again" in:
    val font = countingFont(21.5f)
    val text = "Typing into an ordinary paragraph, 0123456789 times over."
    GlyphAdvances.measure(text, 0, text.length, 0, singleFontResolver(font), countedFrc)
    val afterFirst = font.calls.get
    GlyphAdvances.measure(text + " more", 0, text.length + 5, 0, singleFontResolver(font), countedFrc)
    GlyphAdvances.measure(text.reverse, 0, text.length, 0, singleFontResolver(font), countedFrc)
    font.calls.get shouldBe afterFirst

  it should "keep measuring a run holding a surrogate pair with a glyph vector each time" in:
    val font = countingFont(22.5f)
    val text = "emoji 👍 inside a run"
    GlyphAdvances.measure(text, 0, text.length, 0, singleFontResolver(font), countedFrc)
    val afterFirst = font.calls.get
    GlyphAdvances.measure(text, 0, text.length, 0, singleFontResolver(font), countedFrc)
    font.calls.get should be > afterFirst
    assertSameAsLegacy(text, font, countedFrc)

  it should "keep measuring a run holding a character that needs layout with a glyph vector each time" in:
    val font = countingFont(23.5f)
    val text = "combining é accent and ​ zero width"
    GlyphAdvances.measure(text, 0, text.length, 0, singleFontResolver(font), countedFrc)
    val afterFirst = font.calls.get
    GlyphAdvances.measure(text, 0, text.length, 0, singleFontResolver(font), countedFrc)
    font.calls.get should be > afterFirst
    assertSameAsLegacy(text, font, countedFrc)

  it should "still shape a font with layout attributes each time, but measure its glyphs only once" in:
    val font = countingFont(24.5f, TextAttribute.LIGATURES -> TextAttribute.LIGATURES_ON)
    val text = "plain text with fi and fl"
    GlyphAdvances.measure(text, 0, text.length, 0, singleFontResolver(font), countedFrc)
    val glyphVectorsAfterFirst = font.calls.get
    val shapingsAfterFirst     = font.shapings.get
    GlyphAdvances.measure(text, 0, text.length, 0, singleFontResolver(font), countedFrc)
    font.calls.get shouldBe glyphVectorsAfterFirst
    font.shapings.get should be > shapingsAfterFirst
    assertSameAsLegacy(text, font, countedFrc)

  "The glyph advance table" should "hold a plain character's advance and leave the rest to the run" in:
    val font     = fonts(1)
    val frc      = frcs(1)
    val table    = GlyphAdvanceTable(font, frc)
    val expected = font.createGlyphVector(frc, Array('W')).getGlyphMetrics(0).getAdvance
    val code     = font.createGlyphVector(frc, Array('W')).getGlyphCode(0)
    Vector.fill(2)(table.advanceCell('W')).foreach { cell =>
      bits(GlyphAdvanceTable.advanceOf(cell)) shouldBe bits(expected)
      table.glyphCode('W') shouldBe code
    }
    Vector('\u200b', '\n', 0xd83d.toChar, 0xdc4d.toChar, '\u0301', '\u05d0').foreach { unsuited =>
      GlyphAdvanceTable.isUnsuited(table.advanceCell(unsuited)) shouldBe true
    }

  "The glyph advance table cache" should "serve equal fonts built apart from one table" in:
    val cache = GlyphAdvanceTableCache(4)
    val first = cache.forFont(Font(Font.SERIF, Font.PLAIN, 14), frcs(0))
    cache.forFont(Font(Font.SANS_SERIF, Font.PLAIN, 14), frcs(0)) should not be theSameInstanceAs(first)
    cache.forFont(
      Font(Font.SERIF, Font.PLAIN, 14),
      frcOf(antialias = true, fractional = true)
    ) should be theSameInstanceAs first
    cache.size shouldBe 2

  it should "stay within its cap and keep measuring correctly once the cap is reached" in:
    val cache = GlyphAdvanceTableCache(3)
    val text  = "Bounded tables, still exact: 0123456789"
    (10 until 30).foreach { size =>
      val font  = Font(Font.SERIF, Font.PLAIN, size)
      val table = cache.forFont(font, frcs(0))
      cache.size should be <= 3
      text.foreach { char =>
        val expected = font.createGlyphVector(frcs(0), Array(char)).getGlyphMetrics(0).getAdvance
        bits(GlyphAdvanceTable.advanceOf(table.advanceCell(char))) shouldBe bits(expected)
      }
    }

  it should "give identical advances to threads measuring at once" in:
    val frc     = frcOf(antialias = true, fractional = false, scale = 1.11)
    val random  = java.util.Random(5L)
    val texts   = Vector.fill(16)(randomText(random, plainFragments ++ fragments.slice(14, 24), 60))
    val font    = fonts(2)
    val results = java.util.concurrent.Executors.newFixedThreadPool(8)
    try
      val futures = (0 until 8).map { worker =>
        results.submit(
          new java.util.concurrent.Callable[Vector[Vector[Int]]]:
            def call(): Vector[Vector[Int]] =
              texts.drop(worker).concat(texts.take(worker)).map { text =>
                val measured = GlyphAdvances.measure(text, 0, text.length, 0, singleFontResolver(font), frc)
                Vector.tabulate(text.length)(index => bits(measured.caretsFrom(index, 1)(1)))
              }
        )
      }
      val expected = texts.map { text =>
        LegacyGlyphAdvances
          .measure(text, 0, text.length, 0, singleFontResolver(font), frc)
          .advances
          .toVector
          .map(advance => bits(0.0f + advance))
      }
      futures.zipWithIndex.foreach { (future, worker) =>
        future.get shouldBe expected.drop(worker).concat(expected.take(worker))
      }
    finally results.shutdown()
