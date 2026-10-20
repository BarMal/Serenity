package com.serenity.ui.layout

import java.awt.{Font, GraphicsEnvironment}
import java.nio.file.Paths

import com.serenity.state.models.{MeasuredRow, TextVisualLine}
import com.serenity.ui.fonts.{FontFallback, FontLoader}
import com.serenity.ui.renderer.ShapedTextCache
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** #1866: text the primary font has no glyph for is measured in a fallback font, so carets sit where the fallback
  * glyphs are drawn. The fallback is a test subset font (中 and 文 one em wide, ★ 0.8 em, ascent 1.4 em and descent 0.4
  * em), and the proportional prose font is a physical test font too: a logical font such as Serif already falls back on
  * its own, so it would never reach this chain, so nothing depends on what the machine's `Dialog` font reaches.
  */
class FontFallbackLayoutSpec extends AnyFlatSpec with Matchers:

  private val frc = TextLayoutSnapshot.defaultFontRenderContext()

  private def registered(resource: String): Font =
    val font = Font.createFont(Font.TRUETYPE_FONT, Paths.get(getClass.getResource(resource).toURI).toFile)
    val _    = GraphicsEnvironment.getLocalGraphicsEnvironment.registerFont(font)
    font

  private val subset = registered("/fonts/SerenityFallbackTest.ttf")

  private val fallback = FontFallback(Vector(subset.getFamily))

  private val sizePx = 14.0f

  private def bundled(resource: String): Font =
    Font
      .createFont(Font.TRUETYPE_FONT, Paths.get(getClass.getResource(resource).toURI).toFile)
      .deriveFont(Font.PLAIN, sizePx)

  private val codeFont  = bundled("/fonts/MonaspaceNeon-Regular.otf")
  private val proseFont = registered("/fonts/SerenityProseTest.ttf").deriveFont(Font.PLAIN, sizePx)

  private def context(chain: FontFallback = fallback) =
    RichTextContext(None, 1.0f, dropCapsEnabled = true, fallback = chain)

  private def rowOf(text: String, font: Font, chain: FontFallback = fallback): TextVisualLine =
    val rows = TextLayoutSnapshot.boundedVisualLinesForText(
      text,
      0,
      10_000,
      font,
      frc,
      forceCellLayout = false,
      richText = context(chain)
    )
    rows should have size 1
    rows.head

  private def advanceOf(text: String, font: Font): Float =
    rowOf(text, font).widthPx

  "A prose line with a codepoint its font lacks" should "put the next caret one fallback advance later" in {
    val row = rowOf("a中b", proseFont)
    row.row.caretXs(2) - row.row.caretXs(1) shouldBe sizePx +- 0.01f
  }

  it should "end the row where the fallback glyphs end" in {
    val row = rowOf("a中文b", proseFont)
    row.widthPx shouldBe (advanceOf("a", proseFont) + 2 * sizePx + advanceOf("b", proseFont)) +- 0.01f
  }

  it should "measure each fallback in its own advance" in {
    val row = rowOf("★中", proseFont)
    row.row.caretXs(1) shouldBe 0.8f * sizePx +- 0.01f
  }

  it should "grow the row to fit the taller fallback glyphs in a proportional font" in {
    FontLoader.isMonospacedFont(proseFont) shouldBe false
    val plain  = rowOf("ab", proseFont)
    val mixed  = rowOf("a中b", proseFont)
    val tallPx = math.ceil(1.8 * sizePx).toInt
    mixed.heightPx should be >= tallPx
    mixed.heightPx should be > plain.heightPx
    mixed.ascentPx should be >= math.ceil(1.4 * sizePx).toInt
  }

  it should "record the stretch in the fallback font on its row, relative to the row's start" in {
    val row = rowOf("a中文b", proseFont)
    row.fontRuns.toVector shouldBe Vector(1, 3, 1)
    row.fallbackFamilies.lift(0) shouldBe Some(subset.getFamily)
  }

  "A line the primary font covers" should "keep a resolver with no fallback runs" in {
    val inputs =
      LineLayout.lineLayoutInputs(
        codeFont,
        frc,
        measuredLayout = true,
        context(),
        0,
        5,
        "hello",
        ShapedTextCache(),
        LayoutMemos()
      )
    inputs.resolver.runs shouldBe empty
  }

  it should "record no font runs on its rows, sharing the one empty array" in {
    val runs = rowOf("abi ab", proseFont).fontRuns
    runs.length shouldBe 0
    System.identityHashCode(runs) shouldBe System.identityHashCode(MeasuredRow.NoFontRuns)
  }

  it should "measure exactly as it did without a fallback chain" in {
    val plain = rowOf("hello world", proseFont, FontFallback.Default)
    plain.row shouldBe TextLayoutSnapshot
      .boundedVisualLinesForText("hello world", 0, 10_000, proseFont, frc, richText = RichTextContext.plain)
      .head
      .row
  }

  "A code line (monospaced font)" should "snap a wide character to exactly two cells of the primary's float advance" in {
    val cell = advanceOf("a", codeFont)
    val row  = rowOf("a中b", codeFont)
    row.row.caretXs(1) shouldBe cell +- 0.001f
    row.row.caretXs(2) shouldBe 3 * cell +- 0.001f
    row.widthPx shouldBe 4 * cell +- 0.001f
  }

  it should "snap each character of a run of wide characters" in {
    val cell = advanceOf("a", codeFont)
    val row  = rowOf("中文a", codeFont)
    row.row.caretXs.toVector.map(_ / cell) shouldBe Vector(0.0f, 2.0f, 4.0f, 5.0f).map(identity)
  }

  it should "snap a narrow fallback symbol to exactly one cell" in {
    val cell = advanceOf("a", codeFont)
    val row  = rowOf("a★b", codeFont)
    row.row.caretXs(2) - row.row.caretXs(1) shouldBe cell +- 0.001f
    row.widthPx shouldBe 3 * cell +- 0.001f
  }

  it should "keep the columns after a narrow fallback symbol on the primary's grid" in {
    val cell = advanceOf("a", codeFont)
    val row  = rowOf("a★★b c", codeFont)
    row.row.caretXs.toVector.map(_ / cell) shouldBe Vector(0.0f, 1.0f, 2.0f, 3.0f, 4.0f, 5.0f, 6.0f)
  }

  it should "paint each narrow fallback symbol on its own, at its snapped caret" in {
    val row = rowOf("a★★b", codeFont)
    row.fontRuns.toVector shouldBe Vector(1, 2, 1, 2, 3, 1)
  }

  it should "lay out a line of fallback symbols on the cell grid when it holds no wide character" in {
    val inputs =
      LineLayout.lineLayoutInputs(codeFont, frc, true, context(), 0, 3, "a★b", ShapedTextCache(), LayoutMemos())
    inputs.resolver.wideCellPx should be > 0.0f
  }

  it should "leave a symbol the primary font covers on its own advance" in {
    val covered = rowOf("a→b", codeFont)
    covered.row.fontRuns.toVector shouldBe empty
    covered.row.caretXs(2) - covered.row.caretXs(1) shouldBe advanceOf("→", codeFont) +- 0.001f
  }

  it should "keep the spaces and punctuation around a wide character on the primary's grid" in {
    val cell = advanceOf("a", codeFont)
    val row  = rowOf("中 a", codeFont)
    row.row.caretXs.toVector.map(_ / cell) shouldBe Vector(0.0f, 2.0f, 3.0f, 4.0f).map(identity)
    row.row.fontRuns.toVector shouldBe Vector(0, 1, 1)
  }

  it should "keep a fixed line pitch however tall the fallback glyphs are" in {
    FontLoader.isMonospacedFont(codeFont) shouldBe true
    val plain = rowOf("ab", codeFont)
    val mixed = rowOf("a中b", codeFont)
    mixed.heightPx shouldBe plain.heightPx
    mixed.ascentPx shouldBe plain.ascentPx
  }

  it should "wrap on the snapped width" in {
    val cell = advanceOf("a", codeFont)
    val rows = TextLayoutSnapshot.boundedVisualLinesForText(
      "中文中文",
      0,
      math.round(5 * cell),
      codeFont,
      frc,
      richText = context()
    )
    rows.map(_.text) shouldBe Vector("中文", "中文")
  }

  "The fallback itemisation on the hot path" should "not be consulted for a line of plain ASCII" in {
    val cache = WrappedLineCache.bounded()
    val _ = LineLayout.lineLayoutInputs(
      codeFont,
      frc,
      true,
      context(),
      0,
      12,
      "hello, world",
      cache.shapedText,
      cache.layoutMemos,
      wrapCache = cache
    )
    cache.fallbackCoverageLookups shouldBe 0L
  }

  it should "be done once for a line that needs it, however often the line is laid out" in {
    val cache = WrappedLineCache.bounded()
    (1 to 5).foreach { _ =>
      val _ =
        LineLayout.lineLayoutInputs(
          codeFont,
          frc,
          true,
          context(),
          0,
          3,
          "a中b",
          cache.shapedText,
          cache.layoutMemos,
          wrapCache = cache
        )
    }
    cache.fallbackCoverageLookups shouldBe 1L
  }

  it should "scan a repeated plain ASCII line once, however often the line is laid out" in {
    val cache = WrappedLineCache.bounded()
    val text  = "hello, world " * 20
    (1 to 5).foreach { _ =>
      val _ = LineLayout.lineLayoutInputs(
        codeFont,
        frc,
        true,
        context(),
        0,
        text.length,
        text,
        cache.shapedText,
        cache.layoutMemos,
        wrapCache = cache
      )
    }
    cache.plainAsciiScans shouldBe 1L
  }
