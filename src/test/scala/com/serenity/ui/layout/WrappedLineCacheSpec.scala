package com.serenity.ui.layout

import java.awt.font.FontRenderContext
import java.awt.geom.AffineTransform
import java.awt.{Font, RenderingHints}

import com.serenity.richtext.{
  InlineMark,
  ParagraphRole,
  RichTextDocument,
  RichTextParagraph,
  RichTextRun,
  RichTextStyle
}
import com.serenity.rope.Balance
import com.serenity.state.models.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.TableDrivenPropertyChecks

class WrappedLineCacheSpec extends AnyFlatSpec with Matchers with TableDrivenPropertyChecks:

  given Balance = Balance.default

  private val sans  = Font(Font.SANS_SERIF, Font.PLAIN, 13)
  private val serif = Font(Font.SERIF, Font.PLAIN, 14)
  private val mono  = Font(Font.MONOSPACED, Font.PLAIN, 12)
  private val frc   = TextLayoutSnapshot.defaultFontRenderContext()
  private val prose =
    "The quick brown fox jumps over the lazy dog while the wrap width keeps folding this sentence onto new rows."

  private def wrap(
    text: String,
    cache: WrappedLineCache,
    widthPx: Int = 180,
    font: Font = sans,
    fontRenderContext: FontRenderContext = frc,
    bufferLine: Int = 0,
    maxVisualLines: Int = Int.MaxValue,
    cellMetrics: Option[CellMetrics] = None,
    forceCellLayout: Boolean = false
  ): Vector[TextVisualLine] =
    TextLayoutSnapshot.boundedVisualLinesForText(
      text,
      bufferLine,
      widthPx,
      font,
      fontRenderContext,
      maxVisualLines = maxVisualLines,
      cellMetricsOverride = cellMetrics,
      forceCellLayout = forceCellLayout,
      wrapCache = cache
    )

  private def richBuffer(paragraphs: List[RichTextParagraph]): Buffer =
    val document = RichTextDocument(paragraphs)
    Buffer
      .fromString(BufferId(7), document.plainText)
      .copy(
        richText = RichTextState(richTextDocument = Some(document)),
        viewport = Viewport(topLine = 0, leftColumn = 0, visibleColumns = 40, visibleLines = 40)
      )

  private def boldTail(text: String, boldFrom: Int, role: ParagraphRole = ParagraphRole.Body): RichTextParagraph =
    RichTextParagraph(
      List(
        RichTextRun(text.take(boldFrom)),
        RichTextRun(text.drop(boldFrom), RichTextStyle.empty.withMark(InlineMark.Bold))
      ),
      role = role
    )

  private def snapshot(buffer: Buffer, cache: WrappedLineCache, widthPx: Int = 200): TextLayoutSnapshot =
    TextLayoutSnapshot.fromBuffer(buffer, widthPx, serif, frc, wrapCache = cache)

  "WrappedLineCache" should "serve a plain line identical to an uncached wrap" in {
    val cache    = WrappedLineCache.bounded()
    val uncached = wrap(prose, WrappedLineCache.Uncached)
    wrap(prose, cache) shouldBe uncached
    wrap(prose, cache) shouldBe uncached
    cache.size shouldBe 1
    uncached.length should be > 2
  }

  it should "serve rich-text lines identical to an uncached snapshot" in {
    val buffer = richBuffer(
      List(
        boldTail(prose, 20),
        RichTextParagraph(List(RichTextRun(prose, RichTextStyle.empty.withFontSize(22.0f)))),
        boldTail(prose, 1, ParagraphRole.dropCap(lines = 2))
      )
    )
    val cache    = WrappedLineCache.bounded()
    val uncached = snapshot(buffer, WrappedLineCache.Uncached)
    snapshot(buffer, cache) shouldBe uncached
    snapshot(buffer, cache) shouldBe uncached
    cache.size shouldBe 3
  }

  it should "miss whenever any ingredient of the wrap changes" in {
    val cache = WrappedLineCache.bounded()
    val plainFractional = FontRenderContext(
      AffineTransform(),
      RenderingHints.VALUE_TEXT_ANTIALIAS_OFF,
      RenderingHints.VALUE_FRACTIONALMETRICS_ON
    )
    val noFractional = FontRenderContext(
      AffineTransform(),
      RenderingHints.VALUE_TEXT_ANTIALIAS_ON,
      RenderingHints.VALUE_FRACTIONALMETRICS_OFF
    )
    val variants: List[() => Vector[TextVisualLine]] = List(
      () => wrap(prose, cache),
      () => wrap(prose + "!", cache),
      () => wrap(prose, cache, widthPx = 181),
      () => wrap(prose, cache, font = serif),
      () => wrap(prose, cache, font = sans.deriveFont(14.0f)),
      () => wrap(prose, cache, font = sans.deriveFont(Font.BOLD)),
      () => wrap(prose, cache, fontRenderContext = plainFractional),
      () => wrap(prose, cache, fontRenderContext = noFractional),
      () => wrap(prose, cache, font = mono, forceCellLayout = true),
      () => wrap(prose, cache, font = mono, cellMetrics = Some(CellMetrics(1, 1, 0)), forceCellLayout = true),
      () => wrap(prose, cache, font = mono, cellMetrics = Some(CellMetrics(1, 1, 0, true)), forceCellLayout = true)
    )
    variants.zipWithIndex.foreach { (variant, index) =>
      variant()
      withClue(s"variant $index should have missed: ")(cache.size shouldBe index + 1)
    }

    val richVariants = List(
      richBuffer(List(RichTextParagraph.plain(prose))),
      richBuffer(List(boldTail(prose, 30))),
      richBuffer(List(boldTail(prose, 31))),
      richBuffer(List(RichTextParagraph(List(RichTextRun(prose, RichTextStyle.empty.withMark(InlineMark.Italic)))))),
      richBuffer(List(boldTail(prose, 30, ParagraphRole.dropCap(lines = 2)))),
      richBuffer(List(boldTail(prose, 30, ParagraphRole.dropCap(lines = 3))))
    )
    val before = cache.size
    richVariants.zipWithIndex.foreach { (buffer, index) =>
      snapshot(buffer, cache) shouldBe snapshot(buffer, WrappedLineCache.Uncached)
      withClue(s"rich variant $index should have missed: ")(cache.size shouldBe before + index + 1)
    }
  }

  it should "evict the least recently used lines beyond its line bound" in {
    val cache = WrappedLineCache.bounded(maxLines = 3)
    (0 until 10).foreach(index => wrap(s"line $index $prose", cache))
    cache.size shouldBe 3
    wrap("line 7 " + prose, cache)
    cache.size shouldBe 3
    wrap("line 9 " + prose, cache) shouldBe wrap("line 9 " + prose, WrappedLineCache.Uncached)
  }

  it should "evict by retained characters as well as by line count" in {
    val cache = WrappedLineCache.bounded(maxChars = prose.length.toLong * 2 + 10)
    (0 until 6).foreach(index => wrap(s"$index$prose", cache))
    cache.size shouldBe 2
    cache.retainedChars should be <= (prose.length.toLong * 2 + 10)
  }

  it should "rebase cached rows onto a line's new buffer line after a line is inserted above it" in {
    val text   = s"$prose\n\n$prose tail"
    val buffer = Buffer.fromString(BufferId(3), text).copy(viewport = Viewport(0, 0, 40, 40))
    val cache  = WrappedLineCache.bounded()
    snapshot(buffer, cache)
    val populated = cache.size

    val inserted = Buffer.fromString(BufferId(3), "inserted\n" + text).copy(viewport = Viewport(0, 0, 40, 40))
    val cached   = snapshot(inserted, cache)
    cached shouldBe snapshot(inserted, WrappedLineCache.Uncached)
    cached.visualLines.map(_.bufferLine).distinct shouldBe Vector(0, 1, 2, 3)
    cache.size shouldBe populated + 1
  }

  it should "answer a longer request after caching a row-limited wrap, and a shorter one after a full wrap" in {
    val cache    = WrappedLineCache.bounded()
    val full     = wrap(prose, WrappedLineCache.Uncached)
    val firstTwo = wrap(prose, WrappedLineCache.Uncached, maxVisualLines = 2)
    wrap(prose, cache, maxVisualLines = 2) shouldBe firstTwo
    wrap(prose, cache) shouldBe full
    wrap(prose, cache, maxVisualLines = 2) shouldBe firstTwo
    wrap(prose, cache, bufferLine = 5, maxVisualLines = 1) shouldBe full.take(1).map(_.copy(bufferLine = 5))
  }

  private val variedLines = Table(
    "line",
    "",
    "a",
    prose,
    "Supercalifragilisticexpialidocious" * 6,
    "tab\tseparated\tcolumns\tthat\twrap\tacross\tthe\tpanel\twidth\tseveral\ttimes",
    "été café naı̈ve résumé " * 4,
    "Family 👨‍👩‍👧‍👦 flags 🇬🇧🇯🇵 thumbs 👍🏽 and more emoji 🎉🎉🎉 to wrap " * 3,
    "日本語のテキストは空白なしで折り返されるので、行の途中で改行できる必要があります。" * 2,
    "中文混合 English words 和标点符号，测试换行。" * 3,
    "non breaking spaces hold these words together " * 3,
    "مرحبا بالعالم هذا نص عربي طويل يلتف عبر عدة أسطر " * 2
  )

  private val layouts = Table(
    ("font", "widthPx", "cellMetrics", "forceCell"),
    (sans, 160, None, false),
    (serif, 97, None, false),
    (mono, 240, None, false),
    (mono, 30, Some(CellMetrics(1, 1, 0)), true),
    (mono, 25, Some(CellMetrics(1, 1, 0, true)), true)
  )

  it should "wrap varied lines identically with and without the cache" in {
    val cache = WrappedLineCache.bounded()
    forAll(layouts) { (font, widthPx, cellMetrics, forceCell) =>
      forAll(variedLines) { line =>
        def run(c: WrappedLineCache, limit: Int) =
          wrap(line, c, widthPx, font, cellMetrics = cellMetrics, forceCellLayout = forceCell, maxVisualLines = limit)
        val expected = run(WrappedLineCache.Uncached, Int.MaxValue)
        run(cache, 2) shouldBe expected.take(2)
        run(cache, Int.MaxValue) shouldBe expected
        run(cache, Int.MaxValue) shouldBe expected
      }
    }
  }

  it should "lay out varied rich-text paragraphs identically with and without the cache" in {
    val paragraphs = variedLines.toList.filter(_.nonEmpty).zipWithIndex.map { (line, index) =>
      if index % 3 == 0 then RichTextParagraph.plain(line)
      else if index % 3 == 1 then boldTail(line, line.length / 2)
      else RichTextParagraph(List(RichTextRun(line, RichTextStyle.empty.withFontSize(18.0f))))
    }
    val buffer   = richBuffer(paragraphs).copy(viewport = Viewport(0, 0, 40, 400))
    val cache    = WrappedLineCache.bounded()
    val expected = snapshot(buffer, WrappedLineCache.Uncached, widthPx = 150)
    snapshot(buffer, cache, widthPx = 150) shouldBe expected
    snapshot(buffer, cache, widthPx = 150) shouldBe expected
  }
