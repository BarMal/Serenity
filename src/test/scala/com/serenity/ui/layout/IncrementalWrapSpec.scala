package com.serenity.ui.layout

import java.awt.RenderingHints
import java.awt.font.FontRenderContext
import java.awt.geom.AffineTransform

import scala.util.Random
import scala.util.chaining.*

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
import com.serenity.ui.layout.WrapFixtures.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Re-wrapping an edited line from an earlier wrap of it must give bitwise the rows a cold wrap of the new text gives:
  * every property compares against the cold wrap, byte for byte.
  */
class IncrementalWrapSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private def check(
    cache: WrappedLineCache,
    text: String,
    layout: Layout,
    widthPx: Int,
    bufferLine: Int = 0
  ): Unit =
    assertSameAsCold(wrap(text, cache, layout, widthPx, bufferLine), text, layout, widthPx, bufferLine)

  private def incrementalWraps(cache: WrappedLineCache.Bounded): Long = cache.wrapStats.incrementalWraps

  private def rowsFor(rnd: Random): Int = if rnd.nextInt(3) == 0 then 1 + rnd.nextInt(60) else 1 + rnd.nextInt(8)

  private def exoticCase(rnd: Random): Boolean = rnd.nextInt(7) == 0

  "an incremental re-wrap" should "equal the cold wrap for random single edits" in {
    val rnd = Random(20260412L)
    val resumed = (0 until Cases).map { _ =>
      val layout  = layouts(rnd.nextInt(layouts.length))
      val width   = widthPx(rnd, layout)
      val exotic  = exoticCase(rnd)
      val before  = paragraphFor(rnd, layout, width, rowsFor(rnd), exotic)
      val cache   = WrappedLineCache.bounded()
      val rows    = wrap(before, cache, layout, width)
      val changed = edit(rnd, before, rows, exotic).applyTo(before)
      check(cache, changed, layout, width)
      if exotic then 0L else incrementalWraps(cache)
    }
    info(
      s"$Cases single edits compared with the cold wrap; ${resumed.sum} of the plain-prose ones resumed incrementally"
    )
    resumed.sum should be > (Cases * 8L / 10)
  }

  it should "equal the cold wrap along chains of edits, each applied to the previous incremental result" in {
    val rnd = Random(20260413L)
    @annotation.tailrec
    def chains(done: Int, resumed: Long, eligible: Int): (Long, Int) =
      if done >= Cases then (resumed, eligible)
      else
        val layout = layouts(rnd.nextInt(layouts.length))
        val width  = widthPx(rnd, layout)
        val exotic = exoticCase(rnd)
        val cache  = WrappedLineCache.bounded()
        val start  = paragraphFor(rnd, layout, width, rowsFor(rnd), exotic)
        wrap(start, cache, layout, width)
        val steps = 10 + rnd.nextInt(41)
        val _ = (0 until steps).foldLeft(start) { (text, _) =>
          val rows    = wrap(text, cache, layout, width)
          val changed = edit(rnd, text, rows, exotic).applyTo(text)
          check(cache, changed, layout, width)
          changed
        }
        chains(
          done + steps,
          resumed + (if exotic then 0L else incrementalWraps(cache)),
          eligible + (if exotic then 0 else steps)
        )
    val (resumed, eligible) = chains(0, 0L, 0)
    info(
      s"edits compared with the cold wrap along chains: at least $Cases; $resumed of $eligible plain-prose ones resumed"
    )
    resumed should be > (eligible * 8L / 10)
  }

  it should "equal the cold wrap for an insertion, a deletion and a replacement at every offset" in {
    val rnd    = Random(20260414L)
    val stride = if Cases >= 20000 then 1 else 11
    val configurations =
      Vector("sans", "default-prose", "liberation-serif-kerned", "mono-cells", "serif-kerned-ligatured")
        .flatMap(name => Vector(14, 40).map(columns => layouts.find(_.name == name).get -> columns))
    val compared = configurations.map { (layout, columns) =>
      val width  = columns * layout.charWidthPx + 3
      val before = paragraphFor(rnd, layout, width, 12)
      (0 to before.length by stride).map { offset =>
        val edits = Vector(
          Edit(offset, 0, "x"),
          Edit(offset, 0, " "),
          Edit(offset, 0, "AV To"),
          Edit(offset, 1.min(before.length - offset), ""),
          Edit(offset, 3.min(before.length - offset), ""),
          Edit(offset, 1.min(before.length - offset), " ")
        )
        edits.foreach { change =>
          val cache = WrappedLineCache.bounded()
          wrap(before, cache, layout, width)
          check(cache, change.applyTo(before), layout, width)
        }
        edits.length
      }.sum
    }.sum
    info(s"$compared sweep edits (insert, delete, replace at every ${stride}th offset) compared with the cold wrap")
  }

  it should "equal the cold wrap when a word moves up to the previous row" in {
    val layout = layouts.find(_.name == "default-prose").get
    val rnd    = Random(20260415L)
    val width  = 30 * layout.charWidthPx
    val before = paragraphFor(rnd, layout, width, 25)
    val rows   = wrap(before, WrappedLineCache.Uncached, layout, width)
    val movedUp = rows.drop(1).count { row =>
      val wordEnd   = before.indexWhere(_ == ' ', row.startColumn).pipe(end => if end < 0 then before.length else end)
      val shortened = Edit(row.startColumn, 1.max(wordEnd - row.startColumn - 1), "")
      val cache     = WrappedLineCache.bounded()
      wrap(before, cache, layout, width)
      val changed = shortened.applyTo(before)
      check(cache, changed, layout, width)
      incrementalWraps(cache) shouldBe 1L
      wrap(changed, WrappedLineCache.Uncached, layout, width).map(_.endColumn).take(rows.indexOf(row)) !=
        rows.map(_.endColumn).take(rows.indexOf(row))
    }
    withClue("the sweep must include edits that change a row above the edited one: ")(movedUp should be > 0)
  }

  it should "equal the cold wrap for edits at the first character of every row" in {
    val rnd = Random(20260416L)
    layouts.foreach { layout =>
      val width  = 18 * layout.charWidthPx + 1
      val before = paragraphFor(rnd, layout, width, 20)
      val rows   = wrap(before, WrappedLineCache.Uncached, layout, width)
      rows.foreach { row =>
        Vector(Edit(row.startColumn, 0, "z"), Edit(row.startColumn, 0, " "), Edit(row.startColumn, 1, "")).foreach {
          change =>
            val cache = WrappedLineCache.bounded()
            wrap(before, cache, layout, width)
            check(cache, change.applyTo(before), layout, width)
        }
      }
    }
  }

  it should "equal the cold wrap when spaces are inserted before and after a word that wraps, and when the space that made a break goes" in {
    val rnd = Random(20260417L)
    layouts.foreach { layout =>
      val width  = 20 * layout.charWidthPx + 2
      val before = paragraphFor(rnd, layout, width, 15)
      val rows   = wrap(before, WrappedLineCache.Uncached, layout, width)
      rows.drop(1).foreach { row =>
        val spaceBefore = before.lastIndexOf(' ', row.startColumn - 1)
        val wordEnd     = before.indexOf(' ', row.startColumn).pipe(end => if end < 0 then before.length else end)
        val edits = Vector(
          Edit(row.startColumn, 0, " "),
          Edit(wordEnd, 0, " "),
          Edit(wordEnd, 0, "  "),
          Edit(spaceBefore.max(0), if spaceBefore >= 0 then 1 else 0, ""),
          Edit(spaceBefore.max(0), if spaceBefore >= 0 then 1 else 0, "-")
        )
        edits.foreach { change =>
          val cache = WrappedLineCache.bounded()
          wrap(before, cache, layout, width)
          check(cache, change.applyTo(before), layout, width)
        }
      }
    }
  }

  it should "reuse the paragraph's rows rather than recompute them for a keystroke at the end" in {
    val layout = layouts.find(_.name == "default-prose").get
    val width  = 30 * layout.charWidthPx
    val before = paragraphFor(Random(11L), layout, width, 30)
    val cache  = WrappedLineCache.bounded()
    wrap(before, cache, layout, width)
    check(cache, before + "x", layout, width)
    incrementalWraps(cache) shouldBe 1L
  }

  it should "label reused rows with the line they are requested for" in {
    val layout = layouts.find(_.name == "serif").get
    val width  = 160
    val before = paragraphFor(Random(5L), layout, width, 20)
    val cache  = WrappedLineCache.bounded()
    wrap(before, cache, layout, width, bufferLine = 3)
    check(cache, before.patch(40, "ab", 0), layout, width, bufferLine = 9)
    incrementalWraps(cache) shouldBe 1L
    wrap(before.patch(40, "ab", 0), cache, layout, width, bufferLine = 9).map(_.bufferLine).distinct shouldBe Vector(9)
  }

  it should "equal the cold wrap for lines laid out from a later column" in {
    val layout = layouts.find(_.name == "sans").get
    val before = paragraphFor(Random(6L), layout, 150, 10)
    val cache  = WrappedLineCache.bounded()
    wrap(before, cache, layout, 150, baseColumn = 7)
    val after = before.patch(30, "q", 0)
    assertSameAsCold(wrap(after, cache, layout, 150, baseColumn = 7), after, layout, 150, baseColumn = 7)
    incrementalWraps(cache) shouldBe 1L
  }

  // -- fallbacks: the cold path is taken and the result is still the cold wrap -------------------------------------

  private def assertCold(layout: Layout, width: Int, before: String, after: String): Unit =
    val cache = WrappedLineCache.bounded()
    wrap(before, cache, layout, width)
    check(cache, after, layout, width)
    withClue(s"${after.take(60)}: ")(incrementalWraps(cache) shouldBe 0L)

  private val sans  = layouts.find(_.name == "sans").get
  private val prose = layouts.find(_.name == "default-prose").get
  private val text  = paragraphFor(Random(77L), sans, 160, 12)

  it should "fall back for text in a dictionary-segmented script" in {
    assertCold(sans, 160, text + " 日本語のテキスト", text + " 日本語のテキスト。")
    assertCold(sans, 160, text + " ภาษาไทยเป็นภาษา", text + " ภาษาไทยเป็นภาษาที่")
    assertCold(layouts.head, 120, text + " 中文混合", text + " 中文混合x")
  }

  it should "fall back for surrogates, format, control and complex-script text under a measured layout" in {
    assertCold(prose, 160, text + " 🎉", text + " 🎉🎉")
    assertCold(prose, 160, text + " 👨‍👩‍👧", text + " 👨‍👩‍👧x")
    assertCold(prose, 160, text + " zero​width", text + " zero​width!")
    assertCold(prose, 160, text + " soft­hyphen", text + " soft­hyphens")
    assertCold(prose, 160, text + " مرحبا بالعالم", text + " مرحبا بالعالم.")
    assertCold(prose, 160, text + " שלום עולם", text + " שלום עולם!")
    assertCold(prose, 160, text + " café", text + " cafés")
  }

  it should "resume text with emoji under the cell layout, which has no shaping to cut" in {
    val cells  = layouts.find(_.name == "display-width-cells").get
    val before = paragraphFor(Random(8L), cells, 30, 10) + " 🎉 👍🏽 end"
    val cache  = WrappedLineCache.bounded()
    wrap(before, cache, cells, 30)
    check(cache, before.patch(20, "ab", 0), cells, 30)
    incrementalWraps(cache) shouldBe 1L
  }

  it should "fall back for a paragraph longer than one measurement window" in {
    val long = paragraphFor(Random(9L), sans, 160, 12).pipe(unit => Iterator.continually(unit).take(120).mkString(" "))
    long.length should be > ParagraphMeasurement.MaxWindowChars
    assertCold(sans, 160, long, long.patch(50, "x", 0))
  }

  it should "fall back for an edit larger than the bound" in {
    val big = "y" * (IncrementalWrap.MaxEditChars + 1)
    assertCold(sans, 160, text, text.patch(30, big, 0))
    assertCold(sans, 160, text, text.patch(30, "", IncrementalWrap.MaxEditChars + 1))
  }

  it should "fall back for a row-limited request" in {
    val cache = WrappedLineCache.bounded()
    wrap(text, cache, sans, 160)
    val edited  = text.patch(30, "ab", 0)
    val limited = wrap(edited, cache, sans, 160, maxVisualLines = 3)
    limited shouldBe wrap(edited, WrappedLineCache.Uncached, sans, 160, maxVisualLines = 3)
    incrementalWraps(cache) shouldBe 0L
  }

  it should "never replace a complete entry with a partial one" in {
    val cache  = WrappedLineCache.bounded()
    val edited = text.patch(30, "ab", 0)
    wrap(text, cache, sans, 160)
    check(cache, edited, sans, 160)
    val coldWraps = cache.wrapStats.coldWraps
    val limited   = wrap(edited, cache, sans, 160, maxVisualLines = 3)
    limited.length shouldBe 3
    cache.wrapStats.coldWraps shouldBe coldWraps
    check(cache, edited, sans, 160)
    cache.wrapStats.coldWraps shouldBe coldWraps
  }

  it should "resume from a complete wrap and not from a row-limited one" in {
    val cache = WrappedLineCache.bounded()
    wrap(text, cache, sans, 160, maxVisualLines = 3)
    check(cache, text.patch(30, "ab", 0), sans, 160)
    incrementalWraps(cache) shouldBe 0L
  }

  it should "fall back for rich text and drop caps" in {
    val serif = layouts.find(_.name == "serif").get
    def buffer(paragraph: RichTextParagraph): Buffer =
      val document = RichTextDocument(List(paragraph))
      Buffer
        .fromString(BufferId(7), document.plainText)
        .copy(
          richText = RichTextState(richTextDocument = Some(document)),
          viewport = Viewport(topLine = 0, leftColumn = 0, visibleColumns = 40, visibleLines = 80)
        )
    def bold(body: String, role: ParagraphRole) = RichTextParagraph(
      List(RichTextRun(body.take(20)), RichTextRun(body.drop(20), RichTextStyle.empty.withMark(InlineMark.Bold))),
      role = role
    )
    Vector(ParagraphRole.Body, ParagraphRole.dropCap(lines = 3)).foreach { role =>
      val cache = WrappedLineCache.bounded()
      def snapshot(body: String, wrapCache: WrappedLineCache) =
        TextLayoutSnapshot.fromBuffer(buffer(bold(body, role)), 200, serif.font, frc, wrapCache = wrapCache)
      val before = text
      snapshot(before, cache)
      val after = before.patch(40, "ab", 0)
      snapshot(after, cache) shouldBe snapshot(after, WrappedLineCache.Uncached)
      withClue(s"$role: ")(incrementalWraps(cache) shouldBe 0L)
    }
  }

  it should "fall back whenever any non-text ingredient differs, and never resume across shapes" in {
    val noFractional = FontRenderContext(
      AffineTransform(),
      RenderingHints.VALUE_TEXT_ANTIALIAS_ON,
      RenderingHints.VALUE_FRACTIONALMETRICS_OFF
    )
    val serif = layouts.find(_.name == "serif").get
    val after = text.patch(40, "ab", 0)
    val otherShapes: Vector[WrappedLineCache => Vector[TextVisualLine]] = Vector(
      cache => wrap(after, cache, sans, 161),
      cache => wrap(after, cache, sans, 160, baseColumn = 1),
      cache => wrap(after, cache, serif, 160),
      cache => TextLayoutSnapshot.boundedVisualLinesForText(after, 0, 160, sans.font, noFractional, wrapCache = cache),
      cache => wrap(after, cache, layouts.head, 160)
    )
    otherShapes.zipWithIndex.foreach { (wrapAfter, index) =>
      val cache = WrappedLineCache.bounded()
      wrap(text, cache, sans, 160)
      val actual = bitwise(wrapAfter(cache))
      withClue(s"shape $index: ")(actual shouldBe bitwise(wrapAfter(WrappedLineCache.Uncached)))
      incrementalWraps(cache) shouldBe 0L
    }
  }

  it should "pick a predecessor of the same shape when lines of several shapes are interleaved" in {
    val cache  = WrappedLineCache.bounded()
    val serif  = layouts.find(_.name == "serif").get
    val widths = Vector(160, 161, 240)
    val fonts  = Vector(sans, serif)
    val shapes = for font <- fonts; width <- widths yield (font, width)
    shapes.foreach((font, width) => wrap(text, cache, font, width))
    shapes.foreach { (font, width) =>
      val before = incrementalWraps(cache)
      check(cache, text.patch(33, "xy", 0), font, width)
      incrementalWraps(cache) shouldBe before + 1
    }
  }

  it should "forget predecessors the line cache has evicted" in {
    val cache = WrappedLineCache.bounded(maxLines = 2)
    wrap(text, cache, sans, 160)
    (0 until 4).foreach(index => wrap(paragraphFor(Random(100L + index), sans, 160, 12), cache, sans, 160))
    val before = incrementalWraps(cache)
    check(cache, text.patch(30, "ab", 0), sans, 160)
    incrementalWraps(cache) shouldBe before
  }

  it should "keep only the most recent wraps of a shape as predecessors" in {
    val cache = WrappedLineCache.bounded()
    val first = text
    wrap(first, cache, sans, 160)
    (1 to WrappedLineCache.MaxRecentPerShape).foreach(index =>
      wrap(s"other line number $index ${"w " * 20}", cache, sans, 160)
    )
    val before = incrementalWraps(cache)
    check(cache, first.patch(30, "ab", 0), sans, 160)
    incrementalWraps(cache) shouldBe before
  }
