package com.serenity.ui.layout

import java.awt.Font

import com.serenity.richtext.{ParagraphAlignment, ParagraphRole, RichTextDocument, RichTextParagraph}
import com.serenity.rope.Balance
import com.serenity.state.models.{Buffer, BufferId, RichTextState, TextVisualLine, Viewport}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** The cached wrap is a function of the paragraph's text and shape only: the buffer line, the paragraph alignment and
  * the drop cap inset are applied when a row is read (#1908).
  */
class PositionIndependentWrapSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private val font = Font(Font.SANS_SERIF, Font.PLAIN, 13)
  private val prose =
    "The quick brown fox jumps over the lazy dog while the wrap width keeps folding this sentence onto new rows."

  private def buffer(paragraphs: List[RichTextParagraph]): Buffer =
    val document = RichTextDocument(paragraphs)
    Buffer
      .fromString(BufferId(21), document.plainText)
      .copy(
        richText = RichTextState(richTextDocument = Some(document)),
        viewport = Viewport(topLine = 0, leftColumn = 0, visibleColumns = 40, visibleLines = 60)
      )

  private def visualLines(source: Buffer, cache: WrappedLineCache, widthPx: Int = 220): Vector[TextVisualLine] =
    TextLayoutSnapshot.fromBuffer(source, widthPx, font, wrapCache = cache).visualLines

  private def sameInstance(first: AnyRef, second: AnyRef): Boolean = first eq second

  "A cached wrap" should "serve a paragraph moved to another buffer line from the very rows it cached" in {
    val cache = WrappedLineCache.bounded()
    val above = TextLayoutSnapshot.boundedVisualLinesForText(prose, 3, 180, font, wrapCache = cache)
    val moved = TextLayoutSnapshot.boundedVisualLinesForText(prose, 7, 180, font, wrapCache = cache)
    val back  = TextLayoutSnapshot.boundedVisualLinesForText(prose, 3, 180, font, wrapCache = cache)

    above.length should be > 2
    above.map(_.bufferLine).distinct shouldBe Vector(3)
    moved.map(_.bufferLine).distinct shouldBe Vector(7)
    back.map(_.bufferLine).distinct shouldBe Vector(3)
    moved.zip(above).foreach((a, b) => sameInstance(a.row, b.row) shouldBe true)
    back.zip(above).foreach((a, b) => sameInstance(a.row, b.row) shouldBe true)
    cache.size shouldBe 1
    cache.wrapStats.coldWraps shouldBe 1L
    cache.wrapStats.incrementalWraps shouldBe 0L
  }

  it should "keep every paragraph's cached rows when a line is inserted above them" in {
    val paragraphs = List(RichTextParagraph.plain(prose), RichTextParagraph.plain(prose + " tail"))
    val cache      = WrappedLineCache.bounded()
    val before     = visualLines(buffer(paragraphs), cache)
    val shifted    = visualLines(buffer(RichTextParagraph.plain("inserted") :: paragraphs), cache)

    val moved = shifted.filter(_.bufferLine >= 1)
    moved.map(_.bufferLine) shouldBe before.map(_.bufferLine + 1)
    moved.zip(before).foreach((after, earlier) => sameInstance(after.row, earlier.row) shouldBe true)
    cache.wrapStats.coldWraps shouldBe 3L
  }

  it should "place centred and right-aligned carets by a scalar offset over unshifted cached stops" in {
    val metrics = CellMetrics.fromFont(font)
    val panel   = metrics.charWidth * 40
    val cache   = WrappedLineCache.bounded()
    def rowsAligned(alignment: ParagraphAlignment) =
      visualLines(buffer(List(RichTextParagraph.plain("abcd efgh", alignment = alignment))), cache, panel)

    val left   = rowsAligned(ParagraphAlignment.Left).head
    val center = rowsAligned(ParagraphAlignment.Center).head
    val right  = rowsAligned(ParagraphAlignment.Right).head

    left.xOffsetPx shouldBe 0.0f
    center.xOffsetPx shouldBe (panel.toFloat - left.widthPx) / 2.0f
    right.xOffsetPx shouldBe panel.toFloat - left.widthPx
    sameInstance(center.row, left.row) shouldBe true
    sameInstance(right.row, left.row) shouldBe true
    left.row.caretStops.head.xPx shouldBe 0.0f
    (0 to "abcd efgh".length).foreach { column =>
      center.xForColumn(column) shouldBe left.xForColumn(column).map(_ + center.xOffsetPx)
      right.xForColumn(column) shouldBe left.xForColumn(column).map(_ + right.xOffsetPx)
    }
    center.caretStops.map(_.xPx) shouldBe left.caretStops.map(_.xPx + center.xOffsetPx)
    center.xSortedCaretStops.map(_.xPx) shouldBe left.xSortedCaretStops.map(_.xPx + center.xOffsetPx)
    cache.wrapStats.coldWraps shouldBe 1L
  }

  it should "hit-test an aligned row in the same coordinates it reports its carets in" in {
    val metrics = CellMetrics.fromFont(font)
    val panel   = metrics.charWidth * 40
    val line = visualLines(
      buffer(List(RichTextParagraph.plain("abcd efgh", alignment = ParagraphAlignment.Right))),
      WrappedLineCache.bounded(),
      panel
    ).head

    (0 to "abcd efgh".length).foreach { column =>
      line.nearestColumnForXPx(line.xForColumn(column).getOrElse(fail("missing caret x"))) shouldBe column
    }
    line.nearestColumnForXPx(0.0f) shouldBe 0
    line.nearestColumnForXPx(panel.toFloat) shouldBe "abcd efgh".length
  }

  it should "apply a drop cap inset as a scalar, shifting carets without re-mapping the cached stops" in {
    val dropCap = RichTextParagraph.plain(prose, role = ParagraphRole.dropCap(lines = 2))
    val cache   = WrappedLineCache.bounded()
    val rows    = visualLines(buffer(List(dropCap)), cache)

    rows.length should be >= 3
    val indent = rows(1).xOffsetPx
    indent should be > 0.0f
    rows(2).xOffsetPx shouldBe 0.0f
    rows.foreach(row => row.row.caretStops.head.xPx shouldBe 0.0f)
    // The second row starts at the indent; the first gives back the width of the character its glyph replaces, so
    // the text after that character starts at the same x.
    rows(1).xForColumn(rows(1).startColumn) shouldBe Some(indent)
    rows(1).caretStops.head.xPx shouldBe indent
    val replacedWidthPx = rows.head.row.xForColumn(1).getOrElse(fail("missing caret x"))
    replacedWidthPx should be > 0.0f
    rows.head.xOffsetPx shouldBe (indent - replacedWidthPx) +- 0.001f
    rows.head.xForColumn(1).getOrElse(fail("missing caret x")) shouldBe indent +- 0.001f
    rows.head.caretStops.head.xPx shouldBe (indent - replacedWidthPx) +- 0.001f
    rows(2).caretStops shouldBe rows(2).row.caretStops
    visualLines(buffer(List(dropCap)), cache) shouldBe rows
    cache.wrapStats.coldWraps shouldBe 1L
  }

  it should "add a centred drop cap paragraph's alignment to its inset" in {
    val metrics     = CellMetrics.fromFont(font)
    val panel       = metrics.charWidth * 40
    val dropCap     = RichTextParagraph.plain(prose, role = ParagraphRole.dropCap(lines = 2))
    val centred     = dropCap.copy(alignment = ParagraphAlignment.Center)
    val cache       = WrappedLineCache.bounded()
    val plainRows   = visualLines(buffer(List(dropCap)), cache, panel)
    val centredRows = visualLines(buffer(List(centred)), cache, panel)

    centredRows.zip(plainRows).foreach { (aligned, unaligned) =>
      val available = math.max(0.0f, panel.toFloat - unaligned.xOffsetPx - unaligned.widthPx)
      aligned.xOffsetPx shouldBe unaligned.xOffsetPx + available / 2.0f
      aligned.xForColumn(aligned.startColumn) shouldBe Some(aligned.xOffsetPx)
    }
  }
