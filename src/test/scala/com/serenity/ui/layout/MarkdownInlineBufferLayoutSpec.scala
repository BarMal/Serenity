package com.serenity.ui.layout

import java.awt.Font

import com.serenity.config.MarkdownViewMode
import com.serenity.lsp.config.LanguageId
import com.serenity.markdown.MarkerMode
import com.serenity.rope.Balance
import com.serenity.state.models.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** How a buffer's markdown view mode and caret reach the layout: which lines hide their markers, and that every row
  * count and snapshot of the buffer agrees on it.
  */
class MarkdownInlineBufferLayoutSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private val frc       = TextLayoutSnapshot.defaultFontRenderContext()
  private val serif     = Font(Font.SERIF, Font.PLAIN, 16)
  private val boldSerif = serif.deriveFont(Font.BOLD)

  private def advance(text: String, font: Font): Float = TextLayoutSnapshot.caretXsForText(text, font, frc).last

  private val wrapsOnlyWithMarkers =
    (advance("aaaa ", serif) + advance("bbbb", boldSerif) + 3.0f).toInt

  private def markdownBuffer(
    text: String,
    cursors: List[CursorPosition],
    language: Option[LanguageId] = Some(LanguageId.Markdown)
  ) =
    val base = Buffer.fromString(BufferId(1), text)
    base.copy(
      document = base.document.copy(language = language),
      editing = EditingState(cursors),
      viewport = Viewport.default.copy(visibleLines = 20, visibleColumns = 200)
    )

  private def rowsOfFirstLine(buffer: Buffer, mode: MarkdownViewMode, cache: WrappedLineCache): Int =
    VisualRowCounts
      .forBuffer(
        buffer,
        wrapsOnlyWithMarkers,
        serif,
        cellMetricsOverride = None,
        forceCellLayout = false,
        wrapCache = cache,
        markdownViewMode = mode
      )
      .rowsIn(0)

  private def snapshot(buffer: Buffer, mode: MarkdownViewMode, forceCellLayout: Boolean = false) =
    TextLayoutSnapshot.fromBuffer(
      buffer,
      wrapsOnlyWithMarkers,
      serif,
      frc,
      forceCellLayout = forceCellLayout,
      markdownViewMode = mode
    )

  "Row counts" should "follow the view mode on the same cache and the same text" in {
    val cache  = WrappedLineCache.bounded()
    val buffer = markdownBuffer("aaaa **bbbb**", List(CursorPosition(0, 0)))

    rowsOfFirstLine(buffer, MarkdownViewMode.Source, cache) shouldBe 2
    rowsOfFirstLine(buffer, MarkdownViewMode.Read, cache) shouldBe 1
    rowsOfFirstLine(buffer, MarkdownViewMode.Source, cache) shouldBe 2
  }

  they should "follow the caret in live preview, which reveals the line it is on" in {
    val cache = WrappedLineCache.bounded()
    val text  = "aaaa **bbbb**\nsecond line"

    val caretElsewhere = markdownBuffer(text, List(CursorPosition(1, 0)))
    val caretOnLine    = caretElsewhere.copy(editing = EditingState(List(CursorPosition(0, 3))))

    rowsOfFirstLine(caretElsewhere, MarkdownViewMode.LivePreview, cache) shouldBe 1
    rowsOfFirstLine(caretOnLine, MarkdownViewMode.LivePreview, cache) shouldBe 2
    rowsOfFirstLine(caretElsewhere, MarkdownViewMode.LivePreview, cache) shouldBe 1
  }

  they should "reveal a line a selection reaches" in {
    val cache = WrappedLineCache.bounded()
    val text  = "aaaa **bbbb**\nsecond line"
    val selecting = markdownBuffer(text, Nil).copy(
      editing = EditingState.fromCursors(List(Cursor(CursorPosition(1, 3), Some(CursorPosition(0, 1)))))
    )

    rowsOfFirstLine(selecting, MarkdownViewMode.LivePreview, cache) shouldBe 2
  }

  "A snapshot" should "hide markers off the caret line and show them on it in live preview" in {
    val text = "**a** b\n**c** d"

    val onFirst  = snapshot(markdownBuffer(text, List(CursorPosition(0, 0))), MarkdownViewMode.LivePreview)
    val onSecond = snapshot(markdownBuffer(text, List(CursorPosition(1, 0))), MarkdownViewMode.LivePreview)

    onFirst.visualLines(0).widthPx should be > onFirst.visualLines(1).widthPx
    onSecond.visualLines(0).widthPx should be < onSecond.visualLines(1).widthPx
    onFirst.markdownInline.mode shouldBe MarkerMode.Live
  }

  it should "hide markers on every line in read mode, whatever the caret" in {
    val text     = "**a** b\n**a** b"
    val snapshot = this.snapshot(markdownBuffer(text, List(CursorPosition(0, 0))), MarkdownViewMode.Read)

    snapshot.visualLines(0).widthPx shouldBe snapshot.visualLines(1).widthPx
    snapshot.markdownInline.mode shouldBe MarkerMode.Read
  }

  it should "leave markdown source alone in source mode and in other languages" in {
    val text = "aaaa **bbbb**"

    snapshot(markdownBuffer(text, Nil), MarkdownViewMode.Source).markdownInline.isActive shouldBe false
    snapshot(markdownBuffer(text, Nil), MarkdownViewMode.SplitPreview).markdownInline.isActive shouldBe false
    snapshot(markdownBuffer(text, Nil), MarkdownViewMode.InlineLens).markdownInline.isActive shouldBe false
    snapshot(markdownBuffer(text, Nil, language = None), MarkdownViewMode.Read).markdownInline.isActive shouldBe false
  }

  it should "not hide anything on a cell-grid surface" in {
    val snapshot = this.snapshot(markdownBuffer("aaaa **bbbb**", Nil), MarkdownViewMode.Read, forceCellLayout = true)

    snapshot.markdownInline.isActive shouldBe false
    snapshot.usesMeasuredLayout shouldBe false
  }
