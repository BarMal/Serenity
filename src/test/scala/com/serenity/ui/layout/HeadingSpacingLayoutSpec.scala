package com.serenity.ui.layout

import java.awt.Font

import com.serenity.config.MarkdownViewMode
import com.serenity.lsp.config.LanguageId
import com.serenity.richtext.{ParagraphRole, RichTextDocument, RichTextParagraph}
import com.serenity.rope.Balance
import com.serenity.state.models.{Buffer, BufferId, RichTextState, TextVisualLine, Viewport}
import com.serenity.ui.theme.{HeadingSettings, HeadingStyle}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** The space above and below a heading is read onto its rows when the layout is built: it belongs to the paragraph's
  * first and last visual line, never to the wrap cache, and it leaves the rows themselves (and so every row count)
  * exactly as they were.
  */
class HeadingSpacingLayoutSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private val font       = Font(Font.SERIF, Font.PLAIN, 14)
  private val panelWidth = 600
  private val longTitle  = "A long chapter title that has to wrap across several rows of the page " * 2

  private val body = "Body words that run on for a while but fit one row."

  private val document = RichTextDocument(
    List(
      RichTextParagraph.plain("Title", role = ParagraphRole.Heading(1)),
      RichTextParagraph.plain(body),
      RichTextParagraph.plain("Chapter two", role = ParagraphRole.Heading(2)),
      RichTextParagraph.plain(body),
      RichTextParagraph.plain(longTitle.trim, role = ParagraphRole.Heading(3)),
      RichTextParagraph.plain(body)
    )
  )

  private def richBuffer(doc: RichTextDocument = document, topLine: Int = 0, topVisualLine: Int = 0): Buffer =
    Buffer
      .fromString(BufferId(1), doc.plainText)
      .copy(
        richText = RichTextState(richTextDocument = Some(doc)),
        viewport = Viewport(
          topLine = topLine,
          topVisualLine = topVisualLine,
          leftColumn = 0,
          visibleColumns = 80,
          visibleLines = 30
        )
      )

  private def spaced(level: Int, before: Float, after: Float): HeadingSettings =
    val styles = HeadingStyle.defaults.updated(
      level - 1,
      HeadingStyle.default(level).copy(spaceBeforeEm = before, spaceAfterEm = after)
    )
    HeadingSettings(styles = styles)

  private def allSpaced(before: Float, after: Float): HeadingSettings =
    HeadingSettings(styles =
      HeadingStyle.defaults.zipWithIndex.map((style, _) => style.copy(spaceBeforeEm = before, spaceAfterEm = after))
    )

  private def snapshotOf(
    buffer: Buffer,
    headings: HeadingSettings,
    cache: WrappedLineCache = WrappedLineCache.Uncached,
    width: Int = panelWidth
  ): TextLayoutSnapshot =
    TextLayoutSnapshot.fromBuffer(buffer, width, font, headings = headings, wrapCache = cache)

  private def rowsOfLine(snapshot: TextLayoutSnapshot, line: Int): Vector[TextVisualLine] =
    snapshot.visualLines.filter(_.bufferLine == line)

  private def px(em: Float, level: Int, headings: HeadingSettings): Int =
    math.round(em * headings.sizeMultiple(level) * 12.0f)

  "A heading's space" should "be none anywhere while every level is automatic" in {
    val snapshot = snapshotOf(richBuffer(), HeadingSettings.default)

    snapshot.visualLines.map(line => (line.spaceBeforePx, line.spaceAfterPx)).distinct shouldBe Vector((0, 0))
  }

  it should "leave rows identical to the default look when it is configured to zero" in {
    val zero = snapshotOf(richBuffer(), allSpaced(0.0f, 0.0f))

    zero.visualLines shouldBe snapshotOf(richBuffer(), HeadingSettings.default).visualLines
  }

  it should "sit above the first row and below the last row of the heading, in em of the heading's own size" in {
    val settings = spaced(2, before = 1.5f, after = 0.5f)
    val chapter  = rowsOfLine(snapshotOf(richBuffer(), settings), 2)

    chapter should have size 1
    chapter.head.spaceBeforePx shouldBe px(1.5f, 2, settings)
    chapter.head.spaceAfterPx shouldBe px(0.5f, 2, settings)
    chapter.head.spaceBeforePx should be > 0
  }

  it should "scale with the heading's size, so the same em is larger for a larger heading" in {
    val settings = allSpaced(1.0f, 1.0f)
    val snapshot = snapshotOf(richBuffer(), settings)

    val h2 = rowsOfLine(snapshot, 2).head
    h2.spaceBeforePx shouldBe px(1.0f, 2, settings)
    val h1 = snapshotOf(richBuffer(topLine = 0), settings).visualLines.head
    h1.spaceAfterPx shouldBe px(1.0f, 1, settings)
    h1.spaceAfterPx should be > h2.spaceAfterPx
  }

  it should "reach only the first row above and the last row below a heading that wraps" in {
    val settings = spaced(3, before = 1.0f, after = 2.0f)
    val wrapped  = rowsOfLine(snapshotOf(richBuffer(), settings, width = 300), 4)

    wrapped.size should be >= 3
    wrapped.head.spaceBeforePx shouldBe px(1.0f, 3, settings)
    wrapped.map(_.spaceBeforePx).tail.distinct shouldBe Vector(0)
    wrapped.last.spaceAfterPx shouldBe px(2.0f, 3, settings)
    wrapped.map(_.spaceAfterPx).init.distinct shouldBe Vector(0)
  }

  it should "leave body rows without any space, whatever the headings around them have" in {
    val snapshot = snapshotOf(richBuffer(), allSpaced(2.0f, 2.0f), width = 300)

    List(1, 3, 5)
      .flatMap(rowsOfLine(snapshot, _))
      .map(line => (line.spaceBeforePx, line.spaceAfterPx))
      .distinct shouldBe
      Vector((0, 0))
  }

  it should "take each level's own settings" in {
    val settings = spaced(2, before = 1.0f, after = 0.0f)
    val snapshot = snapshotOf(richBuffer(), settings)

    rowsOfLine(snapshot, 2).head.spaceBeforePx should be > 0
    rowsOfLine(snapshot, 4).head.spaceBeforePx shouldBe 0
    snapshot.visualLines.head.spaceBeforePx shouldBe 0
  }

  "The space above a heading" should "be dropped for the first row of the document, which has no page above it" in {
    val snapshot = snapshotOf(richBuffer(), spaced(1, before = 2.0f, after = 1.0f))
    val title    = snapshot.visualLines.head

    title.bufferLine shouldBe 0
    title.spaceBeforePx shouldBe 0
    title.spaceAfterPx should be > 0
  }

  it should "be dropped for a heading that scrolls to the top of the window" in {
    val settings = spaced(2, before = 2.0f, after = 1.0f)
    val inFlow   = rowsOfLine(snapshotOf(richBuffer(), settings), 2).head
    val atTop    = snapshotOf(richBuffer(topLine = 2), settings).visualLines.head

    atTop.bufferLine shouldBe 2
    inFlow.spaceBeforePx should be > 0
    atTop.spaceBeforePx shouldBe 0
    atTop.spaceAfterPx shouldBe inFlow.spaceAfterPx
  }

  it should "stay dropped only for the top row, so the next heading below keeps its space" in {
    val settings = allSpaced(1.0f, 0.0f)
    val snapshot = snapshotOf(richBuffer(topLine = 2), settings)

    snapshot.visualLines.head.spaceBeforePx shouldBe 0
    rowsOfLine(snapshot, 4).head.spaceBeforePx should be > 0
  }

  it should "not appear on a row that continues a heading scrolled partway" in {
    val settings = spaced(3, before = 1.0f, after = 2.0f)
    val snapshot = snapshotOf(richBuffer(topLine = 4, topVisualLine = 1), settings, width = 300)

    snapshot.visualLines.head.bufferLine shouldBe 4
    snapshot.visualLines.head.startColumn should be > 0
    snapshot.visualLines.head.spaceBeforePx shouldBe 0
  }

  "A column page" should "drop the space above its first row in every column, as a page does" in {
    val settings = allSpaced(1.0f, 1.0f)
    // A heading is kept with the paragraph after it, so it keeps its space only where it sits clear of a column's foot:
    // two body rows above it and columns of fourteen lines leave it room in any font.
    val sparse = RichTextDocument(
      List(
        RichTextParagraph.plain(body),
        RichTextParagraph.plain(body),
        RichTextParagraph.plain("Chapter", role = ParagraphRole.Heading(2))
      ) ++ List.fill(60)(RichTextParagraph.plain(body))
    )
    val buffer = richBuffer(sparse).copy(viewport = richBuffer().viewport.copy(visibleLines = 14))
    val columns =
      TextLayoutSnapshot.fromBufferColumns(buffer, panelWidth, font, columnCount = 3, headings = settings)

    columns.size should be >= 2
    columns.foreach(column => column.visualLines.head.spaceBeforePx shouldBe 0)
    columns.flatMap(_.visualLines).exists(_.spaceBeforePx > 0) shouldBe true
  }

  "Heading space" should "not change the rows, so row counts and wrap points stay as they were" in {
    val plain  = snapshotOf(richBuffer(), HeadingSettings.default, width = 300)
    val spaced = snapshotOf(richBuffer(), allSpaced(2.0f, 1.0f), width = 300)

    spaced.visualLines.map(_.row) shouldBe plain.visualLines.map(_.row)
    spaced.visualLines.map(_.bufferLine) shouldBe plain.visualLines.map(_.bufferLine)
    spaced.visualLines.map(_.xOffsetPx) shouldBe plain.visualLines.map(_.xOffsetPx)
  }

  it should "not enter the wrap cache's key, so changing it wraps nothing again" in {
    val cache = WrappedLineCache.bounded()
    snapshotOf(richBuffer(), HeadingSettings.default, cache)
    val before = cache.wrapStats

    val bigger = snapshotOf(richBuffer(), allSpaced(3.0f, 3.0f), cache)

    cache.wrapStats shouldBe before
    bigger.visualLines.exists(_.spaceAfterPx > 0) shouldBe true
  }

  it should "give a cached layout the same space as an uncached one" in {
    val cache    = WrappedLineCache.bounded()
    val settings = allSpaced(1.25f, 0.75f)
    snapshotOf(richBuffer(), HeadingSettings.default, cache)

    snapshotOf(richBuffer(), settings, cache).visualLines shouldBe
      snapshotOf(richBuffer(), settings, WrappedLineCache.Uncached).visualLines
  }

  it should "round to whole rows on a cell grid, a half row or more being one row" in {
    def spaceOf(em: Float): (Int, Int) =
      val snapshot = TextLayoutSnapshot.fromBuffer(
        richBuffer(),
        80,
        font,
        headings = spaced(2, em, em),
        cellMetricsOverride = Some(CellMetrics.cellUnit),
        forceCellLayout = true
      )
      val row = rowsOfLine(snapshot, 2).head
      (row.spaceBeforePx, row.spaceAfterPx)

    spaceOf(0.0f) shouldBe (0, 0)
    spaceOf(0.49f) shouldBe (0, 0)
    spaceOf(0.5f) shouldBe (1, 1)
    spaceOf(1.0f) shouldBe (1, 1)
    spaceOf(1.49f) shouldBe (1, 1)
    spaceOf(1.5f) shouldBe (2, 2)
    spaceOf(3.0f) shouldBe (3, 3)
  }

  it should "round to whole cell rows in the cell grid's own unit when a font draws the grid" in {
    val metrics = CellMetrics.fromFont(Font(Font.MONOSPACED, Font.PLAIN, 12))
    val snapshot = TextLayoutSnapshot.fromBuffer(
      richBuffer(),
      80 * metrics.charWidth,
      Font(Font.MONOSPACED, Font.PLAIN, 12),
      headings = spaced(2, 1.0f, 0.0f),
      forceCellLayout = true
    )

    rowsOfLine(snapshot, 2).head.spaceBeforePx shouldBe metrics.lineHeight
  }

  "A Markdown heading" should "carry its space only where the preview styles it" in {
    val source = "Opening words.\n\n# Chapter one\n\nSome text.\n\n## Section\n\nMore text."
    val base   = Buffer.fromString(BufferId(2), source)
    val buffer = base.copy(
      document = base.document.copy(language = Some(LanguageId.Markdown)),
      viewport = Viewport(topLine = 0, leftColumn = 0, visibleColumns = 80, visibleLines = 30)
    )
    val settings = allSpaced(1.0f, 0.5f)

    def lineOf(mode: MarkdownViewMode, line: Int): TextVisualLine =
      TextLayoutSnapshot
        .fromBuffer(buffer, panelWidth, font, headings = settings, markdownViewMode = mode)
        .visualLines
        .find(_.bufferLine == line)
        .getOrElse(fail(s"no row for line $line"))

    lineOf(MarkdownViewMode.Source, 2).spaceBeforePx shouldBe 0
    lineOf(MarkdownViewMode.Source, 2).spaceAfterPx shouldBe 0

    List(MarkdownViewMode.LivePreview, MarkdownViewMode.Read).foreach { mode =>
      withClue(mode.toString) {
        val chapter = lineOf(mode, 2)
        chapter.spaceBeforePx shouldBe math.round(settings.sizeMultiple(1) * font.getSize2D)
        chapter.spaceAfterPx shouldBe math.round(0.5f * settings.sizeMultiple(1) * font.getSize2D)
        lineOf(mode, 6).spaceBeforePx should be > 0
        lineOf(mode, 4).spaceBeforePx shouldBe 0
        lineOf(mode, 0).spaceAfterPx shouldBe 0
      }
    }
  }
