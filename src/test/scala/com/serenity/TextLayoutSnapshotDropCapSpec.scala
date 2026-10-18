package com.serenity

import java.awt.Font

import com.serenity.richtext.{ParagraphRole, RichTextDocument, RichTextParagraph}
import com.serenity.rope.Balance
import com.serenity.state.models.*
import com.serenity.ui.layout.{
  BundledInterFont,
  CellMetrics,
  DropCapGlyph,
  DropCapLayout,
  LayoutRect,
  TextLayoutSnapshot,
  TextRowMetrics
}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Drop cap layout coverage for [[TextLayoutSnapshot]] -- split out of `TextLayoutSnapshotSpec` to keep both files
  * under the architecture size targets.
  */
class TextLayoutSnapshotDropCapSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  "TextLayoutSnapshot drop cap layout" should
    "start every one of a drop cap paragraph's first n visual lines at the glyph's width plus the gap" in {
      val text     = "Hello there this wraps across several lines of body text"
      val document = RichTextDocument(List(RichTextParagraph.plain(text, role = ParagraphRole.dropCap(lines = 2))))
      val buffer = Buffer
        .fromString(BufferId(30), document.plainText)
        .copy(
          richText = RichTextState(richTextDocument = Some(document)),
          viewport = Viewport(topLine = 0, leftColumn = 0, visibleColumns = 40, visibleLines = 6)
        )
      val font    = Font(Font.SANS_SERIF, Font.PLAIN, 12)
      val metrics = CellMetrics.fromFont(font)
      val frc     = TextLayoutSnapshot.defaultFontRenderContext()

      val snapshot = TextLayoutSnapshot.fromBuffer(buffer, panelWidthPx = metrics.charWidth * 14, font)

      val glyph = DropCapGlyph
        .forParagraph(font, frc, Some(document), 0, ParagraphRole.DropCap(2), proseScale = 1.0f)
        .getOrElse(fail("expected a drop cap glyph"))
      val expectedIndentPx = glyph.widthPx + DropCapLayout.DefaultGapEm * font.getSize2D

      snapshot.visualLines.length should be >= 3
      // The second line has no small first character to make room for, so its offset is the indent itself.
      snapshot.visualLines(1).xOffsetPx shouldBe expectedIndentPx +- 0.5f
      // The first line keeps the width of the character the glyph replaces in its caret stops, and takes it off its
      // offset, so its text starts at the same x as the second line's.
      val home = snapshot.visualLines(0)
      home.xForColumn(1).get shouldBe expectedIndentPx +- 0.5f
      home.xOffsetPx should be < expectedIndentPx
      // The third visual line falls outside the drop cap's 2-line span, so it reserves no inset.
      snapshot.visualLines(2).xOffsetPx shouldBe 0.0f

      // The reserved indent narrows the wrap width on the indented lines rather than merely shifting overflowing
      // text: the drawn text plus its indent must still fit inside the panel.
      snapshot.visualLines.take(2).foreach { line =>
        (line.widthPx + line.xOffsetPx) should be <= (metrics.charWidth * 14).toFloat + 2.0f
      }

      // The glyph's oversized style must never inflate the line height its home lines measure with -- they stay at
      // the paragraph's ordinary body height, same as the snapshot's own uniform per-line height.
      snapshot.visualLines(0).heightPx shouldBe snapshot.lineHeightPx
      snapshot.visualLines(1).heightPx shouldBe snapshot.lineHeightPx
    }

  it should "leave a non-drop-cap paragraph's visual lines with no left inset" in {
    val text     = "Hello there this wraps across several lines of body text"
    val document = RichTextDocument(List(RichTextParagraph.plain(text)))
    val buffer = Buffer
      .fromString(BufferId(31), document.plainText)
      .copy(
        richText = RichTextState(richTextDocument = Some(document)),
        viewport = Viewport(topLine = 0, leftColumn = 0, visibleColumns = 40, visibleLines = 6)
      )
    val font    = Font(Font.SANS_SERIF, Font.PLAIN, 12)
    val metrics = CellMetrics.fromFont(font)

    val snapshot = TextLayoutSnapshot.fromBuffer(buffer, panelWidthPx = metrics.charWidth * 14, font)

    snapshot.visualLines.foreach(_.xOffsetPx shouldBe 0.0f)
  }

  it should "degrade a drop cap paragraph to plain body layout when the config toggle is off" in {
    val text = "Hello there this wraps across several lines of body text"
    val dropCapDocument =
      RichTextDocument(List(RichTextParagraph.plain(text, role = ParagraphRole.dropCap(lines = 2))))
    val bodyDocument = RichTextDocument(List(RichTextParagraph.plain(text)))
    val viewport     = Viewport(topLine = 0, leftColumn = 0, visibleColumns = 40, visibleLines = 6)
    val font         = Font(Font.SANS_SERIF, Font.PLAIN, 12)
    val metrics      = CellMetrics.fromFont(font)
    val panelWidthPx = metrics.charWidth * 14

    val dropCapBuffer = Buffer
      .fromString(BufferId(32), dropCapDocument.plainText)
      .copy(richText = RichTextState(richTextDocument = Some(dropCapDocument)), viewport = viewport)
    val bodyBuffer = Buffer
      .fromString(BufferId(33), bodyDocument.plainText)
      .copy(richText = RichTextState(richTextDocument = Some(bodyDocument)), viewport = viewport)

    val toggledOffSnapshot =
      TextLayoutSnapshot.fromBuffer(dropCapBuffer, panelWidthPx, font, dropCapsEnabled = false)
    val plainBodySnapshot = TextLayoutSnapshot.fromBuffer(bodyBuffer, panelWidthPx, font, dropCapsEnabled = true)

    toggledOffSnapshot.visualLines.map(_.text) shouldBe plainBodySnapshot.visualLines.map(_.text)
    toggledOffSnapshot.visualLines.map(_.xOffsetPx) shouldBe plainBodySnapshot.visualLines.map(_.xOffsetPx)
    toggledOffSnapshot.visualLines.map(_.heightPx) shouldBe plainBodySnapshot.visualLines.map(_.heightPx)
  }

  private def snapshotOf(
    paragraphs: List[RichTextParagraph],
    forceCellLayout: Boolean = false,
    font: Font = BundledInterFont.plain(14f)
  ): TextLayoutSnapshot =
    val document = RichTextDocument(paragraphs)
    val buffer = Buffer
      .fromString(BufferId(34), document.plainText)
      .copy(
        richText = RichTextState(richTextDocument = Some(document)),
        viewport = Viewport(topLine = 0, leftColumn = 0, visibleColumns = 80, visibleLines = 12)
      )
    TextLayoutSnapshot.fromBuffer(buffer, panelWidthPx = 600, font, forceCellLayout = forceCellLayout)

  private def topsOf(snapshot: TextLayoutSnapshot): Vector[Int] = snapshot.rowHeightsPx.scanLeft(0)(_ + _)

  "A drop cap paragraph shorter than its glyph" should
    "reserve the glyph's height, so the next paragraph starts at or below the glyph box's bottom" in {
      val lines = 4
      val snapshot = snapshotOf(
        List(
          RichTextParagraph.plain("A short opening paragraph.", role = ParagraphRole.dropCap(lines)),
          RichTextParagraph.plain("The paragraph that follows it.")
        )
      )
      val home        = snapshot.visualLines.head
      val glyphBottom = DropCapLayout.glyphHeightPx(ParagraphRole.dropCap(lines), home.heightPx)
      val nextTopPx   = topsOf(snapshot)(snapshot.visualLines.indexWhere(_.bufferLine == 1))

      snapshot.visualLines.count(_.bufferLine == 0) should be < lines
      nextTopPx should be >= glyphBottom
      nextTopPx shouldBe glyphBottom
    }

  it should "keep the rows, text and caret geometry of the same paragraph without the cap" in {
    val text = "A short opening paragraph."
    val capped =
      snapshotOf(List(RichTextParagraph.plain(text, role = ParagraphRole.dropCap(4)), RichTextParagraph.plain("Next.")))
    val uncapped = snapshotOf(List(RichTextParagraph.plain(text), RichTextParagraph.plain("Next.")))

    capped.visualLines.map(line => (line.bufferLine, line.startColumn, line.endColumn)) shouldBe
      uncapped.visualLines.map(line => (line.bufferLine, line.startColumn, line.endColumn))
    capped.visualLines.map(_.heightPx) shouldBe uncapped.visualLines.map(_.heightPx)
  }

  it should "own the pixels of its padding for clicks, which land on its last row" in {
    val snapshot = snapshotOf(
      List(
        RichTextParagraph.plain("A short opening paragraph.", role = ParagraphRole.dropCap(4)),
        RichTextParagraph.plain("The paragraph that follows it.")
      )
    )
    val metrics = TextRowMetrics(
      LayoutRect(0, 0, 80, 40),
      CellMetrics(charWidth = 1, lineHeight = 1, ascent = 1),
      snapshot.lineHeightPx,
      usesMeasuredLayout = true,
      rowHeightsPx = snapshot.rowHeightsPx
    )
    val nextRow = snapshot.visualLines.indexWhere(_.bufferLine == 1)
    val nextTop = topsOf(snapshot)(nextRow)

    metrics.visualRowAt(nextTop - 1) shouldBe nextRow - 1
    metrics.visualRowAt(nextTop) shouldBe nextRow
    metrics.lineTopPx(nextRow) shouldBe nextTop
  }

  it should "reserve nothing when the paragraph already reaches the foot of the glyph" in {
    val snapshot = snapshotOf(
      List(
        RichTextParagraph.plain(
          "Hello there this wraps across several lines of body text. " * 6,
          role = ParagraphRole.dropCap(2)
        )
      )
    )

    snapshot.visualLines.map(_.reservedBelowPx).distinct shouldBe Vector(0)
  }

  it should "reserve nothing in the cell layout, where a glyph never spans rows" in {
    val snapshot = snapshotOf(
      List(
        RichTextParagraph.plain("A short opening paragraph.", role = ParagraphRole.dropCap(4)),
        RichTextParagraph.plain("The paragraph that follows it.")
      ),
      forceCellLayout = true,
      font = Font(Font.MONOSPACED, Font.PLAIN, 12)
    )

    snapshot.usesMeasuredLayout shouldBe false
    snapshot.visualLines.map(_.reservedBelowPx).distinct shouldBe Vector(0)
    snapshot.rowHeightsPx.distinct shouldBe Vector(snapshot.lineHeightPx)
  }
