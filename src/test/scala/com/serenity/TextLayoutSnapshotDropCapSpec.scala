package com.serenity

import java.awt.Font

import com.serenity.richtext.{ParagraphRole, RichTextDocument, RichTextParagraph, RichTextStyle}
import com.serenity.rope.Balance
import com.serenity.state.models.*
import com.serenity.ui.layout.{CellMetrics, DropCapLayout, TextLayoutSnapshot}
import com.serenity.ui.theme.{RichTextStyling, TextStyle}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Drop cap layout coverage for [[TextLayoutSnapshot]] -- split out of `TextLayoutSnapshotSpec` to keep both files
  * under the architecture size targets.
  */
class TextLayoutSnapshotDropCapSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  "TextLayoutSnapshot drop cap layout" should
    "reserve the glyph's own measured width as a left inset on a drop cap paragraph's first n visual lines" in {
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

      val expectedGlyphStyle = RichTextStyling
        .dropCapGlyphStyle(RichTextStyle.empty, ParagraphRole.DropCap(2), RichTextStyling.ProseZoomBaselinePx, 1.0f)
        .getOrElse(fail("expected a drop cap glyph style"))
      val expectedGlyphWidthPx =
        DropCapLayout.glyphWidthPx(TextStyle.styledFont(font, expectedGlyphStyle), frc, text.take(1))

      snapshot.visualLines.length should be >= 3
      snapshot.visualLines(0).xOffsetPx shouldBe expectedGlyphWidthPx +- 0.5f
      snapshot.visualLines(1).xOffsetPx shouldBe expectedGlyphWidthPx +- 0.5f
      // The third visual line falls outside the drop cap's 2-line span, so it reserves no inset.
      snapshot.visualLines(2).xOffsetPx shouldBe 0.0f

      // The reserved inset narrows the wrap width on the inset lines rather than merely shifting overflowing text:
      // the drawn text plus its inset must still fit inside the panel.
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
