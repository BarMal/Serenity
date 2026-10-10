package com.serenity.ui.layout

import java.awt.Font

import com.serenity.config.MarkdownViewMode
import com.serenity.lsp.config.LanguageId
import com.serenity.markdown.MarkdownBlockLens.FenceRangeIndex
import com.serenity.markdown.{MarkdownBlockSpans, MarkdownInlineView, MarkerMode}
import com.serenity.rope.Balance
import com.serenity.state.models.*
import com.serenity.ui.theme.TextStyle
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Headings, list markers and rules in the layout: a heading is set in a larger face, its hashes take no width unless
  * revealed, and a rule has no width of its own.
  */
class MarkdownBlockLayoutSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private val frc       = TextLayoutSnapshot.defaultFontRenderContext()
  private val serif     = Font(Font.SERIF, Font.PLAIN, 16)
  private val boldSerif = serif.deriveFont(Font.BOLD)
  private val tolerance = 0.05f

  private def headingFont(level: Int): Font =
    TextStyle.styledFont(
      serif,
      TextStyle(isBold = true, fontSize = Some(serif.getSize2D * MarkdownBlockSpans.headingScale(level)))
    )

  private def advance(text: String, font: Font): Float = TextLayoutSnapshot.caretXsForText(text, font, frc).last

  private def view(
    mode: MarkerMode,
    revealed: Vector[Range.Inclusive] = Vector.empty,
    fences: FenceRangeIndex = FenceRangeIndex(Vector.empty)
  ) = MarkdownInlineView(mode, revealed, fences)

  private def rows(text: String, inline: MarkdownInlineView, bufferLine: Int = 0): Vector[TextVisualLine] =
    TextLayoutSnapshot.boundedVisualLinesForText(
      text,
      bufferLine,
      4000,
      serif,
      frc,
      richText = RichTextContext(None, 1.0f, dropCapsEnabled = true, markdown = inline)
    )

  private def read(text: String): TextVisualLine = rows(text, view(MarkerMode.Read)).head

  "A heading" should "lay out in its larger face without its hashes" in {
    read("# Title").widthPx shouldBe (advance("Title", headingFont(1)) +- tolerance)
    read("### Third").widthPx shouldBe (advance("Third", headingFont(3)) +- tolerance)
  }

  it should "share its caret position with the first visible column" in {
    val line = read("## Title")

    line.xForColumn(0) shouldBe line.xForColumn(3)
    line.xForColumn(2) shouldBe line.xForColumn(3)
  }

  it should "be taller than a body row" in {
    read("# Title").heightPx should be > read("Title").heightPx
  }

  it should "show its hashes in the body face when revealed" in {
    val revealed = rows("# Title", view(MarkerMode.Live, Vector(0 to 0))).head

    revealed.widthPx shouldBe (advance("# ", serif) + advance("Title", headingFont(1)) +- tolerance)
  }

  "A list bullet" should "be measured in the bold face and the text after it in the body face" in {
    read("- item").widthPx shouldBe (advance("-", boldSerif) + advance(" item", serif) +- tolerance)
  }

  "A thematic break" should "have no width unless revealed" in {
    read("---").widthPx shouldBe 0.0f
    rows("---", view(MarkerMode.Live, Vector(0 to 0))).head.widthPx shouldBe (advance("---", serif) +- tolerance)
  }

  "A table" should "hide its delimiter row and keep its pipes at full width" in {
    val fences    = FenceRangeIndex(Vector.empty, Vector(0 to 2))
    val inline    = view(MarkerMode.Read, fences = fences)
    val delimiter = rows("|---|---|", inline, bufferLine = 1).head
    val body      = rows("| 1 | 2 |", inline, bufferLine = 2).head

    delimiter.widthPx shouldBe 0.0f
    body.widthPx shouldBe (advance("| 1 | 2 |", serif) +- tolerance)
  }

  private def readSnapshot(text: String, mode: MarkdownViewMode = MarkdownViewMode.Read) =
    val base   = Buffer.fromString(BufferId(1), text)
    val buffer = base.copy(document = base.document.copy(language = Some(LanguageId.Markdown)))
    TextLayoutSnapshot.fromBuffer(buffer, 4000, serif, frc, markdownViewMode = mode)

  private def xOf(line: TextVisualLine, column: Int): Float = line.xForColumn(column).getOrElse(-1.0f)

  "A table in read mode" should "line its pipes up whatever the width of the cells" in {
    val lines = readSnapshot("| a | bbbb |\n|---|---|\n| cccccc | d |").visualLines

    xOf(lines(0), 4) shouldBe (xOf(lines(2), 9) +- tolerance)
    xOf(lines(0), 11) shouldBe (xOf(lines(2), 13) +- tolerance)
    lines(0).widthPx shouldBe (lines(2).widthPx +- tolerance)
  }

  it should "pad a cell before the pipe that ends it, leaving the pipe's own caret stop where the pipe is" in {
    val header = readSnapshot("| a | bbbb |\n|---|---|\n| cccccc | d |").visualLines(0)

    xOf(header, 4) - xOf(header, 3) should be > advance(" ", serif)
    header.nearestColumnForXPx(xOf(header, 3) + 0.25f * (xOf(header, 4) - xOf(header, 3))) shouldBe 3
    header.nearestColumnForXPx(xOf(header, 4) - 0.1f) shouldBe 4
    header.nearestColumnForXPx(xOf(header, 4)) shouldBe 4
  }

  it should "right-align the cells of a column whose delimiter ends in a colon" in {
    val lines = readSnapshot("| a | b |\n|---|---:|\n| cc | dddd |").visualLines

    xOf(lines(0), 4) shouldBe (xOf(lines(2), 5) +- tolerance)
    xOf(lines(0), 8) shouldBe (xOf(lines(2), 12) +- tolerance)
    xOf(lines(0), 6) should be > xOf(lines(2), 7)
  }

  it should "leave a table row alone in source mode" in {
    val lines = readSnapshot("| a | bbbb |\n|---|---|\n| cccccc | d |", MarkdownViewMode.Source).visualLines

    lines(0).widthPx shouldBe (advance("| a | bbbb |", serif) +- tolerance)
  }

  "A setext heading" should "lay out its text in the heading face and its underline without width" in {
    val lines = readSnapshot("Title\n=====\nbody").visualLines

    lines(0).widthPx shouldBe (advance("Title", headingFont(1)) +- tolerance)
    lines(1).widthPx shouldBe 0.0f
    lines(2).widthPx shouldBe (advance("body", serif) +- tolerance)
  }

  it should "leave a rule that follows a blank line a rule" in {
    readSnapshot("text\n\n---").visualLines(2).widthPx shouldBe 0.0f
  }

  "Struck text" should "lay out without its tildes" in {
    read("a ~~b~~ c").widthPx shouldBe (advance("a b c", serif) +- tolerance)
  }

  "A heading line in a buffer" should "make its row taller than the body line under it" in {
    val base     = Buffer.fromString(BufferId(1), "# Title\nbody")
    val buffer   = base.copy(document = base.document.copy(language = Some(LanguageId.Markdown)))
    val snapshot = TextLayoutSnapshot.fromBuffer(buffer, 4000, serif, frc, markdownViewMode = MarkdownViewMode.Read)

    snapshot.visualLines(0).heightPx should be > snapshot.visualLines(1).heightPx
  }

  "A line that is not Markdown block syntax" should "lay out as it does without the view" in {
    rows("plain prose", view(MarkerMode.Read)).head shouldBe rows("plain prose", MarkdownInlineView.Off).head
  }
