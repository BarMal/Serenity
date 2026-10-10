package com.serenity.ui.layout

import java.awt.Font

import com.serenity.markdown.MarkdownBlockLens.FenceRangeIndex
import com.serenity.markdown.{MarkdownInlineView, MarkerMode}
import com.serenity.state.models.TextVisualLine
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Markdown markers hidden by the inline view take no width: the line lays out, wraps and answers caret and hit queries
  * exactly as the same text without them would, while the columns stay in the buffer text.
  */
class HiddenColumnLayoutSpec extends AnyFlatSpec with Matchers:

  private val frc       = TextLayoutSnapshot.defaultFontRenderContext()
  private val serif     = Font(Font.SERIF, Font.PLAIN, 16)
  private val boldSerif = serif.deriveFont(Font.BOLD)
  private val tolerance = 0.05f

  private val noFences = FenceRangeIndex(Vector.empty)

  private def view(
    mode: MarkerMode,
    revealed: Vector[Range.Inclusive] = Vector.empty,
    fences: FenceRangeIndex = noFences
  ) =
    MarkdownInlineView(mode, revealed, fences)

  private def context(inline: MarkdownInlineView): RichTextContext =
    RichTextContext(None, 1.0f, dropCapsEnabled = true, markdown = inline)

  private def rows(
    text: String,
    inline: MarkdownInlineView,
    font: Font = serif,
    widthPx: Int = 4000,
    bufferLine: Int = 0
  ): Vector[TextVisualLine] =
    TextLayoutSnapshot.boundedVisualLinesForText(
      text,
      bufferLine,
      widthPx,
      font,
      frc,
      richText = context(inline)
    )

  private def advance(text: String, font: Font): Float =
    TextLayoutSnapshot.caretXsForText(text, font, frc).last

  private def read(text: String, font: Font = serif, widthPx: Int = 4000): TextVisualLine =
    rows(text, view(MarkerMode.Read), font, widthPx).head

  "A hidden marker" should "take no width, so the line is as wide as the styled text alone" in {
    val line = read("a **bold** b")

    line.widthPx shouldBe (advance("a ", serif) + advance("bold", boldSerif) + advance(" b", serif) +- tolerance)
  }

  it should "share its caret position with the next visible column" in {
    val line = read("a **bold** b")

    line.xForColumn(2) shouldBe line.xForColumn(4)
    line.xForColumn(3) shouldBe line.xForColumn(4)
    line.xForColumn(8) shouldBe line.xForColumn(10)
    line.xForColumn(9) shouldBe line.xForColumn(10)
  }

  it should "still be a column of the line text" in {
    val line = read("a **bold** b")

    line.text shouldBe "a **bold** b"
    line.endColumn shouldBe 12
  }

  it should "never be where a click or a vertical move lands" in {
    val line = read("a **bold** b")

    line.nearestColumnForXPx(line.xForColumn(4).getOrElse(-1.0f)) shouldBe 4
    line.nearestColumnForXPx(line.xForColumn(10).getOrElse(-1.0f)) shouldBe 10
    line.nearestColumnForXPx(0.0f) shouldBe 0
    line.nearestColumnForXPx(line.widthPx + 50.0f) shouldBe 12
  }

  it should "stay hidden when the shaping of the line needs a full text layout" in {
    val line = read("é **b** c")

    line.xForColumn(3) shouldBe line.xForColumn(5)
    line.xForColumn(4) shouldBe line.xForColumn(5)
    line.xForColumn(6) shouldBe line.xForColumn(8)
    line.xForColumn(7) shouldBe line.xForColumn(8)
    line.widthPx shouldBe (advance("é ", serif) + advance("b", boldSerif) + advance(" c", serif) +- tolerance)
  }

  it should "be measured by glyphs even in a font the editor otherwise lays out on a cell grid" in {
    val mono = Font(Font.MONOSPACED, Font.PLAIN, 14)
    val line = read("a **b** c", mono)

    line.widthPx shouldBe (advance("a ", mono) + advance("b", mono.deriveFont(Font.BOLD)) + advance(
      " c",
      mono
    ) +- tolerance)
    line.xForColumn(2) shouldBe line.xForColumn(4)
  }

  "Wrapping" should "fit a row by its visible text, keeping a word's hidden markers with it" in {
    val fitting = advance("aaaa ", serif) + advance("bbbb", boldSerif) + advance(" ", serif) + 3.0f
    val hidden  = rows("aaaa **bbbb** cccc", view(MarkerMode.Read), widthPx = fitting.toInt)
    val shown   = rows("aaaa **bbbb** cccc", view(MarkerMode.Off), widthPx = fitting.toInt)

    hidden.head.text shouldBe "aaaa **bbbb** "
    shown.head.text shouldBe "aaaa "
  }

  "A revealed line" should "show its markers at full width while keeping the styling" in {
    val revealed   = rows("a **bold** b", view(MarkerMode.Live, Vector(0 to 0))).head
    val hidden     = rows("a **bold** b", view(MarkerMode.Live, Vector(3 to 3))).head
    val hiddenRead = read("a **bold** b")

    revealed.xForColumn(4).getOrElse(0.0f) should be > revealed.xForColumn(2).getOrElse(0.0f)
    revealed.widthPx shouldBe (hiddenRead.widthPx + advance("****", serif) +- tolerance)
    hidden.widthPx shouldBe hiddenRead.widthPx
  }

  "Lines inside a fenced code block" should "keep their markers and their plain width" in {
    val fenced =
      rows("a **bold** b", view(MarkerMode.Read, fences = FenceRangeIndex(Vector(0 to 2))), bufferLine = 1).head
    val plain = rows("a **bold** b", MarkdownInlineView.Off).head

    fenced.widthPx shouldBe plain.widthPx
    fenced.xForColumn(4).getOrElse(0.0f) should be > fenced.xForColumn(2).getOrElse(0.0f)
  }

  "A line with nothing to hide" should "lay out as it does without the view" in {
    val withView = rows("plain prose with no markup", view(MarkerMode.Read)).head
    val without  = rows("plain prose with no markup", MarkdownInlineView.Off).head

    withView shouldBe without
  }
