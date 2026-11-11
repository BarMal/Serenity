package com.serenity

import com.serenity.markdown.MarkdownInlineSpans
import com.serenity.markdown.MarkdownInlineSpans.{Run, Style}
import com.serenity.richtext.{InlineMark, RichTextDocument, RichTextParagraph, RichTextRun, RichTextStyle}
import com.serenity.state.models.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** The formatting the caret shows is the formatting the next typed character would get. */
class CaretStyleSpec extends AnyFlatSpec with Matchers:

  private def run(text: String, marks: InlineMark*): RichTextRun =
    RichTextRun(text, RichTextStyle(marks = marks.toSet))

  private def proseBuffer(runs: RichTextRun*): Buffer =
    val document = RichTextDocument(List(RichTextParagraph(runs.toList)))
    val buffer   = Buffer.fromString(BufferId(1), document.plainText)
    buffer.copy(richText = RichTextState().withSyncedDocument(Some(document), buffer.document.contentVersion))

  private def at(column: Int): CursorPosition = CursorPosition(0, column)

  "CaretStyle.forRichText" should "be plain for a buffer that holds no rich text" in {
    CaretStyle.forRichText(Buffer.fromString(BufferId(1), "abc"), at(2)) shouldBe CaretStyle.Plain
  }

  it should "show the marks of the run before the caret" in {
    val buffer = proseBuffer(run("ab", InlineMark.Italic), run("cd", InlineMark.Bold, InlineMark.Underline))

    CaretStyle.forRichText(buffer, at(1)) shouldBe CaretStyle(italic = true)
    CaretStyle.forRichText(buffer, at(3)) shouldBe CaretStyle(bold = true, underline = true)
  }

  it should "be plain after plain text and at the start of an empty line" in {
    val buffer = proseBuffer(run("ab", InlineMark.Italic), run("cd"))

    CaretStyle.forRichText(buffer, at(4)) shouldBe CaretStyle.Plain
    CaretStyle.forRichText(proseBuffer(run("")), at(0)) shouldBe CaretStyle.Plain
  }

  it should "show a style toggled for the next character before anything is typed" in {
    val buffer = proseBuffer(run("ab"))
    val toggled = buffer.copy(richText =
      buffer.richText.copy(insertionRichTextStyle = Some(RichTextStyle(marks = Set(InlineMark.Bold))))
    )

    CaretStyle.forRichText(toggled, at(1)) shouldBe CaretStyle(bold = true)
  }

  it should "show plain when the toggle switched the surrounding style off" in {
    val buffer = proseBuffer(run("ab", InlineMark.Italic))
    val toggled = buffer.copy(richText = buffer.richText.copy(insertionRichTextStyle = Some(RichTextStyle.empty)))

    CaretStyle.forRichText(toggled, at(1)) shouldBe CaretStyle.Plain
  }

  it should "be plain when the stored document no longer matches the buffer" in {
    val buffer = proseBuffer(run("ab", InlineMark.Italic))
    val stale  = buffer.copy(richText = buffer.richText.copy(richTextSyncedVersion = None))

    CaretStyle.forRichText(stale, at(1)) shouldBe CaretStyle.Plain
  }

  "CaretStyle.forMarkdownRuns" should "show emphasis and strikethrough of the text before the caret" in {
    val runs = MarkdownInlineSpans.scan("a ~~gone~~ **b** _c_ d")

    CaretStyle.forMarkdownRuns(runs, 6) shouldBe CaretStyle(strikethrough = true)
    CaretStyle.forMarkdownRuns(runs, 13) shouldBe CaretStyle(bold = true)
    CaretStyle.forMarkdownRuns(runs, 18) shouldBe CaretStyle(italic = true)
    CaretStyle.forMarkdownRuns(runs, 1) shouldBe CaretStyle.Plain
  }

  it should "ignore the marker characters and plain text" in {
    val runs = Vector(Run(0, 2, Style(bold = false, italic = false, code = false), isMarker = true))

    CaretStyle.forMarkdownRuns(runs, 1) shouldBe CaretStyle.Plain
    CaretStyle.forMarkdownRuns(Vector.empty, 0) shouldBe CaretStyle.Plain
  }

  "CaretStyle.isPlain" should "hold only when no flag is set" in {
    CaretStyle.Plain.isPlain shouldBe true
    CaretStyle(strikethrough = true).isPlain shouldBe false
  }
end CaretStyleSpec
