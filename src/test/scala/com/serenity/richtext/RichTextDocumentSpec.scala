package com.serenity.richtext

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class RichTextDocumentSpec extends AnyFlatSpec with Matchers:

  "RichTextDocument" should "render plain text by joining paragraphs with newlines" in {
    val document = RichTextDocument(
      List(
        RichTextParagraph.plain("First paragraph"),
        RichTextParagraph.plain("Second paragraph")
      )
    )

    document.plainText shouldBe "First paragraph\nSecond paragraph"
  }

  it should "build plain rich text documents from newline-separated text" in {
    val document = RichTextDocument.fromPlainText("First\n\nThird")

    document.paragraphs shouldBe List(
      RichTextParagraph.plain("First"),
      RichTextParagraph.plain(""),
      RichTextParagraph.plain("Third")
    )
    document.plainText shouldBe "First\n\nThird"
  }

  it should "normalize empty runs and merge adjacent runs with the same style" in {
    val bold = RichTextStyle(marks = Set(InlineMark.Bold))
    val document = RichTextDocument(
      List(
        RichTextParagraph(
          runs = List(
            RichTextRun("Hello", bold),
            RichTextRun("", bold),
            RichTextRun(" world", bold),
            RichTextRun("!", RichTextStyle.empty)
          )
        )
      )
    )

    document.normalized.paragraphs.head.runs shouldBe List(
      RichTextRun("Hello world", bold),
      RichTextRun("!", RichTextStyle.empty)
    )
  }

  it should "apply an inline mark across a single paragraph range" in {
    val document = RichTextDocument.oneParagraph("alpha beta gamma")
    val marked = document.applyMark(
      RichTextRange(
        start = RichTextPosition(paragraphIndex = 0, offset = 6),
        end = RichTextPosition(paragraphIndex = 0, offset = 10)
      ),
      InlineMark.Italic
    )

    marked.paragraphs.head.runs shouldBe List(
      RichTextRun("alpha ", RichTextStyle.empty),
      RichTextRun("beta", RichTextStyle(marks = Set(InlineMark.Italic))),
      RichTextRun(" gamma", RichTextStyle.empty)
    )
  }

  it should "apply an inline mark across multiple paragraph ranges" in {
    val document = RichTextDocument(
      List(
        RichTextParagraph.plain("alpha beta"),
        RichTextParagraph.plain("gamma delta")
      )
    )

    val marked = document.applyMark(
      RichTextRange(
        start = RichTextPosition(paragraphIndex = 0, offset = 6),
        end = RichTextPosition(paragraphIndex = 1, offset = 5)
      ),
      InlineMark.Underline
    )

    marked.paragraphs shouldBe List(
      RichTextParagraph(
        runs = List(
          RichTextRun("alpha ", RichTextStyle.empty),
          RichTextRun("beta", RichTextStyle(marks = Set(InlineMark.Underline)))
        )
      ),
      RichTextParagraph(
        runs = List(
          RichTextRun("gamma", RichTextStyle(marks = Set(InlineMark.Underline))),
          RichTextRun(" delta", RichTextStyle.empty)
        )
      )
    )
  }

  it should "preserve paragraph-level alignment independently from inline marks" in {
    val document = RichTextDocument(
      List(
        RichTextParagraph.plain("Centered", alignment = ParagraphAlignment.Center)
      )
    )

    val marked = document.applyMark(
      RichTextRange(RichTextPosition(0, 0), RichTextPosition(0, 8)),
      InlineMark.Bold
    )

    marked.paragraphs.head.alignment shouldBe ParagraphAlignment.Center
    marked.paragraphs.head.runs.head.style.marks shouldBe Set(InlineMark.Bold)
  }
