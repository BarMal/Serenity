package com.serenity.richtext

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class RichTextDocumentSharingSpec extends AnyFlatSpec with Matchers:

  "RichTextDocument edits" should "leave every paragraph but the typed one reference-equal and stay normalised" in {
    val document = RichTextDocument((0 until 3_000).map(index => RichTextParagraph.plain(s"paragraph $index")).toList)
    val typed =
      document.replaceRange(RichTextRange(RichTextPosition(1_500, 4), RichTextPosition(1_500, 4)), "x")

    typed.paragraphAt(1_500).map(_.plainText) shouldBe Some("paraxgraph 1500")
    (0 until 3_000).filterNot(_ == 1_500).foreach { index =>
      withClue(s"paragraph $index: ")(
        (typed.paragraphAt(index).orNull eq document.paragraphAt(index).orNull) shouldBe true
      )
    }
    (typed.normalized eq typed) shouldBe true
    typed.paragraphCount shouldBe 3_000
  }

  it should "equal a document built from the same paragraphs and differ when one paragraph differs" in {
    val paragraphs = (0 until 800).map(index => RichTextParagraph.plain(s"paragraph $index")).toList
    val document   = RichTextDocument(paragraphs)
    val typed      = document.replaceRange(RichTextRange(RichTextPosition(400, 0), RichTextPosition(400, 0)), "x")
    val untyped    = typed.replaceRange(RichTextRange(RichTextPosition(400, 0), RichTextPosition(400, 1)), "")

    untyped shouldBe document
    untyped.hashCode shouldBe document.hashCode
    typed should not be document
    document should not be RichTextDocument(paragraphs.take(799))
  }

  it should "list heading paragraphs with their indices" in {
    val document = RichTextDocument(
      List(
        RichTextParagraph.plain("Title", role = ParagraphRole.Heading(1)),
        RichTextParagraph.plain("body"),
        RichTextParagraph.plain("  ", role = ParagraphRole.Heading(2)),
        RichTextParagraph.plain("Part", role = ParagraphRole.Heading(2))
      )
    )

    document.headingParagraphs.map((index, paragraph) => index -> paragraph.plainText) shouldBe
      List(0 -> "Title", 3 -> "Part")
    val retitled = document.setParagraphRole(
      RichTextRange(RichTextPosition(1, 0), RichTextPosition(1, 0)),
      ParagraphRole.Heading(3)
    )
    retitled.headingParagraphs.map(_._1) shouldBe List(0, 1, 3)
  }
