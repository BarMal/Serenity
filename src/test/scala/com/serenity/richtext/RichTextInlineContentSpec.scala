package com.serenity.richtext

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Inline content that is not plain styled text: soft-break atoms and links. */
class RichTextInlineContentSpec extends AnyFlatSpec with Matchers:

  "A soft-break atom" should "occupy one character, never merge with neighbours, and survive formatting" in {
    val paragraph = RichTextParagraph(
      List(RichTextRun("ab"), RichTextRun.softBreak(), RichTextRun.softBreak(), RichTextRun("cd"))
    )
    val document = RichTextDocument(List(paragraph))

    document.normalized.paragraphs.head.runs.size shouldBe 4
    document.plainText shouldBe "ab\uFFFC\uFFFCcd"
    document.exportText shouldBe "ab\n\ncd"

    val bolded = document.toggleMark(
      RichTextRange(RichTextPosition(0, 1), RichTextPosition(0, 5)),
      InlineMark.Bold
    )
    bolded.paragraphs.head.runs.map(_.text) shouldBe List("a", "b", "\uFFFC", "\uFFFC", "c", "d")
    bolded.paragraphs.head.runs.map(_.atom) shouldBe
      List(None, None, Some(InlineAtom.SoftBreak), Some(InlineAtom.SoftBreak), None, None)
  }

  it should "be removed by deleting its character and not inherited by typed text" in {
    val document = RichTextDocument(List(RichTextParagraph(List(RichTextRun("ab"), RichTextRun.softBreak()))))

    val typed = document.replaceRange(RichTextRange(RichTextPosition(0, 3), RichTextPosition(0, 3)), "x")
    typed.paragraphs.head.runs.map(_.atom) shouldBe List(None, Some(InlineAtom.SoftBreak), None)

    val deleted = document.replaceRange(RichTextRange(RichTextPosition(0, 2), RichTextPosition(0, 3)), "")
    deleted.paragraphs.head.runs shouldBe List(RichTextRun("ab"))
  }

  it should "count as formatting so a plain-text save is warned about it" in {
    RichTextDocument(
      List(RichTextParagraph(List(RichTextRun("a"), RichTextRun.softBreak())))
    ).hasFormatting shouldBe true
  }

  "A link" should "be continued by typing inside it but not at either edge" in {
    val link = RichTextStyle.empty.withLink("https://example.com")
    val document = RichTextDocument(
      List(RichTextParagraph(List(RichTextRun("ab"), RichTextRun("link", link), RichTextRun("cd"))))
    )

    def typedAt(offset: Int): List[RichTextRun] =
      document
        .replaceRange(RichTextRange(RichTextPosition(0, offset), RichTextPosition(0, offset)), "X")
        .paragraphs
        .head
        .runs

    typedAt(2).map(_.style.link) shouldBe List(None, Some("https://example.com"), None)
    typedAt(4).map(_.text) shouldBe List("ab", "liXnk", "cd")
    typedAt(6).map(_.style.link) shouldBe List(None, Some("https://example.com"), None)
    typedAt(6).map(_.text) shouldBe List("ab", "link", "Xcd")
    document.hasFormatting shouldBe true
  }
