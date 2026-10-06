package com.serenity.manuscript

import com.serenity.richtext.{
  InlineMark,
  ParagraphAlignment,
  ParagraphRole,
  RichTextDocument,
  RichTextParagraph,
  RichTextRun,
  RichTextStyle
}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class RichTextManuscriptSpec extends AnyFlatSpec with Matchers:

  private def heading(text: String, level: Int = 1): RichTextParagraph =
    RichTextParagraph.plain(text, role = ParagraphRole.Heading(level))

  private def body(text: String): RichTextParagraph =
    RichTextParagraph.plain(text)

  private def paragraph(text: String, kind: ParagraphKind = ParagraphKind.Body): Block =
    Block.Paragraph(List(RichTextRun(text)), kind)

  "RichTextManuscript" should "map Heading(1) paragraphs to chapters and drop blank paragraphs" in {
    val document = RichTextDocument(List(heading("One"), body("First."), body(""), body("Second."), heading("Two")))

    RichTextManuscript.sections(document, SectionRules.default) shouldBe Vector(
      Section.Chapter(Some(SectionHeading.of("One")), Vector(paragraph("First."), paragraph("Second."))),
      Section.Chapter(Some(SectionHeading.of("Two")), Vector.empty)
    )
  }

  it should "read a centred # and the asterisk spellings as scene breaks" in {
    val document = RichTextDocument(
      List(
        body("A."),
        RichTextParagraph.plain("#", alignment = ParagraphAlignment.Center),
        body("B."),
        body("* * *"),
        body("C."),
        body("  ***  "),
        body("D.")
      )
    )

    RichTextManuscript.sections(document, SectionRules.default) shouldBe Vector(
      Section.Chapter(
        None,
        Vector(
          paragraph("A."),
          Block.SceneBreak,
          paragraph("B."),
          Block.SceneBreak,
          paragraph("C."),
          Block.SceneBreak,
          paragraph("D.")
        )
      )
    )
  }

  it should "keep runs and their marks, and centred text as centred paragraphs" in {
    val italic = RichTextRun("quietly", RichTextStyle(marks = Set(InlineMark.Italic)))
    val document = RichTextDocument(
      List(
        RichTextParagraph(List(RichTextRun("She left "), italic)),
        RichTextParagraph.plain("Epigraph", alignment = ParagraphAlignment.Center)
      )
    )

    RichTextManuscript.sections(document, SectionRules.default) shouldBe Vector(
      Section.Chapter(
        None,
        Vector(
          Block.Paragraph(List(RichTextRun("She left "), italic), ParagraphKind.Body),
          paragraph("Epigraph", ParagraphKind.Centered)
        )
      )
    )
  }

  it should "use the configured part and chapter levels" in {
    val document = RichTextDocument(List(heading("Part"), heading("Chapter", 2), body("Text.")))
    val rules    = SectionRules(Some(1), 2, SectionRules.DefaultSceneBreakPatterns)

    RichTextManuscript.sections(document, rules) shouldBe Vector(
      Section.Part(
        Some(SectionHeading.of("Part")),
        Vector(Section.Chapter(Some(SectionHeading.of("Chapter")), Vector(paragraph("Text."))))
      )
    )
  }
