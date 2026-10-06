package com.serenity.manuscript

import com.serenity.richtext.{InlineMark, RichTextRun, RichTextStyle}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.TableDrivenPropertyChecks

class MarkdownManuscriptSpec extends AnyFlatSpec with Matchers with TableDrivenPropertyChecks:

  private val partsAtOne = SectionRules(Some(1), 2, SectionRules.DefaultSceneBreakPatterns)

  private def paragraph(text: String, kind: ParagraphKind = ParagraphKind.Body): Block =
    Block.Paragraph(List(RichTextRun(text)), kind)

  private def chapter(title: String, blocks: Block*): Section =
    Section.Chapter(Some(SectionHeading.of(title)), blocks.toVector)

  "MarkdownManuscript" should "open a chapter at each top-level heading by default" in {
    MarkdownManuscript.sections("# One\n\nFirst.\n\n# Two\n\nSecond.", SectionRules.default) shouldBe Vector(
      chapter("One", paragraph("First.")),
      chapter("Two", paragraph("Second."))
    )
  }

  it should "group chapters into parts when a part level is set" in {
    val markdown = "# Book One\n\n## Arrival\n\nText.\n\n## Departure\n\nMore.\n\n# Book Two\n\n## Return\n\nEnd."

    MarkdownManuscript.sections(markdown, partsAtOne) shouldBe Vector(
      Section.Part(
        Some(SectionHeading.of("Book One")),
        Vector(chapter("Arrival", paragraph("Text.")), chapter("Departure", paragraph("More.")))
      ),
      Section.Part(Some(SectionHeading.of("Book Two")), Vector(chapter("Return", paragraph("End."))))
    )
  }

  it should "keep headings deeper than the chapter level inside the chapter as centred paragraphs" in {
    MarkdownManuscript.sections("# One\n\n### Interlude\n\nText.", SectionRules.default) shouldBe Vector(
      chapter("One", paragraph("Interlude", ParagraphKind.Centered), paragraph("Text."))
    )
  }

  it should "put text before the first heading in a chapter without a heading" in {
    MarkdownManuscript.sections("Prologue text.\n\n# One\n\nText.", SectionRules.default) shouldBe Vector(
      Section.Chapter(None, Vector(paragraph("Prologue text."))),
      chapter("One", paragraph("Text."))
    )
  }

  private val sceneBreakSpellings = Table(
    "separator",
    "***",
    "* * *",
    "---",
    "#",
    "\\#"
  )

  it should "read every scene-break spelling as a scene break" in
    forAll(sceneBreakSpellings) { separator =>
      MarkdownManuscript.sections(s"# One\n\nBefore.\n\n$separator\n\nAfter.", SectionRules.default) shouldBe Vector(
        chapter("One", paragraph("Before."), Block.SceneBreak, paragraph("After."))
      )
    }

  it should "carry emphasis, strong emphasis and Serenity's underline tags as marks" in {
    val blocks = MarkdownManuscript.sections("She *was* **there** <u>then</u>.", SectionRules.default)

    blocks shouldBe Vector(
      Section.Chapter(
        None,
        Vector(
          Block.Paragraph(
            List(
              RichTextRun("She "),
              RichTextRun("was", RichTextStyle(marks = Set(InlineMark.Italic))),
              RichTextRun(" "),
              RichTextRun("there", RichTextStyle(marks = Set(InlineMark.Bold))),
              RichTextRun(" "),
              RichTextRun("then", RichTextStyle(marks = Set(InlineMark.Underline))),
              RichTextRun(".")
            ),
            ParagraphKind.Body
          )
        )
      )
    )
  }

  it should "join soft-wrapped lines with a space and keep hard breaks" in {
    MarkdownManuscript.sections("one\ntwo  \nthree", SectionRules.default) shouldBe Vector(
      Section.Chapter(None, Vector(paragraph("one two\nthree")))
    )
  }

  it should "map block quotes to block-quote paragraphs and code to preformatted lines" in {
    MarkdownManuscript.sections("> Quoted.\n\n```\nlet # = 1\nnext\n```", SectionRules.default) shouldBe Vector(
      Section.Chapter(
        None,
        Vector(paragraph("Quoted.", ParagraphKind.BlockQuote), Block.Preformatted(Vector("let # = 1", "next")))
      )
    )
  }

  it should "mark inline code with the code family so transforms can skip it" in {
    MarkdownManuscript.sections("Run `\"x\"` now.", SectionRules.default) shouldBe Vector(
      Section.Chapter(
        None,
        Vector(
          Block.Paragraph(
            List(
              RichTextRun("Run "),
              RichTextRun("\"x\"", RichTextStyle(fontFamily = Some(MarkdownManuscript.CodeFontFamily))),
              RichTextRun(" now.")
            ),
            ParagraphKind.Body
          )
        )
      )
    )
  }

  it should "keep list markers as text and drop HTML blocks" in {
    MarkdownManuscript.sections("- red\n- blue\n\n<!-- note to self -->\n\n3. three", SectionRules.default) shouldBe
      Vector(Section.Chapter(None, Vector(paragraph("• red"), paragraph("• blue"), paragraph("3. three"))))
  }
