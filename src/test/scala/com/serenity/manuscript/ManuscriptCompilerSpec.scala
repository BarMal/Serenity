package com.serenity.manuscript

import com.serenity.richtext.{
  InlineMark,
  ParagraphRole,
  RichTextDocument,
  RichTextParagraph,
  RichTextRun,
  RichTextStyle
}
import org.scalatest.EitherValues
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ManuscriptCompilerSpec extends AnyFlatSpec with Matchers with EitherValues:

  private val plain = CompileSpec.forTitle("The Long Night").copy(transforms = Nil, endMarker = None)

  private def compiled(spec: CompileSpec, markdown: String*): Manuscript =
    ManuscriptCompiler.compile(spec, markdown.toList.map(SourceDocument.Markdown(_))).value

  private def headings(manuscript: Manuscript): Vector[Vector[String]] =
    manuscript.body.flatMap {
      case Section.Part(heading, nested) =>
        heading.map(_.lines).toVector ++ ManuscriptText.chapters(nested).flatMap(_.heading.map(_.lines))
      case chapter: Section.Chapter => chapter.heading.map(_.lines).toVector
    }

  private def paragraphRuns(manuscript: Manuscript): Vector[List[RichTextRun]] =
    ManuscriptText.chapters(manuscript.body).flatMap(_.blocks.collect { case Block.Paragraph(runs, _) => runs })

  "ManuscriptCompiler" should "number chapters straight through the sources and across parts" in {
    val spec = plain.copy(
      rules = SectionRules(Some(1), 2, SectionRules.DefaultSceneBreakPatterns),
      chapterHeading = HeadingTemplate("Chapter <$n>\n<$t>"),
      partHeading = HeadingTemplate("Part <$R>")
    )

    val manuscript = compiled(spec, "# One\n\n## Arrival\n\nA.\n\n## Storm\n\nB.", "# Two\n\n## Return\n\nC.")

    headings(manuscript) shouldBe Vector(
      Vector("Part I"),
      Vector("Chapter 1", "Arrival"),
      Vector("Chapter 2", "Storm"),
      Vector("Part II"),
      Vector("Chapter 3", "Return")
    )
  }

  it should "start each source without headings as its own numbered chapter" in {
    val manuscript = compiled(plain.copy(chapterHeading = HeadingTemplate("<$n>")), "First file.", "Second file.")

    headings(manuscript) shouldBe Vector(Vector("1"), Vector("2"))
  }

  it should "fill the title-page fields from the spec and count the words" in {
    val spec = plain.copy(
      author = AuthorName.fromLegal("Jane Q. Writer"),
      contact = List("1 High Street"),
      dedication = Some("For M."),
      endMarker = Some("END")
    )

    val manuscript = compiled(spec, "# One\n\nThree short words.\n\n# Two\n\nTwo more.")

    manuscript.meta shouldBe ManuscriptMeta(
      title = "The Long Night",
      shortTitle = "THE LONG NIGHT",
      author = AuthorName("Jane Q. Writer", "Writer"),
      byline = "Jane Q. Writer",
      contact = List("1 High Street"),
      wordCount = 5,
      wordCountRounding = WordCountRounding.Novel
    )
    manuscript.front shouldBe List(FrontMatter.TitlePage, FrontMatter.Dedication("For M."))
    manuscript.endMarker shouldBe Some("END")
  }

  it should "carry the e-book language and identifier into the meta" in {
    val manuscript =
      compiled(plain.copy(language = "de", identifier = Some("urn:uuid:1b4e28ba-2fa1-11d2-883f-0016d3cca427")), "# One")

    (manuscript.meta.language, manuscript.meta.identifier) shouldBe
      ("de", Some("urn:uuid:1b4e28ba-2fa1-11d2-883f-0016d3cca427"))
  }

  it should "educate punctuation across run boundaries but not inside code" in {
    val manuscript = compiled(plain.copy(transforms = List(TextTransform.SmartPunctuation)), "\"*Now*\" -- `\"x\"`")

    paragraphRuns(manuscript) shouldBe Vector(
      List(
        RichTextRun("“"),
        RichTextRun("Now", RichTextStyle(marks = Set(InlineMark.Italic))),
        RichTextRun("” – "),
        RichTextRun("\"x\"", RichTextStyle(fontFamily = Some(MarkdownManuscript.CodeFontFamily)))
      )
    )
  }

  it should "apply the classic transforms: straight quotes, -- for an em dash, italics underlined" in {
    val classic = plain.copy(transforms = ManuscriptFormat.Classic.defaultTransforms)
    val source = RichTextDocument(
      List(
        RichTextParagraph(
          List(RichTextRun("“Go—now,” "), RichTextRun("she", RichTextStyle(marks = Set(InlineMark.Italic))))
        )
      )
    )

    val manuscript = ManuscriptCompiler.compile(classic, List(SourceDocument.Rich(source))).value

    paragraphRuns(manuscript) shouldBe Vector(
      List(RichTextRun("\"Go--now,\" "), RichTextRun("she", RichTextStyle(marks = Set(InlineMark.Underline))))
    )
  }

  it should "apply literal and regex replacements to prose only" in {
    val spec = plain.copy(
      replacements =
        List(Replacement("Bob", "Robert", regex = false), Replacement("""\bcolou?r\b""", "hue", regex = true))
    )

    val manuscript = compiled(spec, "Bob saw the colour. `Bob`")

    paragraphRuns(manuscript).map(ManuscriptText.plainText) shouldBe Vector("Robert saw the hue. Bob")
  }

  it should "refuse an invalid regular expression" in {
    val spec = plain.copy(replacements = List(Replacement("(", "x", regex = true)))

    ManuscriptCompiler
      .compile(spec, List(SourceDocument.Markdown("Text.")))
      .left
      .value shouldBe a[CompileError.InvalidReplacement]
  }

  it should "refuse to compile when there is no text" in {
    ManuscriptCompiler
      .compile(plain, List(SourceDocument.Markdown("  \n")))
      .left
      .value shouldBe CompileError.NothingToCompile
  }

  it should "leave the source document unchanged" in {
    val paragraphs =
      List(RichTextParagraph.plain("One", role = ParagraphRole.Heading(1)), RichTextParagraph.plain("\"Hi\""))
    val source = RichTextDocument(paragraphs)

    ManuscriptCompiler.compile(CompileSpec.forTitle("T"), List(SourceDocument.Rich(source))).isRight shouldBe true
    source.paragraphs shouldBe paragraphs
  }
