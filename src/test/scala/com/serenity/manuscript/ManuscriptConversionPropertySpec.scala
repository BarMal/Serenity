package com.serenity.manuscript

import com.serenity.richtext.{ParagraphRole, RichTextDocument, RichTextParagraph}
import org.scalacheck.Gen
import org.scalatest.matchers.should.Matchers
import org.scalatest.propspec.AnyPropSpec
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

/** #1206 round-trip safety: with no transforms, conversion keeps every word of prose and every heading, and never
  * touches the source.
  */
class ManuscriptConversionPropertySpec extends AnyPropSpec with ScalaCheckPropertyChecks with Matchers:

  private val word: Gen[String] = Gen.nonEmptyListOf(Gen.alphaNumChar).map(_.take(8).mkString)

  private val sentence: Gen[String] = Gen.choose(1, 6).flatMap(Gen.listOfN(_, word)).map(_.mkString(" "))

  private enum SourceBlock:
    case Heading(title: String)
    case Prose(text: String)

  private val blocks: Gen[List[SourceBlock]] =
    Gen.listOf(Gen.frequency(1 -> sentence.map(SourceBlock.Heading(_)), 4 -> sentence.map(SourceBlock.Prose(_))))

  private def proseOf(source: List[SourceBlock]): Vector[String] =
    source.collect { case SourceBlock.Prose(text) => text }.toVector

  private def headingsOf(source: List[SourceBlock]): Vector[String] =
    source.collect { case SourceBlock.Heading(title) => title }.toVector

  private def headingTitles(sections: Vector[Section]): Vector[String] =
    ManuscriptText.chapters(sections).flatMap(_.heading.map(_.title))

  property("Markdown conversion keeps every paragraph's text and every heading, in order") {
    forAll(blocks) { source =>
      val markdown = source
        .map {
          case SourceBlock.Heading(title) => s"# $title"
          case SourceBlock.Prose(text)    => text
        }
        .mkString("\n\n")

      val sections = MarkdownManuscript.sections(markdown, SectionRules.default)

      ManuscriptText.bodyTexts(sections) shouldBe proseOf(source)
      headingTitles(sections) shouldBe headingsOf(source)
    }
  }

  property("rich-text conversion keeps every paragraph's text and every heading, and leaves the source unchanged") {
    forAll(blocks) { source =>
      val paragraphs = source.flatMap {
        case SourceBlock.Heading(title) => List(RichTextParagraph.plain(title, role = ParagraphRole.Heading(1)))
        case SourceBlock.Prose(text)    => List(RichTextParagraph.plain(text), RichTextParagraph.plain(""))
      }
      val document = RichTextDocument(paragraphs)

      val sections = RichTextManuscript.sections(document, SectionRules.default)

      ManuscriptText.bodyTexts(sections) shouldBe proseOf(source)
      headingTitles(sections) shouldBe headingsOf(source)
      document.paragraphs shouldBe paragraphs
    }
  }
