package com.serenity.exporting

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import com.serenity.manuscript.layout.{PagedDocument, PaginationError, Paginator}
import com.serenity.manuscript.typography.PageTypography
import com.serenity.manuscript.{
  AuthorName,
  Block,
  Manuscript,
  ManuscriptMeta,
  ParagraphKind,
  Section,
  SectionHeading,
  WordCountRounding
}
import com.serenity.richtext.RichTextRun
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.{EitherValues, OptionValues}

/** The paginator over real Courier Prime metrics: monospace, so line counts are plain arithmetic. */
class PaginatorFontBoxSpec extends AnyFlatSpec with Matchers with EitherValues with OptionValues:

  private val typography = PageTypography.StandardManuscriptCourier

  private val meta = ManuscriptMeta(
    "Title",
    "TITLE",
    AuthorName.fromLegal("Jane Writer"),
    "Jane Writer",
    Nil,
    100,
    WordCountRounding.Exact
  )

  private def paginatedBlocks(blocks: Block*): Either[PaginationError, PagedDocument] =
    val manuscript = Manuscript(
      meta,
      Nil,
      Vector(Section.Chapter(Some(SectionHeading.of("One")), blocks.toVector)),
      None
    )
    FontBoxTextMeasurer
      .resource()
      .use(measurer => IO(Paginator.paginate(manuscript, typography, measurer)))
      .unsafeRunSync()

  private def paginated(text: String): PagedDocument =
    paginatedBlocks(body(text)).value

  private def body(text: String, kind: ParagraphKind = ParagraphKind.Body): Block =
    Block.Paragraph(List(RichTextRun(text)), kind)

  private def lineTexts(doc: PagedDocument): Vector[String] =
    doc.pages.flatMap(_.lines).drop(1).map(_.text)

  "paginating with Courier Prime at 12 pt" should "set 7.2 points a character: 60 on the indented first line, then 65" in {
    val words = Vector.fill(40)("abcd").mkString(" ")

    val lines = paginated(words).pages.flatMap(_.lines).drop(1)

    lines.map(_.text.stripTrailing.split(" ").length) shouldBe Vector(12, 13, 13, 2)
    lines.map(_.text).mkString shouldBe words
  }

  it should "hold the first line to 432 points and later lines to the 468-point measure" in {
    val lines = paginated(Vector.fill(40)("abcd").mkString(" ")).pages.flatMap(_.lines).drop(1)

    lines.flatMap(_.runs.lastOption).map(run => run.x + run.text.stripTrailing.length * 7.2f).foreach { right =>
      right should be <= 72f + 468f + 0.05f
    }
    lines.headOption.flatMap(_.runs.headOption).map(_.x) shouldBe Some(108f)
  }

  it should "lay lines on the double-spaced pitch of the face's own metrics" in {
    val lines = paginated(Vector.fill(40)("abcd").mkString(" ")).pages.flatMap(_.lines).drop(1)

    val gaps  = lines.zip(lines.drop(1)).map((above, below) => below.baselineY - above.baselineY)
    val first = gaps.headOption.value
    gaps.foreach(_ shouldBe (first +- 0.01f))
    first should be > 12f * 2f
  }

  "text a font has no glyph for" should "end the line at a soft or hard line break" in {
    val doc = paginatedBlocks(body("alpha\nbeta\u2028gamma\u000Bdelta")).value

    lineTexts(doc) shouldBe Vector("alpha", "beta", "gamma", "delta")
  }

  it should "indent only the first of the lines a break makes" in {
    val doc = paginatedBlocks(body("alpha\nbeta")).value

    doc.pages.flatMap(_.lines).drop(1).flatMap(_.runs.headOption).map(_.x) shouldBe Vector(108f, 72f)
  }

  it should "keep an intentional blank line, and ignore a break that ends the paragraph" in {
    lineTexts(paginatedBlocks(body("a\n\nb\n")).value) shouldBe Vector("a", "", "b")
  }

  it should "turn a leading tab into the first-line indent" in {
    val doc = paginatedBlocks(body("\tquoted", ParagraphKind.BlockQuote), body("\tplain")).value

    val lines = doc.pages.flatMap(_.lines).drop(1)
    lines.map(_.text) shouldBe Vector("quoted", "plain")
    lines.flatMap(_.runs.headOption).map(_.x) shouldBe Vector(72f + 36f + 36f, 108f)
  }

  it should "turn any other tab into a single space" in {
    lineTexts(paginatedBlocks(body("left\tright")).value) shouldBe Vector("left right")
  }

  it should "drop control and format characters before measuring" in {
    val doc = paginatedBlocks(body("a\u0000b\u0007c\r\nd\u200Be\u00ADf\u007F")).value

    lineTexts(doc) shouldBe Vector("abc", "def")
  }

  it should "not fail an export however the characters are mixed" in {
    val messy = "Tab\there,\u0001 break\nand\u200D more\u2028done\t"

    paginatedBlocks(body(messy), Block.Preformatted(Vector("\tcode\tline\u0000"))).isRight shouldBe true
  }

  it should "still report a printable character the face lacks" in {
    paginatedBlocks(body("a\u4e2d")).isLeft shouldBe true
  }

  "tabs in code" should "expand to the next multiple-of-4 column, so two levels of indentation survive" in {
    val code = Block.Preformatted(Vector("top", "\tone", "\t\ttwo\tx", "\t\treturn\ty"))

    val lines = paginatedBlocks(code).value.pages.flatMap(_.lines).drop(1)

    lines.map(_.text) shouldBe Vector("top", "    one", "        two x", "        return  y")
    lines.flatMap(_.runs.headOption).map(_.x).distinct shouldBe Vector(72f)
  }

  it should "keep the indentation width in points: eight columns at 7.2 points a character" in {
    val line = paginatedBlocks(Block.Preformatted(Vector("\t\tx"))).value.pages.flatMap(_.lines).drop(1)

    line.map(_.text.length) shouldBe Vector(9)
    line.flatMap(_.runs).map(_.text) shouldBe Vector("        x")
  }
