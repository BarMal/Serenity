package com.serenity.exporting

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import com.serenity.manuscript.layout.{PagedDocument, Paginator}
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

  private def paginated(text: String): PagedDocument =
    val manuscript = Manuscript(
      meta,
      Nil,
      Vector(
        Section.Chapter(
          Some(SectionHeading.of("One")),
          Vector(Block.Paragraph(List(RichTextRun(text)), ParagraphKind.Body))
        )
      ),
      None
    )
    FontBoxTextMeasurer
      .resource()
      .use(measurer => IO(Paginator.paginate(manuscript, typography, measurer)))
      .unsafeRunSync()
      .value

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
