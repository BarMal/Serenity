package com.serenity.manuscript.layout

import com.serenity.manuscript.layout.PaginatorFixture.*
import com.serenity.manuscript.{Block, ManuscriptText, ParagraphKind, Section}
import com.serenity.richtext.RichTextRun
import org.scalacheck.Gen
import org.scalatest.matchers.should.Matchers
import org.scalatest.propspec.AnyPropSpec
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

/** Laws that hold for any manuscript, whatever the text, checked against the fixed-advance measurer. */
class PaginatorPropertySpec extends AnyPropSpec with ScalaCheckPropertyChecks with Matchers:

  private val Epsilon = 0.01f

  private val word: Gen[String] =
    Gen.choose(1, 12).flatMap(Gen.listOfN(_, Gen.alphaNumChar)).map(_.mkString)

  private val text: Gen[String] =
    Gen.choose(1, 70).flatMap(Gen.listOfN(_, word)).map(_.mkString(" "))

  private val paragraph: Gen[Block] =
    for
      body <- text
      kind <- Gen.frequency(
        8 -> Gen.const(ParagraphKind.Body),
        1 -> Gen.const(ParagraphKind.BlockQuote),
        1 -> Gen.const(ParagraphKind.Centered)
      )
    yield Block.Paragraph(List(RichTextRun(body)), kind)

  private val block: Gen[Block] = Gen.frequency(7 -> paragraph, 2 -> Gen.const(Block.SceneBreak))

  /** A chapter always ends in prose, so a scene break always has text to move with. */
  private def chapterAt(number: Int): Gen[Section] =
    for
      blocks <- Gen.choose(0, 8).flatMap(Gen.listOfN(_, block))
      last   <- paragraph
    yield chapter(s"Chapter $number", (blocks :+ last)*)

  private val sections: Gen[Vector[Section]] =
    Gen.choose(1, 4).flatMap(count => Gen.sequence[Vector[Section], Section]((1 to count).map(chapterAt)))

  private def irText(sections: Vector[Section]): String =
    ManuscriptText
      .chapters(sections)
      .flatMap(chapter =>
        chapter.heading.map(_.title).toVector ++ chapter.blocks.map {
          case Block.Paragraph(runs, _)  => ManuscriptText.plainText(runs)
          case Block.SceneBreak          => typography.sceneBreak
          case Block.Preformatted(lines) => lines.mkString
        }
      )
      .mkString

  property("no line is wider than the measure") {
    forAll(sections) { generated =>
      val doc = paginate(manuscript(generated))

      doc.pages.flatMap(_.lines).foreach { line =>
        line.runs.headOption.foreach(_.x should be >= typography.margins.left - Epsilon)
        visibleRight(line) should be <= typography.margins.left + typography.textWidth + Epsilon
      }
    }
  }

  property("no line leaves the text area vertically") {
    forAll(sections) { generated =>
      paginate(manuscript(generated)).pages.flatMap(_.lines).foreach { line =>
        line.baselineY should be <= typography.margins.top + typography.textHeight + Epsilon
      }
    }
  }

  property("the concatenated page text equals the text of the manuscript") {
    forAll(sections) { generated =>
      paginate(manuscript(generated)).pages.flatMap(_.lines).map(_.text).mkString shouldBe irText(generated)
    }
  }

  property("page numbers are contiguous from 1") {
    forAll(sections) { generated =>
      val numbers = paginate(manuscript(generated)).pages.map(_.number)

      numbers shouldBe (1 to numbers.size).toVector
    }
  }

  property("every chapter starts a new page, headed by its title, and nothing else does") {
    forAll(sections) { generated =>
      val doc    = paginate(manuscript(generated))
      val starts = doc.pages.filter(_.kind == PageKind.SectionStart)

      starts.flatMap(_.lines.headOption).map(_.text) shouldBe
        ManuscriptText.chapters(generated).flatMap(_.heading).map(_.title)
    }
  }

  property("running heads are on body pages only, numbered with the page") {
    forAll(sections) { generated =>
      paginate(manuscript(generated)).pages.foreach { page =>
        page.head.map(_.text) shouldBe Option.when(page.kind == PageKind.Body)(s"Writer / NIGHT / ${page.number}")
      }
    }
  }

  property("a scene break never ends a page") {
    forAll(sections) { generated =>
      paginate(manuscript(generated)).pages
        .flatMap(_.lines.lastOption)
        .map(_.text) should not contain typography.sceneBreak
    }
  }

  property("baselines rise strictly down each page") {
    forAll(sections) { generated =>
      paginate(manuscript(generated)).pages.foreach { page =>
        val baselines = page.lines.map(_.baselineY)
        baselines.zip(baselines.drop(1)).foreach((above, below) => below should be > above)
      }
    }
  }
