package com.serenity.ui.openings

import java.util.concurrent.atomic.AtomicInteger

import com.serenity.config.MarkdownViewMode
import com.serenity.document.MarkdownHeadingIndex
import com.serenity.richtext.{ParagraphRole, RichTextDocument, RichTextParagraph, RichTextPosition, RichTextRange}
import com.serenity.rope.{Balance, Rope}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Updating the openings after an edit reads the lines around the edit, not the document, and measures the cost on a
  * 500-chapter book.
  */
class OpeningDetectionCostSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private val chapters = 500
  private val rules    = OpeningRules.default
  private val mode     = MarkdownViewMode.Read
  private val noMarks  = ChapterOpenings.noExplicitEpigraphs

  private val markdownBook: Vector[String] =
    (1 to chapters).flatMap(chapter => Seq(s"# Chapter $chapter", "", s"Opening $chapter.", "", "Body.", "")).toVector

  private val richBook: RichTextDocument =
    RichTextDocument(
      (1 to chapters).toList.flatMap { chapter =>
        List(
          RichTextParagraph.plain(s"Chapter $chapter", role = ParagraphRole.Heading(1)),
          RichTextParagraph(Nil),
          RichTextParagraph.plain(s"Opening $chapter."),
          RichTextParagraph.plain("Body.")
        )
      }
    )

  private def micros(nanos: Long): String = f"${nanos / 1000.0}%.1f us"

  private def median(samples: Seq[Long]): Long = samples.sorted.apply(samples.size / 2)

  private def timed[A](work: => A): (A, Long) =
    val start  = System.nanoTime()
    val result = work
    (result, System.nanoTime() - start)

  private def markdownEdit(line: Int, text: String): (MarkdownHeadingIndex, MarkdownHeadingIndex, LineEdit) =
    val before = MarkdownHeadingIndex.build(Rope(markdownBook.mkString("\n")))
    val after  = before.updatedTo(Rope(markdownBook.updated(line, text).mkString("\n")))
    val edit =
      LineEdit.between(before.content, after.content).covering(after.parsedFrom, after.parsedFrom + after.parsedLines)
    (before, after, edit)

  private def classifyReads(line: Int, text: String): Int =
    val (before, after, edit) = markdownEdit(line, text)
    val prior                 = ChapterOpenings.openingLines(before, mode, rules, noMarks)
    val reads                 = AtomicInteger(0)
    val classify              = MarkdownOpenings.classify(after, rules, noMarks)
    val counted = (at: Int) =>
      val _ = reads.incrementAndGet()
      classify(at)
    val updated =
      OpeningScan.rescan(prior, edit, after.content.lineCount, rules, counted, MarkdownOpenings.startsBlock(after))
    updated shouldBe ChapterOpenings.openingLines(after, mode, rules, noMarks)
    reads.get

  "Updating Markdown openings" should "read only the lines around a heading edit" in {
    classifyReads(1494, "# Chapter 250 renamed") should be < 12
  }

  it should "read only the lines around a typed character in an opening" in {
    classifyReads(1496, "Opening 250 and more.") should be < 12
  }

  it should "read only the lines around a removed heading" in {
    classifyReads(1494, "Chapter 250") should be < 12
  }

  "Updating rich-text openings" should "read only the paragraphs around a heading edit" in {
    val prior    = ChapterOpenings.openingLines(richBook, rules, noMarks)
    val edit     = LineEdit(998, 1, 1)
    val after    = richBook.replaceRange(RichTextRange(RichTextPosition(998, 0), RichTextPosition(998, 0)), "New ")
    val reads    = AtomicInteger(0)
    val classify = RichTextOpenings.classify(after, rules, noMarks)
    val counted = (at: Int) =>
      val _ = reads.incrementAndGet()
      classify(at)
    val updated = OpeningScan.rescan(prior, edit, after.paragraphCount, rules, counted)
    updated shouldBe ChapterOpenings.openingLines(after, rules, noMarks)
    reads.get should be < 12
  }

  "Opening detection" should "cost far less per edit than a full scan on a 500-chapter book" in {
    val index     = MarkdownHeadingIndex.build(Rope(markdownBook.mkString("\n")))
    val prior     = ChapterOpenings.openingLines(index, mode, rules, noMarks)
    val fullScans = (1 to 15).map(_ => timed(ChapterOpenings.openingLines(index, mode, rules, noMarks))._2)
    val headingEdits = (0 until 40).map { step =>
      val line  = 6 * (100 + step)
      val after = index.updatedTo(Rope(markdownBook.updated(line, s"# Chapter edited $step").mkString("\n")))
      val edit  = LineEdit.between(index.content, after.content)
      timed(ChapterOpenings.updatedOpeningLines(prior, after, edit, mode, rules, noMarks))._2
    }
    val typingEdits = (0 until 40).map { step =>
      val line  = 6 * (100 + step) + 2
      val after = index.updatedTo(Rope(markdownBook.updated(line, s"Opening typed $step.").mkString("\n")))
      val edit  = LineEdit.between(index.content, after.content)
      timed(ChapterOpenings.updatedOpeningLines(prior, after, edit, mode, rules, noMarks))._2
    }
    val richFull  = (1 to 15).map(_ => timed(ChapterOpenings.openingLines(richBook, rules, noMarks))._2)
    val richPrior = ChapterOpenings.openingLines(richBook, rules, noMarks)
    val richEdits = (0 until 40).map { step =>
      val at    = 4 * (100 + step)
      val after = richBook.replaceRange(RichTextRange(RichTextPosition(at, 0), RichTextPosition(at, 0)), "x")
      timed(ChapterOpenings.updatedOpeningLines(richPrior, after, LineEdit(at, 1, 1), rules, noMarks))._2
    }
    info(
      s"BENCH 500 chapters, markdown ${markdownBook.size} lines: full scan ${micros(median(fullScans))}, " +
        s"heading edit ${micros(median(headingEdits))}, typed edit ${micros(median(typingEdits))}; " +
        s"rich ${richBook.paragraphCount} paragraphs: full scan ${micros(median(richFull))}, edit ${micros(median(richEdits))}"
    )
    median(headingEdits) should be < median(fullScans)
    median(richEdits) should be < median(richFull)
  }
