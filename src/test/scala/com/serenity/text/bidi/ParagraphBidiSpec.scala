package com.serenity.text.bidi

import com.ibm.icu.text.Bidi
import com.serenity.text.AllocationProbe
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ParagraphBidiSpec extends AnyFlatSpec with Matchers:

  private def analysed(paragraph: String, direction: TextDirection): ParagraphBidi =
    ParagraphBidi.analyse(paragraph, direction) match
      case Some(bidi) => bidi
      case None       => fail(s"expected bidi analysis for '$paragraph'")

  private def icuFlags(direction: TextDirection): Int = direction match
    case TextDirection.Auto        => Bidi.DIRECTION_DEFAULT_LEFT_TO_RIGHT
    case TextDirection.LeftToRight => Bidi.DIRECTION_LEFT_TO_RIGHT
    case TextDirection.RightToLeft => Bidi.DIRECTION_RIGHT_TO_LEFT

  /** What per-row resolution does today: the row is analysed as if it were a paragraph of its own. */
  private def resolvedAsOwnParagraph(paragraph: String, start: Int, end: Int): Vector[DirectionalRun] =
    val row = new Bidi(paragraph.substring(start, end), Bidi.DIRECTION_DEFAULT_LEFT_TO_RIGHT)
    icuVisualRuns(row, start)

  private def icuVisualRuns(bidi: Bidi, offset: Int): Vector[DirectionalRun] =
    Vector.tabulate(bidi.countRuns()) { index =>
      val run = bidi.getVisualRun(index)
      DirectionalRun(offset + run.getStart, offset + run.getLimit, run.getEmbeddingLevel)
    }

  "analyse" should "give None for all-LTR text in an automatic paragraph" in {
    ParagraphBidi.analyse("plain Latin prose, 42 words.", TextDirection.Auto) shouldBe None
  }

  it should "give None for all-LTR text in an LTR paragraph" in {
    ParagraphBidi.analyse("val x = 1 // code", TextDirection.LeftToRight) shouldBe None
  }

  it should "allocate nothing for an all-LTR paragraph" in {
    val paragraph = "The quick brown fox jumps over the lazy dog. " * 250
    AllocationProbe.allocatedBytes(ParagraphBidi.analyse(paragraph, TextDirection.Auto)).foreach(_ shouldBe 0L)
  }

  it should "analyse all-LTR text when the paragraph is set right to left" in {
    analysed("abc", TextDirection.RightToLeft).paragraphLevel shouldBe 1
  }

  it should "take the paragraph level from the first strong character when automatic" in {
    analysed("אבג abc", TextDirection.Auto).paragraphLevel shouldBe 1
    analysed("abc אבג", TextDirection.Auto).paragraphLevel shouldBe 0
  }

  "visualRuns" should "split a mixed LTR row into level runs in visual order" in {
    analysed("abc אבג def", TextDirection.Auto).visualRuns(0, 11) shouldBe
      Vector(DirectionalRun(0, 4, 0), DirectionalRun(4, 7, 1), DirectionalRun(7, 11, 0))
  }

  it should "resolve a neutral at a row end at the paragraph's level, not the row's (#1898)" in {
    val paragraph = "abc אבג! דהו def"
    analysed(paragraph, TextDirection.Auto).visualRuns(0, 8) shouldBe
      Vector(DirectionalRun(0, 4, 0), DirectionalRun(4, 8, 1))
    resolvedAsOwnParagraph(paragraph, 0, 8) should not be Vector(DirectionalRun(0, 4, 0), DirectionalRun(4, 8, 1))
  }

  it should "resolve a neutral at a row start at the paragraph's level, not the row's (#1898)" in {
    val paragraph = "abc אבג! דהו def"
    analysed(paragraph, TextDirection.Auto).visualRuns(8, 16) shouldBe
      Vector(DirectionalRun(8, 12, 1), DirectionalRun(12, 16, 0))
    resolvedAsOwnParagraph(paragraph, 8, 16) should not be Vector(DirectionalRun(8, 12, 1), DirectionalRun(12, 16, 0))
  }

  it should "put a row's trailing whitespace at the paragraph level in an RTL paragraph (rule L1)" in {
    analysed("abc def", TextDirection.RightToLeft).visualRuns(0, 4) shouldBe
      Vector(DirectionalRun(3, 4, 1), DirectionalRun(0, 3, 2))
  }

  it should "order runs right to left in an RTL paragraph" in {
    analysed("אבג abc דהו", TextDirection.Auto).visualRuns(0, 11) shouldBe
      Vector(DirectionalRun(7, 11, 1), DirectionalRun(4, 7, 2), DirectionalRun(0, 4, 1))
  }

  // ICU reports a line whose levels are all even as one run at the paragraph level, so for those rows only the
  // direction and the covered range are comparable; the finer level runs are ours.
  it should "match ICU's own line reordering for every row of mixed paragraphs" in {
    val paragraphs = Vector(
      "abc אבג def",
      "abc אבג! דהו def",
      "אבג abc, 123 דהו.",
      "مرحبا 123 world\tשלום  ",
      "abc def  ",
      "(אבג) [abc] {דהו} 4.5% ב-2026",
      "a 😀 ب א c"
    )
    val directions = Vector(TextDirection.Auto, TextDirection.LeftToRight, TextDirection.RightToLeft)
    for
      paragraph <- paragraphs
      direction <- directions
      bidi      <- ParagraphBidi.analyse(paragraph, direction).toVector
      start     <- 0 until paragraph.length
      end       <- (start + 1) to paragraph.length
      if !Character.isLowSurrogate(paragraph.charAt(start))
      if end == paragraph.length || !Character.isLowSurrogate(paragraph.charAt(end))
    do
      val icuLine = new Bidi(paragraph, icuFlags(direction)).createLineBidi(start, end)
      withClue(s"'$paragraph' $direction [$start, $end): ") {
        val expected = icuVisualRuns(icuLine, start)
        val actual   = bidi.visualRuns(start, end)
        if expected.forall(!_.isRightToLeft) then
          actual.forall(!_.isRightToLeft) shouldBe true
          actual.map(_.start).minOption shouldBe Some(start)
          actual.map(_.end).maxOption shouldBe Some(end)
        else actual shouldBe expected
      }
  }

  it should "give no runs for an empty row" in {
    analysed("abc אבג", TextDirection.Auto).visualRuns(3, 3) shouldBe Vector.empty
  }
