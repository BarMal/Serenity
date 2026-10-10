package com.serenity

import com.serenity.markdown.MarkdownBlockLens.{SetextRole, TableRow}
import com.serenity.markdown.MarkdownInlineSpans.{Run, Style}
import com.serenity.markdown.{MarkdownBlockLens, MarkdownBlockSpans}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class MarkdownBlockSpansSpec extends AnyFlatSpec with Matchers:

  private val plain  = Style(bold = false, italic = false, code = false)
  private val glyph  = plain.copy(bold = true, muted = true)
  private val quoted = plain.copy(italic = true)
  private val dimmed = plain.copy(muted = true)

  private def heading(level: Int) = plain.copy(bold = true, scale = MarkdownBlockSpans.headingScale(level))

  private def marker(start: Int, end: Int): Run                = Run(start, end, plain, isMarker = true)
  private def content(start: Int, end: Int, style: Style): Run = Run(start, end, style, isMarker = false)

  "A heading" should "hide its hashes and set the text in a larger bold face" in {
    MarkdownBlockSpans.scan("# Title") shouldBe Vector(marker(0, 2), content(2, 7, heading(1)))
    MarkdownBlockSpans.scan("### Third") shouldBe Vector(marker(0, 4), content(4, 9, heading(3)))
  }

  it should "scale by level, largest first" in {
    val scales = (1 to 5).map(MarkdownBlockSpans.headingScale)
    scales shouldBe scales.sorted.reverse
    MarkdownBlockSpans.headingScale(1) should be > 1.0f
  }

  it should "hide a closing run of hashes too" in {
    MarkdownBlockSpans.scan("## Two ##") shouldBe Vector(marker(0, 3), content(3, 6, heading(2)), marker(6, 9))
  }

  it should "keep inline emphasis inside it, in the heading's size" in {
    MarkdownBlockSpans.scan("# a *b*") shouldBe Vector(
      marker(0, 2),
      content(2, 4, heading(1)),
      marker(4, 5),
      content(5, 6, heading(1).copy(italic = true)),
      marker(6, 7)
    )
  }

  it should "not be a heading without a space, with seven hashes, or after four spaces" in {
    MarkdownBlockSpans.scan("#hashtag") shouldBe Vector.empty
    MarkdownBlockSpans.scan("####### seven") shouldBe Vector.empty
    MarkdownBlockSpans.scan("    # code") shouldBe Vector.empty
  }

  "A list item" should "style its bullet or number and leave the text alone" in {
    MarkdownBlockSpans.scan("- item") shouldBe Vector(content(0, 1, glyph))
    MarkdownBlockSpans.scan("  * item") shouldBe Vector(content(2, 3, glyph))
    MarkdownBlockSpans.scan("12. item") shouldBe Vector(content(0, 3, glyph))
    MarkdownBlockSpans.scan("1) item") shouldBe Vector(content(0, 2, glyph))
  }

  it should "style a task box, and dim the text of a finished task" in {
    MarkdownBlockSpans.scan("- [ ] todo") shouldBe Vector(content(0, 1, glyph), content(2, 5, glyph))
    MarkdownBlockSpans.scan("- [x] done") shouldBe
      Vector(content(0, 1, glyph), content(2, 5, glyph), content(6, 10, dimmed))
  }

  it should "not take a delimiter run or a bare number for a bullet" in {
    MarkdownBlockSpans.scan("-item") shouldBe Vector.empty
    MarkdownBlockSpans.scan("2024 was a year") shouldBe Vector.empty
    MarkdownBlockSpans.scan("**bold** start").exists(_.style.muted) shouldBe false
  }

  "A block quote" should "style its marks and set the text in italics" in {
    MarkdownBlockSpans.scan("> quote") shouldBe Vector(content(0, 2, glyph), content(2, 7, quoted))
    MarkdownBlockSpans.scan("> > deep") shouldBe Vector(content(0, 4, glyph), content(4, 8, quoted))
  }

  "A thematic break" should "be one hidden run over the whole line" in {
    MarkdownBlockSpans.scan("---") shouldBe Vector(marker(0, 3))
    MarkdownBlockSpans.scan("* * *") shouldBe Vector(marker(0, 5))
    MarkdownBlockSpans.scan("___  ") shouldBe Vector(marker(0, 5))
    MarkdownBlockSpans.isRule("---", TableRow.None) shouldBe true
    MarkdownBlockSpans.isRule("--", TableRow.None) shouldBe false
  }

  "A setext heading" should "set its text lines like a heading and hide its underline" in {
    MarkdownBlockSpans.scan("Title", TableRow.None, SetextRole.Text(1)) shouldBe Vector(content(0, 5, heading(1)))
    MarkdownBlockSpans.scan("Sub *it*", TableRow.None, SetextRole.Text(2)) shouldBe Vector(
      content(0, 4, heading(2)),
      marker(4, 5),
      content(5, 7, heading(2).copy(italic = true)),
      marker(7, 8)
    )
    MarkdownBlockSpans.scan("---", TableRow.None, SetextRole.Underline) shouldBe Vector(marker(0, 3))
    MarkdownBlockSpans.scan("=", TableRow.None, SetextRole.Underline) shouldBe Vector(marker(0, 1))
  }

  it should "draw its underline as a line, including one too short to be a thematic break" in {
    MarkdownBlockSpans.isRule("--", TableRow.None, SetextRole.Underline) shouldBe true
    MarkdownBlockSpans.isRule("===", TableRow.None, SetextRole.Underline) shouldBe true
  }

  "A table row" should "dim its pipes and set the header in bold" in {
    MarkdownBlockSpans.scan("| a | b |", TableRow.Header) shouldBe Vector(
      content(0, 1, dimmed),
      content(1, 4, plain.copy(bold = true)),
      content(4, 5, dimmed),
      content(5, 8, plain.copy(bold = true)),
      content(8, 9, dimmed)
    )
    MarkdownBlockSpans.scan("| 1 | 2 |", TableRow.Body) shouldBe
      Vector(content(0, 1, dimmed), content(4, 5, dimmed), content(8, 9, dimmed))
  }

  it should "leave an escaped pipe as text" in {
    MarkdownBlockSpans.scan("a \\| b | c", TableRow.Body) shouldBe Vector(content(7, 8, dimmed))
  }

  it should "hide the delimiter row as a rule" in {
    MarkdownBlockSpans.scan("|---|:-:|", TableRow.Delimiter) shouldBe Vector(marker(0, 9))
    MarkdownBlockSpans.isRule("|---|:-:|", TableRow.Delimiter) shouldBe true
  }

  it should "keep inline emphasis inside a cell" in {
    MarkdownBlockSpans.scan("| **a** |", TableRow.Body) shouldBe Vector(
      content(0, 1, dimmed),
      marker(2, 4),
      content(4, 5, plain.copy(bold = true)),
      marker(5, 7),
      content(8, 9, dimmed)
    )
  }

  "A line outside any block" should "have the inline runs alone" in {
    MarkdownBlockSpans.scan("plain **bold**") shouldBe
      Vector(marker(6, 8), content(8, 12, plain.copy(bold = true)), marker(12, 14))
    MarkdownBlockSpans.scan("plain prose") shouldBe Vector.empty
  }

  "The block index" should "find a table from its header, delimiter row and the rows after" in {
    val lines = Vector("intro", "| a | b |", "|---|---|", "| 1 | 2 |", "| 3 | 4 |", "", "after")
    val index = MarkdownBlockLens.fenceRangeIndex(lines.iterator)

    index.tables shouldBe Vector(1 to 4)
    index.tableRowAt(0) shouldBe TableRow.None
    index.tableRowAt(1) shouldBe TableRow.Header
    index.tableRowAt(2) shouldBe TableRow.Delimiter
    index.tableRowAt(3) shouldBe TableRow.Body
    index.tableRowAt(4) shouldBe TableRow.Body
    index.tableRowAt(5) shouldBe TableRow.None
  }

  it should "find a setext heading, with every paragraph line above its underline and its level from the underline" in {
    val index = MarkdownBlockLens.fenceRangeIndex(Iterator("Title", "=====", "", "one", "two", "---", "text"))

    index.setext shouldBe Vector(MarkdownBlockLens.SetextHeading(0 to 1, 1), MarkdownBlockLens.SetextHeading(3 to 5, 2))
    index.setextAt(0) shouldBe SetextRole.Text(1)
    index.setextAt(1) shouldBe SetextRole.Underline
    index.setextAt(2) shouldBe SetextRole.None
    index.setextAt(4) shouldBe SetextRole.Text(2)
    index.setextAt(5) shouldBe SetextRole.Underline
    index.setextAt(6) shouldBe SetextRole.None
  }

  it should "leave a rule that is not directly under a paragraph line alone" in {
    MarkdownBlockLens.fenceRangeIndex(Iterator("text", "", "---")).setext shouldBe Vector.empty
    MarkdownBlockLens.fenceRangeIndex(Iterator("- item", "---")).setext shouldBe Vector.empty
    MarkdownBlockLens.fenceRangeIndex(Iterator("# heading", "---")).setext shouldBe Vector.empty
    MarkdownBlockLens.fenceRangeIndex(Iterator("```", "text", "---", "```")).setext shouldBe Vector.empty
  }

  it should "not take piped prose without a delimiter row for a table" in {
    MarkdownBlockLens.fenceRangeIndex(Iterator("a | b | c", "d | e | f")).tables shouldBe Vector.empty
  }

  it should "not find a table inside a fenced block, and keep the fences" in {
    val lines = Vector("```", "| a | b |", "|---|---|", "```", "| c | d |", "|---|---|")
    val index = MarkdownBlockLens.fenceRangeIndex(lines.iterator)

    index.ranges shouldBe Vector(0 to 3)
    index.tables shouldBe Vector(4 to 5)
  }
