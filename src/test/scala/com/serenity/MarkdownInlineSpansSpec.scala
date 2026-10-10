package com.serenity

import com.serenity.markdown.MarkdownInlineSpans
import com.serenity.markdown.MarkdownInlineSpans.{Run, Style}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class MarkdownInlineSpansSpec extends AnyFlatSpec with Matchers:

  private val plain  = Style(bold = false, italic = false, code = false)
  private val bold   = plain.copy(bold = true)
  private val italic = plain.copy(italic = true)
  private val code   = plain.copy(code = true)

  private def marker(start: Int, end: Int): Run                = Run(start, end, plain, isMarker = true)
  private def content(start: Int, end: Int, style: Style): Run = Run(start, end, style, isMarker = false)

  "MarkdownInlineSpans.scan" should "style strong text and mark its delimiters" in {
    MarkdownInlineSpans.scan("**bold**") shouldBe Vector(marker(0, 2), content(2, 6, bold), marker(6, 8))
  }

  it should "style emphasised text with either delimiter" in {
    MarkdownInlineSpans.scan("*it*") shouldBe Vector(marker(0, 1), content(1, 3, italic), marker(3, 4))
    MarkdownInlineSpans.scan("_it_") shouldBe Vector(marker(0, 1), content(1, 3, italic), marker(3, 4))
    MarkdownInlineSpans.scan("__bold__") shouldBe Vector(marker(0, 2), content(2, 6, bold), marker(6, 8))
  }

  it should "style text between triple delimiters as both strong and emphasised" in {
    MarkdownInlineSpans.scan("***both***") shouldBe
      Vector(marker(0, 3), content(3, 7, bold.copy(italic = true)), marker(7, 10))
  }

  it should "style code spans and treat their content literally" in {
    MarkdownInlineSpans.scan("`code`") shouldBe Vector(marker(0, 1), content(1, 5, code), marker(5, 6))
    MarkdownInlineSpans.scan("`a **b** c`") shouldBe Vector(marker(0, 1), content(1, 10, code), marker(10, 11))
    MarkdownInlineSpans.scan("``a`b``") shouldBe Vector(marker(0, 2), content(2, 5, code), marker(5, 7))
  }

  it should "nest emphasis inside strong text" in {
    MarkdownInlineSpans.scan("**a *b* c**") shouldBe Vector(
      marker(0, 2),
      content(2, 4, bold),
      marker(4, 5),
      content(5, 6, bold.copy(italic = true)),
      marker(6, 7),
      content(7, 9, bold),
      marker(9, 11)
    )
  }

  it should "leave unmatched, escaped and intraword delimiters as text" in {
    MarkdownInlineSpans.scan("2 * 3 * 4") shouldBe Vector.empty
    MarkdownInlineSpans.scan("**unclosed") shouldBe Vector.empty
    MarkdownInlineSpans.scan("snake_case_name") shouldBe Vector.empty
    MarkdownInlineSpans.scan("\\*not italic\\*") shouldBe Vector.empty
    MarkdownInlineSpans.scan("* a list item") shouldBe Vector.empty
    MarkdownInlineSpans.scan("***") shouldBe Vector.empty
    MarkdownInlineSpans.scan("an `unclosed code span") shouldBe Vector.empty
  }

  it should "report only the decorated stretches of a line" in {
    MarkdownInlineSpans.scan("plain **bold** and *it*") shouldBe Vector(
      marker(6, 8),
      content(8, 12, bold),
      marker(12, 14),
      marker(19, 20),
      content(20, 22, italic),
      marker(22, 23)
    )
  }

  "MarkdownInlineSpans.mayContainMarkup" should "be false for a line with no delimiter character" in {
    MarkdownInlineSpans.mayContainMarkup("plain prose, nothing to see") shouldBe false
    MarkdownInlineSpans.mayContainMarkup("a_b") shouldBe true
    MarkdownInlineSpans.mayContainMarkup("a `b") shouldBe true
  }

  "MarkdownInlineSpans.hiddenColumns" should "list the marker columns in ascending order" in {
    MarkdownInlineSpans.hiddenColumns(MarkdownInlineSpans.scan("a **b** c")) shouldBe Vector(2, 3, 5, 6)
  }
