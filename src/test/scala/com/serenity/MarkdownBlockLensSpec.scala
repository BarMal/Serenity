package com.serenity

import com.serenity.markdown.MarkdownBlockLens
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class MarkdownBlockLensSpec extends AnyFlatSpec with Matchers:

  "MarkdownBlockLens" should "treat contiguous paragraph lines as the current block" in {
    val lines = Vector(
      "# Title",
      "",
      "First paragraph line",
      "continued paragraph line",
      "",
      "Next paragraph"
    )

    MarkdownBlockLens.currentBlock(lines, activeLine = 2) shouldBe (2 to 3)
    MarkdownBlockLens.currentBlock(lines, activeLine = 3) shouldBe (2 to 3)
  }

  it should "treat contiguous list items and indented continuations as one block" in {
    val lines = Vector(
      "Before",
      "",
      "- first",
      "  continuation",
      "- second",
      "",
      "After"
    )

    MarkdownBlockLens.currentBlock(lines, activeLine = 3) shouldBe (2 to 4)
  }

  it should "treat blockquotes as one block" in {
    val lines = Vector(
      "Intro",
      "",
      "> one",
      "> two",
      "",
      "After"
    )

    MarkdownBlockLens.currentBlock(lines, activeLine = 2) shouldBe (2 to 3)
  }

  it should "treat fenced code blocks as one block including fences" in {
    val lines = Vector(
      "Intro",
      "```scala",
      "val x = 1",
      "```",
      "After"
    )

    MarkdownBlockLens.currentBlock(lines, activeLine = 2) shouldBe (1 to 3)
  }

  it should "not let a previous closing fence capture following prose" in {
    val lines = Vector(
      "```",
      "code",
      "```",
      "After fence"
    )

    MarkdownBlockLens.currentBlock(lines, activeLine = 3) shouldBe (3 to 3)
  }

  it should "treat markdown tables as one block" in {
    val lines = Vector(
      "Intro",
      "",
      "| A | B |",
      "|---|---|",
      "| 1 | 2 |",
      "",
      "After"
    )

    MarkdownBlockLens.currentBlock(lines, activeLine = 4) shouldBe (2 to 4)
  }

  it should "leave blank lines as single-line blocks" in {
    val lines = Vector("One", "", "Two")

    MarkdownBlockLens.currentBlock(lines, activeLine = 1) shouldBe (1 to 1)
  }
