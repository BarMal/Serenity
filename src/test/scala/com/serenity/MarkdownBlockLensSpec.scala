package com.serenity

import java.util.concurrent.atomic.AtomicInteger

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

  it should "treat headings as their own block even when followed by prose without a blank line" in {
    val lines = Vector(
      "# Title",
      "Paragraph immediately after the heading",
      "continued paragraph line"
    )

    MarkdownBlockLens.currentBlock(lines, activeLine = 0) shouldBe (0 to 0)
    MarkdownBlockLens.currentBlock(lines, activeLine = 1) shouldBe (1 to 2)
  }

  it should "reveal only the current list item with its continuation lines" in {
    val lines = Vector(
      "Before",
      "",
      "- first",
      "  continuation",
      "- second",
      "",
      "After"
    )

    MarkdownBlockLens.currentBlock(lines, activeLine = 2) shouldBe (2 to 3)
    MarkdownBlockLens.currentBlock(lines, activeLine = 3) shouldBe (2 to 3)
    MarkdownBlockLens.currentBlock(lines, activeLine = 4) shouldBe (4 to 4)
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

  it should "keep fenced code together across blank lines" in {
    val lines = Vector(
      "```scala",
      "val x = 1",
      "",
      "",
      "",
      "val y = 2",
      "val z = 3",
      "val result = x + y + z",
      "```",
      "After fence"
    )

    MarkdownBlockLens.currentBlock(lines, activeLine = 7) shouldBe (0 to 8)
  }

  it should "select a long fenced block when the closing delimiter is active" in {
    val lines = Vector("```") ++ (1 to 17).map(index => s"val line = $index") ++ Vector("```")

    MarkdownBlockLens.currentBlock(lines, activeLine = 18) shouldBe (0 to 18)
  }

  it should "select an interior line in a long fenced block" in {
    val lines = Vector("```") ++ (1 to 20).map(index => s"val line = $index") ++ Vector("```")

    MarkdownBlockLens.currentBlock(lines, activeLine = 15) shouldBe (0 to 21)
  }

  it should "select a closing delimiter after more than 64 fenced lines" in {
    val lines = Vector("```") ++ (1 to 80).map(index => s"val line = $index") ++ Vector("```")

    MarkdownBlockLens.currentBlock(lines, activeLine = 81) shouldBe (0 to 81)
  }

  it should "select an interior line beyond the fence lookup window" in {
    val lines = Vector("```") ++ (1 to 80).map(index => s"val line = $index") ++ Vector("```")

    MarkdownBlockLens.currentBlock(lines, activeLine = 70) shouldBe (0 to 81)
  }

  it should "keep a bare closing delimiter with its preceding block" in {
    val lines = Vector(
      "```",
      "first block",
      "```",
      "Prose between blocks",
      "```",
      "second block",
      "```"
    )

    MarkdownBlockLens.currentBlock(lines, activeLine = 2) shouldBe (0 to 2)
  }

  it should "leave prose outside a long fenced block before the next opener" in {
    val lines = Vector("```") ++
      (1 to 17).map(index => s"val first = $index") ++
      Vector("```", "Prose between long blocks", "```", "second block", "```")

    MarkdownBlockLens.currentBlock(lines, activeLine = 19) shouldBe (19 to 19)
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

  it should "not pair prose between adjacent fenced blocks" in {
    val lines = Vector(
      "```scala",
      "val first = 1",
      "```",
      "Prose between fences",
      "```scala",
      "val second = 2",
      "```"
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

  it should "keep a thematic break separate from following prose" in {
    val lines = Vector(
      "- Previous item",
      "---",
      "After paragraph"
    )

    MarkdownBlockLens.currentBlock(lines, activeLine = 1) shouldBe (1 to 1)
  }

  it should "treat setext heading text and underline as one source unit" in {
    val lines = Vector(
      "Setext title",
      "---",
      "After paragraph"
    )

    MarkdownBlockLens.currentBlock(lines, activeLine = 0) shouldBe (0 to 1)
    MarkdownBlockLens.currentBlock(lines, activeLine = 1) shouldBe (0 to 1)
  }

  it should "leave blank lines as single-line blocks" in {
    val lines = Vector("One", "", "Two")

    MarkdownBlockLens.currentBlock(lines, activeLine = 1) shouldBe (1 to 1)
  }

  it should "clamp out-of-range active lines before resolving a block" in {
    val lines = Vector("Only paragraph")

    MarkdownBlockLens.currentBlock(lines, activeLine = -10) shouldBe (0 to 0)
    MarkdownBlockLens.currentBlock(lines, activeLine = 10) shouldBe (0 to 0)
  }

  it should "resolve a block through indexed line access without materialising the document" in {
    val lines = Vector.fill(200)("unrelated").updated(99, "").updated(100, "").updated(101, "First paragraph")
      .updated(102, "continued").updated(103, "")
    val reads = AtomicInteger(0)

    val block = MarkdownBlockLens.currentBlock(
      lineCount = lines.length,
      lineAt = index =>
        reads.incrementAndGet()
        lines.lift(index),
      activeLine = 102
    )

    block shouldBe (101 to 102)
    reads.get() should be < 600
  }

  it should "resolve fenced blocks without reading unrelated leading lines" in {
    val lines = Vector.fill(1_003)("unrelated")
      .updated(998, "")
      .updated(999, "")
      .updated(1_000, "```scala")
      .updated(1_001, "val result = 1")
      .updated(1_002, "```")
    val reads = AtomicInteger(0)

    val block = MarkdownBlockLens.currentBlock(
      lineCount = lines.length,
      lineAt = index =>
        reads.incrementAndGet()
        lines.lift(index),
      activeLine = 1_001
    )

    block shouldBe (1_000 to 1_002)
    reads.get() should be < 30
  }

  it should "resolve a fence after uninterrupted prose" in {
    val lines = Vector.fill(1_003)("unrelated prose")
      .updated(1_000, "```scala")
      .updated(1_001, "val result = 1")
      .updated(1_002, "```")
    val block = MarkdownBlockLens.currentBlock(
      lineCount = lines.length,
      lineAt = lines.lift,
      activeLine = 1_001
    )

    block shouldBe (1_000 to 1_002)
  }
