package com.serenity

import com.serenity.markdown.MarkdownTableColumns
import com.serenity.markdown.MarkdownTableColumns.{Alignment, Cell, Columns}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class MarkdownTableColumnsSpec extends AnyFlatSpec with Matchers:

  private val noneHidden: Int => Boolean = _ => false

  private def columns(widths: Vector[Float], alignments: Alignment*) = Columns(widths, alignments.toVector)

  "MarkdownTableColumns.cells" should "split a row at its unescaped pipes, leaving out the empty cell after a closing pipe" in {
    MarkdownTableColumns.cells("| a | b |") shouldBe Vector(Cell(1, 4), Cell(5, 8))
    MarkdownTableColumns.cells("a | b") shouldBe Vector(Cell(0, 2), Cell(3, 5))
    MarkdownTableColumns.cells("| a \\| b |") shouldBe Vector(Cell(1, 9))
    MarkdownTableColumns.cells("no pipes") shouldBe Vector(Cell(0, 8))
  }

  "MarkdownTableColumns.alignments" should "read them from the delimiter row's colons" in {
    MarkdownTableColumns.alignments("|:--|--:|:-:|---|") shouldBe
      Vector(Alignment.Left, Alignment.Right, Alignment.Center, Alignment.Left)
  }

  "MarkdownTableColumns.extraAdvances" should "pad a left-aligned cell after its text" in {
    val extra = MarkdownTableColumns.extraAdvances(
      "| a | bb |",
      Vector(30.0f, 40.0f),
      columns(Vector(50.0f, 40.0f), Alignment.Left, Alignment.Left),
      noneHidden
    )

    extra shouldBe Map(3 -> 20.0f)
  }

  it should "pad a right-aligned cell before its text, on the pipe in front of it" in {
    val extra = MarkdownTableColumns.extraAdvances(
      "| a | bb |",
      Vector(30.0f, 40.0f),
      columns(Vector(50.0f, 40.0f), Alignment.Right, Alignment.Left),
      noneHidden
    )

    extra shouldBe Map(0 -> 20.0f)
  }

  it should "split the padding of a centred cell around its text" in {
    val extra = MarkdownTableColumns.extraAdvances(
      "| a | bb |",
      Vector(30.0f, 40.0f),
      columns(Vector(50.0f, 40.0f), Alignment.Center, Alignment.Left),
      noneHidden
    )

    extra shouldBe Map(0 -> 10.0f, 3 -> 10.0f)
  }

  it should "never pad a hidden column, nor a last cell with no pipe after it" in {
    val columnsOfBoth = columns(Vector(50.0f, 50.0f), Alignment.Left, Alignment.Left)

    MarkdownTableColumns.extraAdvances("|**a**|", Vector(30.0f), columnsOfBoth, Set(1, 2, 4, 5).contains) shouldBe
      Map(3 -> 20.0f)
    MarkdownTableColumns.extraAdvances("| a | b", Vector(30.0f, 30.0f), columnsOfBoth, noneHidden) shouldBe
      Map(3 -> 20.0f)
  }

  it should "add nothing when every cell is already as wide as its column" in {
    MarkdownTableColumns.extraAdvances(
      "| a |",
      Vector(30.0f),
      columns(Vector(30.0f), Alignment.Left),
      noneHidden
    ) shouldBe Map.empty
  }
