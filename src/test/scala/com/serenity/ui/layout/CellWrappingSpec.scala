package com.serenity.ui.layout

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class CellWrappingSpec extends AnyFlatSpec with Matchers:

  private val sentence =
    "Using the layout removes these settings from your config file, so they follow the layout from then on."

  private def cellsOf(text: String): Int = text.codePoints().map(CharWidth.of(_)).sum()

  "CellWrapping.wrapCells" should "keep every row within the width and every word in order" in
    List(10, 17, 30, 45).foreach { width =>
      val rows = CellWrapping.wrapCells(sentence, width)

      rows.foreach(row => cellsOf(row) should be <= width)
      rows.flatMap(_.split(' ')).filter(_.nonEmpty) shouldBe sentence.split(' ').toList
      rows.size should be > 1
    }

  it should "fill each row as far as the next word allows" in {
    CellWrapping.wrapCells("aa bb cc dd", 5) shouldBe List("aa bb", "cc dd")
  }

  it should "leave text that fits on one row, and an empty line as one empty row" in {
    CellWrapping.wrapCells("short", 20) shouldBe List("short")
    CellWrapping.wrapCells("", 20) shouldBe List("")
  }

  it should "break a word longer than the width across rows without losing a letter" in {
    CellWrapping.wrapCells("abcdefghij kl", 4) shouldBe List("abcd", "efgh", "ij", "kl")
    CellWrapping.wrapCells("x", 0) shouldBe List("x")
  }

  it should "measure wide characters in cells" in {
    CellWrapping.wrapCells("日本語日本語", 6) shouldBe List("日本語", "日本語")
    CellWrapping.wrapCells("日本 語日 本語", 5) shouldBe List("日本", "語日", "本語")
  }

  it should "never split a surrogate pair" in {
    CellWrapping.wrapCells("😀😀😀😀😀", 4) shouldBe List("😀😀", "😀😀", "😀")
  }

  it should "keep a paragraph's indent on every row it wraps to" in {
    CellWrapping.wrapCells("  aa bb cc", 6) shouldBe List("  aa", "  bb", "  cc")
  }

  "CellWrapping.wrap" should "break by whatever measure it is given" in {
    CellWrapping.wrap("aa bb cc", row => row.length <= 5) shouldBe List("aa bb", "cc")
    CellWrapping.wrap("aa bb cc", row => row.length <= 2) shouldBe List("aa", "bb", "cc")
  }

  "TextWrapping.wrap by cells" should "give the rows CellWrapping.wrapCells gives, at every width" in {
    val corpus = List(
      sentence,
      "",
      "short",
      "  indented paragraph that has to wrap more than once",
      "abcdefghij klmnopqrstuvwxyz",
      "日本語日本語 日本 語日 本語",
      "😀😀😀😀😀 😀😀",
      "ééé café au lait",
      "a  b   c",
      " leading and trailing "
    )

    for
      text  <- corpus
      width <- List(-3, 0, 1, 2, 3, 5, 8, 13, 21, 80)
    do CellWrapping.wrapCells(text, width) shouldBe TextWrapping.wrap(text, width)
  }
