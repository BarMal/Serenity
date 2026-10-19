package com.serenity.ui.layout

import java.awt.Font

import com.serenity.ui.renderer.{MeasuredTextAdvances, ShapedTextCache}

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class TextWrappingSpec extends AnyFlatSpec with Matchers:

  private val advances = MeasuredTextAdvances(MeasuredTextAdvances.DefaultCapacity, ShapedTextCache())

  private val sentence =
    "Using the layout removes these settings from your config file, so they follow the layout from then on."

  private def cellsOf(text: String): Int = text.codePoints().map(CharWidth.of(_)).sum()

  "TextWrapping.wrap" should "keep every row within the width and every word in order" in
    List(10, 17, 30, 45).foreach { width =>
      val rows = TextWrapping.wrap(sentence, width)

      rows.foreach(row => cellsOf(row) should be <= width)
      rows.flatMap(_.split(' ')).filter(_.nonEmpty) shouldBe sentence.split(' ').toList
      rows.size should be > 1
    }

  it should "fill each row as far as the next word allows" in {
    TextWrapping.wrap("aa bb cc dd", 5) shouldBe List("aa bb", "cc dd")
  }

  it should "leave text that fits on one row, and an empty line as one empty row" in {
    TextWrapping.wrap("short", 20) shouldBe List("short")
    TextWrapping.wrap("", 20) shouldBe List("")
  }

  it should "break a word longer than the width across rows without losing a letter" in {
    val rows = TextWrapping.wrap("abcdefghij kl", 4)

    rows shouldBe List("abcd", "efgh", "ij", "kl")
    TextWrapping.wrap("x", 0) shouldBe List("x")
  }

  it should "measure wide characters in cells" in {
    val rows = TextWrapping.wrap("日本語日本語", 6)

    rows shouldBe List("日本語", "日本語")
    rows.foreach(row => cellsOf(row) should be <= 6)
    TextWrapping.wrap("日本 語日 本語", 5) shouldBe List("日本", "語日", "本語")
  }

  it should "never split a surrogate pair" in {
    val rows = TextWrapping.wrap("😀😀😀😀😀", 4)

    rows shouldBe List("😀😀", "😀😀", "😀")
  }

  it should "keep a paragraph's indent on every row it wraps to" in {
    TextWrapping.wrap("  aa bb cc", 6) shouldBe List("  aa", "  bb", "  cc")
  }

  "TextWrapping.wrap by pixels" should "keep every row within the content's pixel width with a proportional font" in {
    val measure = proportional()

    List(8, 14, 24, 40).foreach { width =>
      val rows = TextWrapping.wrap(sentence, width, measure)

      rows.foreach(row => advancePx(row, measure) should be <= (width * measure.cellPx).toFloat)
      rows.flatMap(_.split(' ')).filter(_.nonEmpty) shouldBe sentence.split(' ').toList
    }
  }

  it should "leave room for the text inset" in {
    val inset = proportional(insetPx = 30.0)

    TextWrapping.wrap(sentence, 24, inset).foreach { row =>
      advancePx(row, inset) should be <= (24 * inset.cellPx - 30.0).toFloat
    }
  }

  it should "pack narrow glyphs into fewer rows than the cell count would" in {
    val narrow  = List.fill(12)("iiiii").mkString(" ")
    val measure = proportional()

    TextWrapping.wrap(narrow, 20, measure).size should be < TextWrapping.wrap(narrow, 20).size
  }

  it should "break a word wider than the row" in {
    val measure = proportional()
    val rows    = TextWrapping.wrap("WWWWWWWWWWWWWWWWWWWWWWWW", 6, measure)

    rows.size should be > 1
    rows.mkString shouldBe "WWWWWWWWWWWWWWWWWWWWWWWW"
    rows.foreach(row => advancePx(row, measure) should be <= (6 * measure.cellPx).toFloat)
  }

  private def proportional(insetPx: Double = 0.0): TextMeasure.Pixels =
    val font = Font(Font.SANS_SERIF, Font.PLAIN, 14)
    TextMeasure.Pixels(
      font,
      TextLayoutSnapshot.defaultFontRenderContext(),
      CellMetrics.fromFont(Font(Font.MONOSPACED, Font.PLAIN, 14)).charWidth,
      insetPx,
      advances
    )

  private def advancePx(row: String, measure: TextMeasure.Pixels): Float =
    measure.advances.advanceOf(row, measure.font, measure.fontRenderContext)
