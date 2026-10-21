package com.serenity.ui.layout

import java.awt.Font

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** #1866 (PR A2): a combining mark composes onto the character before it and takes no cell of its own, on the terminal
  * grid and the GUI cell grid alike, so the carets, the wrap and the width of a row all count a base and its marks as
  * the one cell the base takes.
  */
class CombiningMarkWidthSpec extends AnyFlatSpec with Matchers:

  private val acute           = "́"
  private val circumflex      = "̂"
  private val devanagariAa    = 0x093e
  private val enclosingCircle = 0x20dd

  private val frc    = TextLayoutSnapshot.defaultFontRenderContext()
  private val font   = Font(Font.MONOSPACED, Font.PLAIN, 14)
  private val cellPx = CellMetrics.fromFont(font).charWidth
  private def rows(text: String, widthPx: Int = 10_000) =
    TextLayoutSnapshot.boundedVisualLinesForText(text, 0, widthPx, font, frc, forceCellLayout = true)

  "CharWidth.of" should "give a combining mark no cells, whichever kind of mark it is" in {
    CharWidth.of(0x0301) shouldBe 0
    CharWidth.of(devanagariAa) shouldBe 0
    CharWidth.of(enclosingCircle) shouldBe 0
  }

  it should "leave the characters that carry marks as they were" in {
    CharWidth.of('e'.toInt) shouldBe 1
    CharWidth.of(0x00e9) shouldBe 1
    CharWidth.of(0x4e2d) shouldBe 2
  }

  "TabStops.advance" should "give a mark no cells, wherever it falls" in {
    TabStops.advance(0x0301, 0, 4) shouldBe 0
    TabStops.advance(0x0301, 3, 4) shouldBe 0
    TabStops.advance('\t'.toInt, 3, 4) shouldBe 1
  }

  "The terminal-grid carets" should "put a mark's caret at the end of its base" in {
    TextCaretMeasurement.displayWidthCaretXs(s"e${acute}x", 1.0f, 4).toVector shouldBe Vector(0.0f, 1.0f, 1.0f, 2.0f)
  }

  it should "stack several marks on the one base" in {
    TextCaretMeasurement.displayWidthCaretXs(s"e$acute${circumflex}x", 1.0f, 4).toVector shouldBe
      Vector(0.0f, 1.0f, 1.0f, 1.0f, 2.0f)
  }

  it should "carry a mark after a wide character on the wide character's two cells" in {
    TextCaretMeasurement.displayWidthCaretXs(s"中${acute}x", 1.0f, 4).toVector shouldBe Vector(0.0f, 2.0f, 2.0f, 3.0f)
  }

  it should "give a mark with no base before it no width" in {
    TextCaretMeasurement.displayWidthCaretXs(s"${acute}a", 1.0f, 4).toVector shouldBe Vector(0.0f, 0.0f, 1.0f)
  }

  "A GUI cell-grid row" should "count a base and its mark as one cell, with no caret stop between them" in {
    val row = rows(s"a${acute}b").head
    row.row.caretXs.toVector.map(_ / cellPx.toFloat) shouldBe Vector(0.0f, 1.0f, 2.0f)
    row.row.caretOffsets.toVector shouldBe Vector(0, 2, 3)
    row.widthPx shouldBe 2.0f * cellPx +- 0.001f
  }

  it should "count stacked marks as part of the one base" in {
    val row = rows(s"e$acute${circumflex}x").head
    row.row.caretXs.toVector.map(_ / cellPx.toFloat) shouldBe Vector(0.0f, 1.0f, 2.0f)
    row.row.caretOffsets.toVector shouldBe Vector(0, 3, 4)
  }

  it should "carry a mark after a wide character on its two cells" in {
    val row = rows(s"中${acute}a").head
    row.row.caretXs.toVector.map(_ / cellPx.toFloat) shouldBe Vector(0.0f, 2.0f, 3.0f)
  }

  it should "give a mark with no base before it no width" in {
    rows(s"${acute}a").head.widthPx shouldBe 1.0f * cellPx +- 0.001f
  }

  it should "wrap on the cells the bases take, never between a base and its mark" in {
    val text = s"e${acute}e${acute}e$acute"
    rows(text, 2 * cellPx).map(_.text) shouldBe Vector(s"e${acute}e$acute", s"e$acute")
  }
