package com.serenity.ui.layout

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** The terminal scrollbar's glyph for each cell of its column: a thin track, a heavy thumb, and half-row precision at
  * the thumb's ends.
  */
class TerminalScrollbarSpec extends AnyFlatSpec with Matchers:

  private val rows = 20

  private def render(glyphs: Option[Vector[ScrollGlyph]]): String =
    glyphs.fold("")(_.map(_.char).mkString)

  "The glyphs" should "be the thin track, the heavy thumb and the two half-row ends" in {
    ScrollGlyph.Track.char shouldBe '│'
    ScrollGlyph.Thumb.char shouldBe '┃'
    ScrollGlyph.ThumbEndsHere.char shouldBe '╿'
    ScrollGlyph.ThumbStartsHere.char shouldBe '╽'
  }

  "A document that fits" should "have no thumb to draw" in {
    TerminalScrollbar.glyphs(ScrollExtent(0, 30, 30), rows) shouldBe None
    TerminalScrollbar.glyphs(ScrollExtent(0, 30, 12), rows) shouldBe None
  }

  "A thumb on a whole number of rows" should "fill those cells with the heavy glyph, at the top of the document" in {
    val glyphs = TerminalScrollbar.glyphs(ScrollExtent(firstRow = 0, visibleRows = 40, totalRows = 160), rows)

    render(glyphs) shouldBe "┃┃┃┃┃" + "│" * 15
  }

  it should "reach the last cell at the end of the document" in {
    val glyphs = TerminalScrollbar.glyphs(ScrollExtent(firstRow = 120, visibleRows = 40, totalRows = 160), rows)

    render(glyphs) shouldBe "│" * 15 + "┃┃┃┃┃"
  }

  "A thumb that starts and ends half-way through a cell" should "be heavy on the half it fills there" in {
    // 20 rows are 40 half-rows; a quarter of 160 rows is a 10 half-row thumb, which at row 19 of 120 starts at half-row 5.
    val glyphs = TerminalScrollbar.glyphs(ScrollExtent(firstRow = 19, visibleRows = 40, totalRows = 160), rows)

    render(glyphs) shouldBe "││╽┃┃┃┃╿" + "│" * 12
  }

  "A document far longer than the column" should "still show a thumb of one cell" in {
    val glyphs = TerminalScrollbar.glyphs(ScrollExtent(firstRow = 0, visibleRows = 40, totalRows = 100000), rows)

    render(glyphs).count(_ != '│') should be >= 1
    render(glyphs).length shouldBe rows
  }

  "Every cell" should "get exactly one glyph, whatever the position" in
    (0 to 120 by 7).foreach { first =>
      TerminalScrollbar.glyphs(ScrollExtent(first, 40, 160), rows).map(_.length) shouldBe Some(rows)
    }
