package com.serenity

import com.serenity.ui.layout.{CellMetrics, LayoutRect, TextRowMetrics}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class TextRowMetricsSpec extends AnyFlatSpec with Matchers:

  private val rect    = LayoutRect(x = 4, y = 2, width = 40, height = 5)
  private val metrics = CellMetrics(charWidth = 8, lineHeight = 16, ascent = 11)

  "TextRowMetrics" should "place grid rows on cell boundaries" in {
    val rows = TextRowMetrics(rect, metrics, rowLineHeightPx = 16, usesMeasuredLayout = false)

    rows.lineTopPx(0) shouldBe 32
    rows.lineTopPx(2) shouldBe 64
    rows.contentBottomPx shouldBe 112
  }

  it should "place measured rows using the rendered text line height inside the content rect" in {
    val rows = TextRowMetrics(rect, metrics, rowLineHeightPx = 24, usesMeasuredLayout = true)

    rows.lineTopPx(0) shouldBe 32
    rows.lineTopPx(2) shouldBe 80
    rows.lineFits(3) shouldBe true
    rows.lineFits(4) shouldBe false
  }

  it should "keep measured cursor tops aligned with measured text rows" in {
    val rows = TextRowMetrics(rect, metrics, rowLineHeightPx = 24, usesMeasuredLayout = true)

    rows.cursorTopPx(1) shouldBe rows.lineTopPx(1)
  }

  it should "optically lift grid cursors without escaping the content top" in {
    val rows = TextRowMetrics(rect, metrics, rowLineHeightPx = 16, usesMeasuredLayout = false)

    rows.cursorTopPx(0) shouldBe rows.lineTopPx(0)
    rows.cursorTopPx(1) should be < rows.lineTopPx(1)
    rows.cursorTopPx(1) should be >= metrics.toPixelY(rect.y)
  }

  // -- #1215: a terminal cell grid's row height IS the pixel unit (rowLineHeightPx == 1), so the optical lift meant
  // to nudge a real sub-pixel font's cursor cannot cost a whole row (or more) there. --
  it should "not lift a cell-grid cursor at all once the row has no spare pixels (a 1px == 1 terminal row grid)" in {
    val oneRowPerCell = CellMetrics(charWidth = 1, lineHeight = 1, ascent = 0)
    val rows          = TextRowMetrics(rect, oneRowPerCell, rowLineHeightPx = 1, usesMeasuredLayout = false)

    rows.cursorTopPx(3) shouldBe rows.lineTopPx(3)
  }

  it should "enforce viewport and content bounds for visible rows" in {
    val rows = TextRowMetrics(rect, metrics, rowLineHeightPx = 24, usesMeasuredLayout = true)

    rows.lineVisible(0, viewportHeightCells = 7) shouldBe true
    rows.lineVisible(3, viewportHeightCells = 6) shouldBe false
    rows.lineVisible(4, viewportHeightCells = 20) shouldBe false
  }
end TextRowMetricsSpec
