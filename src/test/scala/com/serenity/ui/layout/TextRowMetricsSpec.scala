package com.serenity.ui.layout

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class TextRowMetricsSpec extends AnyFlatSpec with Matchers:

  private val pixelGrid = CellMetrics(charWidth = 8, lineHeight = 1, ascent = 1)
  private val rect      = LayoutRect(0, 0, 100, 100)

  "Measured TextRowMetrics with per-line heights" should "stack rows by their own heights" in {
    val metrics = TextRowMetrics(
      rect,
      pixelGrid,
      rowLineHeightPx = 20,
      usesMeasuredLayout = true,
      rowHeightsPx = Vector(20, 40, 20)
    )
    metrics.lineTopPx(0) shouldBe 0
    metrics.lineTopPx(1) shouldBe 20
    metrics.lineTopPx(2) shouldBe 60
    metrics.rowHeightPx(1) shouldBe 40
  }

  it should "fall back to the uniform height for a row past the per-line table" in {
    val metrics =
      TextRowMetrics(rect, pixelGrid, rowLineHeightPx = 20, usesMeasuredLayout = true, rowHeightsPx = Vector(30))
    metrics.lineTopPx(0) shouldBe 0
    metrics.lineTopPx(1) shouldBe 30
    metrics.lineTopPx(2) shouldBe 50
  }

  it should "reproduce uniform stacking when no per-line heights are given" in {
    val metrics = TextRowMetrics(rect, pixelGrid, rowLineHeightPx = 16, usesMeasuredLayout = true)
    metrics.lineTopPx(0) shouldBe 0
    metrics.lineTopPx(1) shouldBe 16
    metrics.lineTopPx(3) shouldBe 48
    metrics.rowHeightPx(2) shouldBe 16
  }

  "visualRowAt" should "reverse a pixel Y to the visual row that owns it, honoring per-line heights" in {
    val metrics = TextRowMetrics(
      rect,
      pixelGrid,
      rowLineHeightPx = 20,
      usesMeasuredLayout = true,
      rowHeightsPx = Vector(20, 40, 20)
    )
    metrics.visualRowAt(0) shouldBe 0
    metrics.visualRowAt(19) shouldBe 0
    metrics.visualRowAt(20) shouldBe 1
    metrics.visualRowAt(59) shouldBe 1 // row 1 spans [20, 60)
    metrics.visualRowAt(60) shouldBe 2
    metrics.visualRowAt(1000) shouldBe 2 // below the last row clamps to it
    metrics.visualRowAt(-5) shouldBe 0
  }

  it should "divide uniformly for a measured layout with no per-line table" in {
    val metrics = TextRowMetrics(rect, pixelGrid, rowLineHeightPx = 16, usesMeasuredLayout = true)
    metrics.visualRowAt(0) shouldBe 0
    metrics.visualRowAt(33) shouldBe 2
  }

  // Rows of 20, 40 and 20 high; the middle one has 10 above and 5 below, the last 8 below.
  private val spaced = TextRowMetrics(
    rect,
    pixelGrid,
    rowLineHeightPx = 20,
    usesMeasuredLayout = true,
    rowHeightsPx = Vector(20, 40, 20),
    rowSpaceBeforePx = Vector(0, 10, 0),
    rowSpaceAfterPx = Vector(0, 5, 8)
  )

  "Measured TextRowMetrics with space around rows" should "put a row's text below the space above it" in {
    spaced.lineTopPx(0) shouldBe 0
    spaced.lineTopPx(1) shouldBe 30
    spaced.lineTopPx(2) shouldBe 75
    spaced.rowHeightPx(1) shouldBe 40
  }

  it should "give each row a band that holds its space and tiles with its neighbours" in {
    spaced.bandTopPx(0) shouldBe 0
    spaced.bandBottomPx(0) shouldBe 20
    spaced.bandTopPx(1) shouldBe 20
    spaced.bandBottomPx(1) shouldBe 75
    spaced.bandTopPx(2) shouldBe 75
    spaced.bandBottomPx(2) shouldBe 103
  }

  it should "stack rows past the table below the last band" in {
    spaced.lineTopPx(3) shouldBe 103
    spaced.lineTopPx(4) shouldBe 123
  }

  it should "send a pixel in the space above or below a row to that row" in {
    spaced.visualRowAt(19) shouldBe 0
    spaced.visualRowAt(20) shouldBe 1
    spaced.visualRowAt(25) shouldBe 1
    spaced.visualRowAt(29) shouldBe 1
    spaced.visualRowAt(30) shouldBe 1
    spaced.visualRowAt(70) shouldBe 1
    spaced.visualRowAt(74) shouldBe 1
    spaced.visualRowAt(75) shouldBe 2
    spaced.visualRowAt(102) shouldBe 2
    spaced.visualRowAt(1000) shouldBe 2
    spaced.visualRowAt(-5) shouldBe 0
  }

  it should "stack exactly as before when no row has any space" in {
    val plain = TextRowMetrics(
      rect,
      pixelGrid,
      rowLineHeightPx = 20,
      usesMeasuredLayout = true,
      rowHeightsPx = Vector(20, 40, 20),
      rowSpaceBeforePx = Vector(0, 0, 0),
      rowSpaceAfterPx = Vector(0, 0, 0)
    )
    val unspaced = plain.copy(rowSpaceBeforePx = Vector.empty, rowSpaceAfterPx = Vector.empty)

    (0 to 4).map(row => (plain.lineTopPx(row), plain.bandTopPx(row), plain.bandBottomPx(row))) shouldBe
      (0 to 4).map(row => (unspaced.lineTopPx(row), unspaced.bandTopPx(row), unspaced.bandBottomPx(row)))
    plain.bandBottomPx(2) shouldBe 80
  }

  // Cell rows of one pixel, the unit a terminal draws in; a blank row above row 1 and one below row 2.
  private val cellRect = LayoutRect(0, 2, 100, 20)

  private val cells = TextRowMetrics(
    cellRect,
    pixelGrid,
    rowLineHeightPx = 1,
    usesMeasuredLayout = false,
    rowSpaceBeforePx = Vector(0, 1, 0, 0),
    rowSpaceAfterPx = Vector(0, 0, 1, 0)
  )

  "Cell TextRowMetrics with blank rows around a row" should "number the screen rows past the blank ones" in {
    cells.screenRow(0) shouldBe 2
    cells.screenRow(1) shouldBe 4
    cells.screenRow(2) shouldBe 5
    cells.screenRow(3) shouldBe 7
    cells.lineTopPx(1) shouldBe 4
    cells.lineTopPx(3) shouldBe 7
  }

  it should "count the blank rows above a row" in {
    cells.blankRowsAbove(0) shouldBe 0
    cells.blankRowsAbove(1) shouldBe 1
    cells.blankRowsAbove(2) shouldBe 1
    cells.blankRowsAbove(3) shouldBe 2
  }

  it should "map a screen row, blank or not, to the row that owns it" in {
    cells.visualRowAtScreenRow(2) shouldBe 0
    cells.visualRowAtScreenRow(3) shouldBe 1
    cells.visualRowAtScreenRow(4) shouldBe 1
    cells.visualRowAtScreenRow(5) shouldBe 2
    cells.visualRowAtScreenRow(6) shouldBe 2
    cells.visualRowAtScreenRow(7) shouldBe 3
    cells.visualRowAtScreenRow(0) shouldBe 0
    cells.visualRowAtScreenRow(40) shouldBe 3
  }

  it should "fit and show only the rows whose text lies inside the content rect" in {
    val short = cells.copy(contentRect = LayoutRect(0, 2, 100, 5))
    short.lineFits(2) shouldBe true
    short.lineFits(3) shouldBe false
    short.lineVisible(2, viewportHeightCells = 40) shouldBe true
    short.lineVisible(3, viewportHeightCells = 40) shouldBe false
  }

  it should "number the screen rows by the row alone when nothing is blank" in {
    val plain = cells.copy(rowSpaceBeforePx = Vector.empty, rowSpaceAfterPx = Vector.empty)

    (0 to 4).map(plain.screenRow) shouldBe (2 to 6)
    (0 to 4).map(plain.blankRowsAbove).distinct shouldBe Vector(0)
    plain.visualRowAtScreenRow(4) shouldBe 2
  }

  "TextRowMetrics for a snapshot" should "carry each visual line's own space, and none when no line has any" in {
    import com.serenity.state.models.{MeasuredRow, TextVisualLine}

    def line(before: Int, after: Int): TextVisualLine =
      TextVisualLine(
        0,
        MeasuredRow.fromStops(0, 1, "x", 1.0f, Vector.empty, heightPx = 20, ascentPx = 15),
        0.0f,
        before,
        after
      )

    def snapshotOf(lines: TextVisualLine*) =
      TextLayoutSnapshot(
        lines.toVector,
        panelWidthPx = 100,
        lineHeightPx = 20,
        ascentPx = 15,
        usesMeasuredLayout = true
      )

    val withSpace = TextRowMetrics.forSnapshot(rect, pixelGrid, snapshotOf(line(0, 4), line(6, 0)), measured = true)
    withSpace.lineTopPx(1) shouldBe 30
    withSpace.bandBottomPx(1) shouldBe 50

    val without = TextRowMetrics.forSnapshot(rect, pixelGrid, snapshotOf(line(0, 0), line(0, 0)), measured = true)
    without.rowSpaceBeforePx shouldBe Vector.empty
    without.rowSpaceAfterPx shouldBe Vector.empty
  }
