package com.serenity.animation

import com.googlecode.lanterna.TextColor
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class FlowAnimationBuilderSpec extends AnyFlatSpec with Matchers:

  private val black = new TextColor.RGB(0, 0, 0)
  private val white = new TextColor.RGB(255, 255, 255)
  private val red   = new TextColor.RGB(255, 0, 0)
  private val blue  = new TextColor.RGB(0, 0, 255)

  /** 3 columns (0–2) × 2 rows (0–1), uniform colour pair */
  private def grid3x2(start: TextColor = black, end: TextColor = white): Map[CharacterKey, CellAnimation] =
    (for col <- 0 until 3; row <- 0 until 2 yield
      CharacterKey(col, row) -> CellAnimation('x', start, end)
    ).toMap

  /** 2 columns (0–1) × 3 rows (0–2), uniform colour pair */
  private def grid2x3(start: TextColor = black, end: TextColor = white): Map[CharacterKey, CellAnimation] =
    (for col <- 0 until 2; row <- 0 until 3 yield
      CharacterKey(col, row) -> CellAnimation('x', start, end)
    ).toMap

  // ── Group 1: Stagger lengths — ByColumn ──────────────────────────────────

  "FlowAnimationBuilder" should "stagger colorSteps length by column in forward direction" in {
    val steps  = 5
    val result = FlowAnimationBuilder.build(grid3x2(), FlowDirection.ByColumn, SweepDirection.Forward, steps)

    for row <- 0 until 2 do
      result(CharacterKey(0, row)).colorSteps should have length steps
      result(CharacterKey(1, row)).colorSteps should have length (1 + steps)
      result(CharacterKey(2, row)).colorSteps should have length (2 + steps)
  }

  it should "stagger colorSteps length by column in backward direction" in {
    val steps  = 5
    val result = FlowAnimationBuilder.build(grid3x2(), FlowDirection.ByColumn, SweepDirection.Backward, steps)

    for row <- 0 until 2 do
      result(CharacterKey(0, row)).colorSteps should have length (2 + steps)
      result(CharacterKey(1, row)).colorSteps should have length (1 + steps)
      result(CharacterKey(2, row)).colorSteps should have length steps
  }

  // ── Group 2: Stagger lengths — ByRow ─────────────────────────────────────

  it should "stagger colorSteps length by row in forward direction" in {
    val steps  = 4
    val result = FlowAnimationBuilder.build(grid2x3(), FlowDirection.ByRow, SweepDirection.Forward, steps)

    for col <- 0 until 2 do
      result(CharacterKey(col, 0)).colorSteps should have length steps
      result(CharacterKey(col, 1)).colorSteps should have length (1 + steps)
      result(CharacterKey(col, 2)).colorSteps should have length (2 + steps)
  }

  it should "stagger colorSteps length by row in backward direction" in {
    val steps  = 4
    val result = FlowAnimationBuilder.build(grid2x3(), FlowDirection.ByRow, SweepDirection.Backward, steps)

    for col <- 0 until 2 do
      result(CharacterKey(col, 0)).colorSteps should have length (2 + steps)
      result(CharacterKey(col, 1)).colorSteps should have length (1 + steps)
      result(CharacterKey(col, 2)).colorSteps should have length steps
  }

  // ── Group 3: Stagger content ──────────────────────────────────────────────

  it should "fill padding frames with the cell's start colour" in {
    val steps  = 4
    val result = FlowAnimationBuilder.build(grid3x2(), FlowDirection.ByColumn, SweepDirection.Forward, steps)
    // Column 2 has a stagger of 2 so the first two entries must hold startColor
    result(CharacterKey(2, 0)).colorSteps.take(2) shouldEqual List(black, black)
  }

  it should "match post-padding color steps to RgbInterpolator output" in {
    val steps    = 4
    val result   = FlowAnimationBuilder.build(grid3x2(), FlowDirection.ByColumn, SweepDirection.Forward, steps)
    val expected = RgbInterpolator.interpolate(black, white, steps)
    // Column 0 has no padding — colorSteps is exactly the interpolation
    result(CharacterKey(0, 0)).colorSteps shouldEqual expected
    // Column 2 has 2 padding frames — dropping them yields the same interpolation
    result(CharacterKey(2, 0)).colorSteps.drop(2) shouldEqual expected
  }

  // ── Group 4: Per-cell colour support ─────────────────────────────────────

  it should "apply stagger offset based on position, not on cell colour" in {
    val steps = 4
    val cells = Map(
      CharacterKey(0, 0) -> CellAnimation('a', black, white),
      CharacterKey(1, 0) -> CellAnimation('b', red,   blue)
    )
    val result = FlowAnimationBuilder.build(cells, FlowDirection.ByColumn, SweepDirection.Forward, steps)

    result(CharacterKey(0, 0)).colorSteps should have length steps
    result(CharacterKey(1, 0)).colorSteps should have length (1 + steps)
  }

  it should "produce distinct color sequences for cells with different colour pairs at the same stagger offset" in {
    val steps = 4
    // Column 1 has two cells with different colour pairs; both share stagger offset 1
    val cells = Map(
      CharacterKey(0, 0) -> CellAnimation('a', black, white),
      CharacterKey(1, 0) -> CellAnimation('b', red,   blue),
      CharacterKey(1, 1) -> CellAnimation('c', black, white)
    )
    val result = FlowAnimationBuilder.build(cells, FlowDirection.ByColumn, SweepDirection.Forward, steps)

    val col1Row0 = result(CharacterKey(1, 0))
    val col1Row1 = result(CharacterKey(1, 1))

    col1Row0.colorSteps should have length col1Row1.colorSteps.length
    col1Row0.colorSteps.drop(1) should not equal col1Row1.colorSteps.drop(1)
  }

  // ── Group 5: Uniformity within a stripe ──────────────────────────────────

  it should "give all cells in the leading stripe exactly `steps` colorSteps with no padding" in {
    val steps  = 6
    val result = FlowAnimationBuilder.build(grid3x2(), FlowDirection.ByColumn, SweepDirection.Forward, steps)

    result(CharacterKey(0, 0)).colorSteps should have length steps
    result(CharacterKey(0, 1)).colorSteps should have length steps
  }

  it should "give all cells in the same column identical colorSteps length regardless of row" in {
    val steps  = 5
    val result = FlowAnimationBuilder.build(grid3x2(), FlowDirection.ByColumn, SweepDirection.Forward, steps)

    for col <- 0 until 3 do
      val lengths = (0 until 2).map(row => result(CharacterKey(col, row)).colorSteps.length)
      lengths.distinct should have length 1
  }

  // ── Group 6: Edge cases ───────────────────────────────────────────────────

  it should "produce a single cell with no padding and colorSteps equal to the interpolation" in {
    val steps  = 4
    val cells  = Map(CharacterKey(3, 7) -> CellAnimation('z', black, white))
    val result = FlowAnimationBuilder.build(cells, FlowDirection.ByColumn, SweepDirection.Forward, steps)

    result(CharacterKey(3, 7)).colorSteps should have length steps
    result(CharacterKey(3, 7)).colorSteps shouldEqual RgbInterpolator.interpolate(black, white, steps)
  }

  it should "give all cells offset 0 when the element spans a single column with ByColumn direction" in {
    val steps = 4
    val cells = (0 until 3).map(row => CharacterKey(2, row) -> CellAnimation('x', black, white)).toMap
    val result = FlowAnimationBuilder.build(cells, FlowDirection.ByColumn, SweepDirection.Forward, steps)

    for row <- 0 until 3 do
      result(CharacterKey(2, row)).colorSteps should have length steps
  }

  it should "give all cells offset 0 when the element spans a single row with ByRow direction" in {
    val steps = 4
    val cells = (0 until 3).map(col => CharacterKey(col, 5) -> CellAnimation('x', black, white)).toMap
    val result = FlowAnimationBuilder.build(cells, FlowDirection.ByRow, SweepDirection.Forward, steps)

    for col <- 0 until 3 do
      result(CharacterKey(col, 5)).colorSteps should have length steps
  }

  it should "compute stagger offset relative to the minimum column in the element" in {
    val steps = 4
    // Element occupies columns 5–7; column 5 is the leading edge, offset 0
    val cells = (for col <- 5 until 8; row <- 0 until 2 yield
      CharacterKey(col, row) -> CellAnimation('x', black, white)
    ).toMap
    val result = FlowAnimationBuilder.build(cells, FlowDirection.ByColumn, SweepDirection.Forward, steps)

    for row <- 0 until 2 do
      result(CharacterKey(5, row)).colorSteps should have length steps
      result(CharacterKey(6, row)).colorSteps should have length (1 + steps)
      result(CharacterKey(7, row)).colorSteps should have length (2 + steps)
  }
