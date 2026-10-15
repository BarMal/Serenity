package com.serenity.ui.tui

import cats.syntax.all.*
import com.serenity.ui.layout.ViewportSize

/** End parks the caret at the right edge of its wrapped row, and the vertical moves that follow carry that edge x to
  * the next row: they have to land on that row's own right edge, not on the column it shares with the row after it.
  */
class TuiEndThenVerticalSpec extends TuiSpec:

  private val text = (0 until 100).map(index => f"w$index%03d ").mkString

  private val environment =
    TuiEnvironment
      .withFile(text)
      .withViewport(ViewportSize(40, 14))
      .withConfig(_.withWordWrap(true).withLineNumbers(false))

  private val caretRow: TuiScript[Int] = settledScreen.map(_.caret._2)

  "Down after End on a wrapped row" should "step to the next row each time" in runTui(environment) {
    for
      _      <- press(ArrowDown)
      _      <- press(End)
      first  <- caretRow
      _      <- press(ArrowDown)
      second <- caretRow
      _      <- press(End)
      _      <- press(ArrowDown)
      third  <- caretRow
    yield (second - first, third - second) shouldBe (1, 1)
  }

  "Up after End on a wrapped row" should "step to the previous row each time" in runTui(environment) {
    for
      _      <- List.fill(4)(ArrowDown).traverse_(press)
      _      <- press(End)
      first  <- caretRow
      _      <- press(ArrowUp)
      second <- caretRow
      _      <- press(ArrowUp)
      third  <- caretRow
    yield (first - second, second - third) shouldBe (1, 1)
  }
