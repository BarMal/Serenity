package com.serenity.ui.tui

import cats.syntax.all.*
import com.serenity.config.AppConfig
import com.serenity.ui.layout.ViewportSize

/** #2570 item 2. Typewriter scrolling holds the caret's row at the middle of the pane however the caret got there, a
  * click included. Without it the view moves only to keep the caret three rows clear of the pane's top and bottom
  * edges, so the text under the caret stays put while it moves within the pane.
  */
class TuiTypewriterScrollingSpec extends TuiSpec:

  // A 14-row terminal: a title row, 12 body rows (1..12) and a status row.
  private val terminalSize = ViewportSize(60, 14)
  private val middleRow    = 7
  private val firstRow     = 1
  private val lastRow      = 12
  private val margin       = 3

  private val document = (0 until 200).map(index => f"L$index%04d").mkString("\n")

  private def environment(config: AppConfig => AppConfig) =
    TuiEnvironment.withFile(document).withViewport(terminalSize).withConfig(c => config(c.withLineNumbers(false)))

  private val caretRow: TuiScript[Int] = settledScreen.map(_.caret._2)

  private def topLine: TuiScript[Option[Int]] = state.map(current => focusedBuffer(current).map(_.viewport.topLine))

  "With typewriter scrolling, a click on a row away from the middle" should "bring the clicked line to the middle" in
    runTui(environment(_.withTypewriterScrolling(true))) {
      for
        _     <- (0 until 30).toList.traverse_(_ => press(ArrowDown))
        _     <- click(3, 3)
        upper <- caretRow
        _     <- click(3, 11)
        lower <- caretRow
      yield (upper, lower) shouldBe ((middleRow, middleRow))
    }

  "Without typewriter scrolling, moving down from the top" should "leave the view put until the caret nears the bottom" in
    runTui(environment(_.withTypewriterScrolling(false))) {
      for
        rows <- (1 to 8).toList.traverse(_ => press(ArrowDown) >> caretRow)
        top  <- topLine
      yield
        rows shouldBe (2 to 9).toList
        top shouldBe Some(0)
    }

  it should "then scroll a row at a time, keeping three rows below the caret" in
    runTui(environment(_.withTypewriterScrolling(false))) {
      for
        _    <- (1 to 8).toList.traverse_(_ => press(ArrowDown))
        rows <- (1 to 6).toList.traverse(_ => press(ArrowDown) >> caretRow)
        top  <- topLine
      yield
        rows shouldBe List.fill(6)(lastRow - margin)
        top shouldBe Some(6)
    }

  "Without typewriter scrolling, moving back up" should "leave the view put until the caret nears the top" in
    runTui(environment(_.withTypewriterScrolling(false))) {
      for
        _      <- (1 to 30).toList.traverse_(_ => press(ArrowDown))
        before <- topLine
        rows   <- (1 to 5).toList.traverse(_ => press(ArrowUp) >> caretRow)
        after  <- topLine
        _      <- press(ArrowUp)
        held   <- caretRow
      yield
        rows shouldBe (8 to 4 by -1).toList
        after shouldBe before
        held shouldBe firstRow + margin
    }

  "Without typewriter scrolling, a page down" should "keep the caret clear of the pane's edges" in
    runTui(environment(_.withTypewriterScrolling(false))) {
      for rows <- (1 to 6).toList.traverse(_ => press(PageDown) >> caretRow)
      yield rows.filterNot(row => row >= firstRow + margin && row <= lastRow - margin) shouldBe Nil
    }

  "Without typewriter scrolling, a click inside the pane" should "leave the view where it is" in
    runTui(environment(_.withTypewriterScrolling(false))) {
      for
        _      <- (1 to 30).toList.traverse_(_ => press(ArrowDown))
        before <- topLine
        _      <- click(3, 2)
        after  <- topLine
      yield after shouldBe before
    }
