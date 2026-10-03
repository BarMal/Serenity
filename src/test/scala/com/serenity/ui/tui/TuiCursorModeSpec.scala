package com.serenity.ui.tui

import cats.syntax.all.*
import com.serenity.config.CursorMode

/** Who owns the caret in a terminal.
  *
  * A cell-addressed terminal has no content path to paint a caret into (`fillPixelRect` is a no-op there), so the
  * terminal's own cursor is the caret -- blink delegates its blinking to DECSCUSR.
  *
  * What it must never do is leave the caret somewhere the editing position is not. `TerminalAnsiDiff`'s writes leave
  * the real cursor wherever the frame's last cell was drawn -- the bottom of the screen for any frame that repaints the
  * status bar -- so a frame that does not re-assert the caret parks it there (#1215).
  */
class TuiCursorModeSpec extends TuiSpec:

  private val ContentColumn = 3

  private def env(mode: CursorMode) =
    TuiEnvironment.withFile("alpha\nbeta\ngamma\ndelta").withConfig(_.withCursorMode(mode))

  /** Where editing leaves the caret: line 2, three characters in. */
  private def moveIntoTheText = press(ArrowDown) >> pressAll(ArrowRight, ArrowRight, ArrowRight)

  "blink mode" should "leave the caret at the editing position after a content frame" in
    runTui(env(CursorMode.Blink)) {
      for
        _       <- settledScreen
        _       <- moveIntoTheText
        painted <- settledScreen
      yield
        painted.caret shouldBe (ContentColumn + 3, 2)
        painted.caretVisible shouldBe true
    }

  it should "keep it there while it blinks" in runTui(env(CursorMode.Blink)) {
    for
      _       <- settledScreen
      _       <- moveIntoTheText
      painted <- settledScreen
      frames  <- (1 to 6).toList.traverse(_ => idleCursorScreen.map(screen => (screen.caret, screen.caretVisible)))
    yield
      frames.map(_._1).distinct shouldBe List(painted.caret)
      frames.map(_._2) should contain(false)
  }
end TuiCursorModeSpec
