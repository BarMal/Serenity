package com.serenity.ui.tui

import TuiScenarios.*

/** What a TUI session actually writes to the terminal, as opposed to what it draws. Bytes are the cost model here: a
  * frame that changed nothing must cost nothing (#1170's zero-idle-wakeup contract), a one-character edit must cost a
  * one-character diff rather than a repaint, and the caret must end up where the caret belongs regardless of what the
  * content diff did to the terminal's cursor on its way past (#1215).
  */
class TuiRedrawSpec extends TuiSpec:

  private val Esc = 0x1b.toChar

  /** A full repaint of the default terminal runs to roughly 12,600 bytes. Anything an order of magnitude under that is
    * a diff rather than a repaint, which is what these budgets are really asserting.
    */
  private val DiffBudgetBytes = 2000

  "a settled screen" should "write nothing at all when nothing has changed" in
    runTui(TuiEnvironment.withFile("unchanging")) {
      for
        _      <- settledScreen
        again  <- screen
        andOne <- screen
      yield
        again.emitted shouldBe ""
        andOne.emitted shouldBe ""
    }

  "typing one character" should "emit a diff of that row and the status bar, not a repaint" in
    runTui(TuiEnvironment.withFile("abc")) {
      for
        before <- settledScreen
        _      <- lineEnd
        _      <- settledScreen
        _      <- typeText("d")
        after  <- screen
      yield
        after.rowText(1).stripTrailing shouldBe " 1 abcd"
        // The edited row, the status bar's column readout, and the header gaining its "- unsaved" marker.
        after.changedRows(before) shouldBe Set(0, 1, after.height - 1)
        after.emitted.length should be < DiffBudgetBytes
    }

  it should "wrap every non-empty flush in synchronized-update brackets" in
    runTui(TuiEnvironment.withFile("sync")) {
      for
        _     <- settledScreen
        _     <- typeText("!")
        after <- screen
      yield
        after.emitted should startWith(s"$Esc[?2026h")
        after.emitted should endWith(s"$Esc[?2026l")
    }

  "moving the caret alone" should "cost a cursor-position escape rather than a content repaint" in
    runTui(TuiEnvironment.withFile("caret movement")) {
      for
        before <- settledScreen
        _      <- arrowRight
        after  <- screen
      yield
        // The document itself is untouched; only the status bar's column and the caret move.
        after.rowText(1) shouldBe before.rowText(1)
        after.caret shouldBe (before.caret._1 + 1, before.caret._2)
        after.emitted should include(s"$Esc[?25h")
        after.emitted.length should be < DiffBudgetBytes
    }

  "a content change away from the caret" should "still leave the terminal cursor on the caret" in
    runTui(TuiEnvironment.withFile("line one\nline two")) {
      for
        _      <- arrowDown
        _      <- settledScreen
        before <- screen
        _      <- typeText("x")
        after  <- screen
      yield
        // #1215: the content diff's own CUP writes leave the terminal cursor wherever the last cell was drawn -- the
        // status bar, here -- so the caret escape has to be re-asserted after them, every time.
        after.caret shouldBe (before.caret._1 + 1, 2)
        val caretEscape = s"$Esc[${after.caret._2 + 1};${after.caret._1 + 1}H"
        after.emitted should include(caretEscape)
        after.emitted.lastIndexOf(caretEscape) should be > after.emitted.indexOf("Col")
    }

  "opening and dismissing an overlay" should "repaint the covered region and nothing outside it" in
    runTui(TuiEnvironment.withFile("document body")) {
      for
        before <- settledScreen
        _      <- openCommandPalette
        opened <- settledScreen
        _      <- escape
        closed <- settledScreen
      yield
        opened.changedRows(before) should not be empty
        // The editor's own first row is under the overlay's panel, but its text is unchanged throughout.
        closed.rowText(1) shouldBe before.rowText(1)
        closed.statusBar shouldBe before.statusBar
        closed.changedCells(before) shouldBe empty
    }

  "a resize" should "cost one full repaint and then go quiet again" in
    runTui(TuiEnvironment.withFile("resize cost")) {
      for
        _       <- settledScreen
        _       <- resize(TuiViewport.HalfScreen)
        resized <- screen
        settled <- settledScreen
        quiet   <- screen
      yield
        // No previous frame at the new size, so the differ emits a clear-and-repaint rather than a diff.
        resized.emitted should include(s"$Esc[2J")
        settled.width shouldBe 100
        quiet.emitted shouldBe ""
    }

  "scrolling a large document" should "emit only the rows that actually changed" in
    runTui(TuiEnvironment.withLines(5000)) {
      for
        before <- settledScreen
        _      <- arrowDown
        after  <- screen
      yield
        // One line down inside the visible window scrolls nothing: the caret and status bar move, the text does not.
        after.rowText(1) shouldBe before.rowText(1)
        after.changedRows(before) shouldBe Set(after.height - 1)
        after.emitted.length should be < DiffBudgetBytes
    }
end TuiRedrawSpec
