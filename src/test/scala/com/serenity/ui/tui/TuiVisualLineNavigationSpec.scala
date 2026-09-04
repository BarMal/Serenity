package com.serenity.ui.tui

import com.serenity.state.models.{AppState, CursorPosition}

import TuiScenarios.*

/** What "navigate by visual line" actually does in a wrapped terminal document, setting by setting.
  *
  * `EditorEventReducer.useVisualLineNavigation` gates on `wordWrapEnabled && visualLineCursorNavigation`, so there are
  * three cases worth pinning: both on (visual rows), the navigation flag off (logical lines), and word wrap off
  * (logical lines, because there are no visual rows to speak of). Up and Down honour all three.
  *
  * Home and End do not, and the two `pendingUntilFixed` tests below assert what they should do rather than what they
  * currently do: End on a wrapped row leaves the caret on the boundary column shared by that row's end and the next
  * row's start, and the renderer resolves that column to the *next* row -- so pressing End on a wrapped line visibly
  * moves the caret to the far left of the row below, and Home from there does not bring it back. It is the same
  * earlier-row/later-row ambiguity #1266 fixed for vertical movement (`NavigationGeometry.visualRowIndexFor`), still
  * present on the horizontal ones: `homeTarget`/`endTarget` in `EditorEventReducer` resolve the column through
  * `visualLineFor` without that disambiguation.
  *
  * When it is fixed those two start passing, and `pendingUntilFixed` fails to ask for its own removal.
  */
class TuiVisualLineNavigationSpec extends TuiSpec:

  private val paragraph = "the quick brown fox jumps over the lazy dog and keeps running onward " * 6
  private val prose     = (0 until 8).map(index => s"Paragraph $index. $paragraph").mkString("\n\n")

  /** The first content column: the gutter is `" 1 "` wide for this document. */
  private val ContentColumn = 3

  private def env(wrap: Boolean = true, visualNav: Boolean = true) =
    TuiEnvironment.withFile(prose).withConfig(_.withWordWrap(wrap).withVisualLineCursorNavigation(visualNav))

  private def cursorOf(current: AppState): Option[CursorPosition] =
    focusedBuffer(current).flatMap(_.editing.cursors.headOption)

  private def cursorColumn(current: AppState): Int = cursorOf(current).map(_.column).getOrElse(-1)
  private def cursorLine(current: AppState): Int   = cursorOf(current).map(_.line).getOrElse(-1)

  // -- Up and Down honour both settings ------------------------------------------------------------------------------

  "Down with wrap and visual-line navigation on" should "move within the wrapped line, not to the next one" in
    runTui(env()) {
      for
        _     <- settledScreen
        _     <- arrowDown
        after <- state
      yield
        // Still inside the first logical line, one visual row further along it.
        cursorLine(after) shouldBe 0
        cursorColumn(after) should be > 100
    }

  "Down with visual-line navigation off" should "move by logical line instead" in
    runTui(env(visualNav = false)) {
      for
        _     <- settledScreen
        _     <- pressAll(TuiKeys.ArrowDown, TuiKeys.ArrowDown, TuiKeys.ArrowDown)
        after <- state
      yield
        // Three presses, three logical lines -- the wrapped rows in between are skipped over entirely.
        cursorLine(after) shouldBe 3
        cursorColumn(after) shouldBe 0
    }

  "Down with word wrap off" should "move by logical line whatever the navigation flag says" in
    runTui(env(wrap = false, visualNav = true)) {
      for
        _       <- settledScreen
        _       <- pressAll(TuiKeys.ArrowDown, TuiKeys.ArrowDown, TuiKeys.ArrowDown)
        after   <- state
        current <- screen
      yield
        cursorLine(after) shouldBe 3
        // And nothing is wrapped on screen either: one logical line per row, clipped at the terminal's width.
        current.rowsContaining("quick brown") should have size 8
    }

  "the toggle command" should "change navigation behaviour in the running session" in runTui(env()) {
    for
      _ <- runCommand("toggle visual line")
      _ <- dismissSurfaces()
      _ <- verifyState("flag off")(current =>
        current.persisted.config.surfaceConfig.visualLineCursorNavigation shouldBe false
      )
      _     <- pressAll(TuiKeys.ArrowDown, TuiKeys.ArrowDown, TuiKeys.ArrowDown)
      after <- state
    yield
      // Visual-line navigation was on when the session started; after the toggle, Down walks logical lines.
      cursorLine(after) shouldBe 3
  }

  // -- Home and End do not (the defect) ------------------------------------------------------------------------------

  /** PENDING -- OPEN DEFECT. Asserts the behaviour that is wanted, and is reported pending until it holds. */
  "End on a wrapped row" should "put the caret at the end of that row, not the start of the row below" in
    pendingUntilFixed {
      runTui(env()) {
        for
          _      <- arrowDown // onto the second visual row of the first logical line
          before <- settledScreen
          onRow  <- state
          _      <- lineEnd
          after  <- settledScreen
          ended  <- state
        yield
          // The caret starts at the left edge of a wrapped row with plenty of text to its right...
          before.caret._1 shouldBe ContentColumn
          cursorColumn(onRow) should be > 100
          // ...and End moves it forward within that same row.
          cursorColumn(ended) should be > cursorColumn(onRow)
          // The caret stays on the row the user pressed End on, at its right-hand end -- not at the far left of the
          // row below, which is the opposite corner of the screen.
          after.caret._2 shouldBe before.caret._2
          after.caret._1 should be > 100
      }
    }

  /** PENDING -- OPEN DEFECT. Asserts the behaviour that is wanted, and is reported pending until it holds. */
  it should "be undone by Home, which returns the caret to that row's own start" in pendingUntilFixed {
    runTui(env()) {
      for
        _         <- arrowDown
        onRow     <- state
        _         <- lineEnd
        afterEnd  <- state
        _         <- lineStart
        afterHome <- state
        current   <- settledScreen
      yield
        // End moved somewhere, and Home brings the caret back to where the row began.
        cursorColumn(afterEnd) should not be cursorColumn(onRow)
        cursorColumn(afterHome) shouldBe cursorColumn(onRow)
        current.caret._1 shouldBe ContentColumn
    }
  }

  "End on the final visual row of a wrapped line" should "land on the logical line's end, where the two agree" in
    runTui(env()) {
      for
        _     <- pressAll(TuiKeys.ArrowDown, TuiKeys.ArrowDown)
        _     <- lineEnd
        after <- state
        text  <- documentText
      yield
        val lineLength = text.map(_.linesIterator.next().length).getOrElse(0)
        // The last visual row ends where the logical line ends, so there is no boundary to be ambiguous about.
        cursorColumn(after) shouldBe lineLength
        cursorLine(after) shouldBe 0
    }

  "End with visual-line navigation off" should "go to the logical line's end from any wrapped row" in
    runTui(env(visualNav = false)) {
      for
        _     <- arrowDown // logical line 1, the blank paragraph break
        _     <- arrowUp   // back to logical line 0, column 0
        _     <- lineEnd
        after <- state
        text  <- documentText
      yield
        val lineLength = text.map(_.linesIterator.next().length).getOrElse(0)
        cursorColumn(after) shouldBe lineLength
        cursorLine(after) shouldBe 0
    }

  "Home on a wrapped row reached by Down" should "return to that row's own start" in runTui(env()) {
    for
      _         <- arrowDown
      onRow     <- state
      _         <- pressAll(List.fill(20)(TuiKeys.ArrowRight)*)
      _         <- lineStart
      after     <- state
      screenNow <- settledScreen
    yield
      // Home does work when the caret is unambiguously inside a row rather than on its boundary.
      cursorColumn(after) shouldBe cursorColumn(onRow)
      screenNow.caret._1 shouldBe ContentColumn
  }
end TuiVisualLineNavigationSpec
