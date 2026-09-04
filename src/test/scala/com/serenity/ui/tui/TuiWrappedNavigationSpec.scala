package com.serenity.ui.tui

import cats.syntax.all.*
import com.serenity.config.{CursorInfoBarPlacement, CursorInfoBarSegment}
import com.serenity.state.models.CursorPosition

/** Regression cover for the three TUI cursor/word-wrap defects fixed in #1266, each of which was invisible to
  * unit-level tests because each only appears once real input, real state and a real incremental repaint are in the
  * same loop:
  *
  *   - the floating cursor info bar left its opaque background behind on the rows it moved off, because
  *     `DamageProducer` never saw the derived surface move and so never asked the renderer to clear them;
  *   - vertical navigation at a wrap-boundary column disagreed with the renderer about which visual row the caret was
  *     on, so Up did nothing and Down skipped a row;
  *   - the viewport stopped centring the caret under word wrap, because it counted wrapped rows with a proportional
  *     font's pixel measurements rather than the cell grid the terminal actually wraps on.
  *
  * The configuration below is the one those bugs need: floating info bar, word wrap on, visual-line navigation on, and
  * a prose document with paragraph breaks that is longer than the viewport.
  */
class TuiWrappedNavigationSpec extends TuiSpec:

  private val paragraph = "the quick brown fox jumps over the lazy dog and keeps running onward " * 6

  private val prose = (0 until 24).map(index => s"Paragraph $index. $paragraph").mkString("\n\n")

  private val environment =
    TuiEnvironment
      .withFile(prose)
      .withConfig(
        _.withCursorInfoBarSegments(List(CursorInfoBarSegment.Position, CursorInfoBarSegment.WordCount))
          .withCursorInfoBarPlacement(CursorInfoBarPlacement.Floating)
          .withWordWrap(true)
          .withVisualLineCursorNavigation(true)
      )

  /** The floating info bar is the only thing on screen carrying a word count, which is what distinguishes its rows from
    * the status bar's own "Line n, Col n" readout.
    */
  private def infoBarRows(screen: TuiScreen): Vector[Int] = screen.rowsContaining("words")

  /** The columns the bar's own text occupies on `row` -- safely inside its panel, and clear of the gutter, which
    * happens to share the panel's background colour and would otherwise read as a trail that was never there.
    */
  private def barTextColumns(screen: TuiScreen, row: Int): Vector[Int] =
    val line  = screen.rowText(row)
    val start = line.indexOf("Line")
    val end   = line.indexOf("words") + "words".length
    if start < 0 || end <= start then Vector.empty else (start until end).toVector

  private def panelBackground(screen: TuiScreen, row: Int): java.awt.Color =
    screen.backgroundAt(barTextColumns(screen, row).headOption.getOrElse(0), row)

  /** Whether any cell on `row` is still painted in the background `panel` -- the stale trail #1266 removed. */
  private def stillPainted(screen: TuiScreen, row: Int, columns: Vector[Int], panel: java.awt.Color): Vector[Int] =
    columns.filter(col => screen.backgroundAt(col, row) == panel)

  private def cursorOf(state: com.serenity.state.models.AppState): Option[CursorPosition] =
    focusedBuffer(state).flatMap(_.editing.cursors.headOption)

  // -- The info bar's trail (#1266, first defect) --------------------------------------------------------------------

  "the floating cursor info bar" should "be drawn below the caret, over the document" in runTui(environment) {
    for _ <- verify("info bar present") { screen =>
          val rows = infoBarRows(screen)
          rows should have size 1
          screen.rowText(rows.head) should include("Line 1, Col 1")
          rows.head should be > 1
          rows.head should be < screen.height - 1
        }
    yield ()
  }

  it should "clear the row it vacates on the very next incremental frame, leaving no trail" in runTui(environment) {
    for
      before <- settledScreen
      vacatedRow     = infoBarRows(before).headOption.getOrElse(fail("expected the info bar on screen"))
      vacatedColumns = barTextColumns(before, vacatedRow)
      panel          = panelBackground(before, vacatedRow)
      _     <- pressAll(List.fill(6)(TuiKeys.ArrowDown)*)
      after <- screen // one incremental frame: no settling, no full repaint
      _ <- verify("no trail") { current =>
        val nowOccupied = infoBarRows(current)
        nowOccupied should not be empty
        nowOccupied should not contain vacatedRow
        stillPainted(current, vacatedRow, vacatedColumns, panel) shouldBe empty
      }
    yield vacatedColumns should not be empty
  }

  it should "leave no trail when the bar moves across a blank paragraph-break row" in runTui(environment) {
    // Paragraph 0 wraps over several rows and is followed by a blank line; walking the caret down through that gap is
    // where the stale background was most visible, since there is no text to repaint over it.
    for
      _      <- pressAll(List.fill(2)(TuiKeys.ArrowDown)*)
      before <- settledScreen
      vacatedRow     = infoBarRows(before).headOption.getOrElse(fail("expected the info bar on screen"))
      vacatedColumns = barTextColumns(before, vacatedRow)
      panel          = panelBackground(before, vacatedRow)
      _     <- pressAll(List.fill(5)(TuiKeys.ArrowDown)*)
      after <- screen
      _ <- verify("gap left clean") { current =>
        infoBarRows(current) should not contain vacatedRow
        stillPainted(current, vacatedRow, vacatedColumns, panel) shouldBe empty
      }
    yield ()
  }

  it should "follow the caret back up again without leaving a trail behind it" in runTui(environment) {
    for
      _      <- pressAll(List.fill(8)(TuiKeys.ArrowDown)*)
      before <- settledScreen
      vacatedRow     = infoBarRows(before).headOption.getOrElse(fail("expected the info bar on screen"))
      vacatedColumns = barTextColumns(before, vacatedRow)
      panel          = panelBackground(before, vacatedRow)
      _     <- pressAll(List.fill(5)(TuiKeys.ArrowUp)*)
      after <- screen
      _ <- verify("no trail upwards") { current =>
        infoBarRows(current) should not contain vacatedRow
        stillPainted(current, vacatedRow, vacatedColumns, panel) shouldBe empty
      }
    yield ()
  }

  // -- Vertical navigation through wrapped rows (#1266, second defect) -----------------------------------------------

  "Down in wrapped prose" should "advance the caret exactly one visual row each press" in runTui(environment) {
    for
      _     <- settledScreen
      start <- screen
      rows  <- (0 until 12).toList.traverse(_ => arrowDown >> screen.map(_.caret._2))
    yield
      // One row per press, no stalls and no skips, from the document's first row downwards.
      rows shouldBe (start.caret._2 + 1 to start.caret._2 + 12).toList
  }

  it should "still advance exactly one row per press from a column at the right margin" in runTui(environment) {
    for
      _ <- settledScreen
      // Park the caret near the wrap boundary, which is the column the renderer and the navigation geometry used to
      // disagree about: one placed the caret on the earlier wrapped row, the other moved from the later one.
      _      <- pressAll(List.fill(180)(TuiKeys.ArrowRight)*)
      atEdge <- settledScreen
      rows   <- (0 until 10).toList.traverse(_ => arrowDown >> screen.map(_.caret._2))
    yield
      atEdge.caret._1 should be > 150
      rows shouldBe (atEdge.caret._2 + 1 to atEdge.caret._2 + 10).toList
  }

  it should "return to exactly where it started after the same number of Ups" in runTui(environment) {
    for
      _      <- pressAll(List.fill(120)(TuiKeys.ArrowRight)*)
      before <- state
      _      <- pressAll(List.fill(20)(TuiKeys.ArrowDown)*)
      middle <- state
      _      <- pressAll(List.fill(20)(TuiKeys.ArrowUp)*)
      after  <- state
    yield
      // Never stuck: twenty presses actually moved somewhere.
      cursorOf(middle) should not be cursorOf(before)
      // Never skipping: the walk back up lands on exactly the row it started from.
      cursorOf(after) shouldBe cursorOf(before)
  }

  it should "keep advancing one visual row at a time past the first screenful" in runTui(environment) {
    for
      _     <- pressAll(List.fill(60)(TuiKeys.ArrowDown)*)
      _     <- settledScreen
      start <- state
      steps <- (0 until 10).toList.traverse(_ => arrowDown >> state.map(cursorOf))
    yield
      // Beyond the first screenful the navigation geometry is rebuilt around the cursor rather than the viewport top;
      // before #1266 that window did not contain the cursor's own line, and movement silently fell back to a
      // character grid that mis-wraps prose -- visible here as repeated or skipped positions.
      steps.distinct should have size steps.size
      steps.headOption.flatten should not be cursorOf(start)
      steps.flatten.map(_.line).sliding(2).foreach {
        case Seq(first, second) => second should be >= first
        case _                  => ()
      }
  }

  // -- Viewport centring under word wrap (#1266, third defect) -------------------------------------------------------

  "the viewport" should "keep the caret vertically centred once the document scrolls beneath it" in
    runTui(environment) {
      for
        _       <- pressAll(List.fill(40)(TuiKeys.ArrowDown)*)
        settled <- settledScreen
        top     <- state.map(current => focusedBuffer(current).map(_.viewport.topLine))
      yield
        val centre = (settled.height - 1) / 2
        // Centred, not merely on screen: within a couple of rows of the middle of the editor body.
        settled.caret._2 should be >= centre - 3
        settled.caret._2 should be <= centre + 3
        top.getOrElse(0) should be > 0
    }

  it should "hold that centre as the caret keeps descending" in runTui(environment) {
    for
      _    <- pressAll(List.fill(40)(TuiKeys.ArrowDown)*)
      _    <- settledScreen
      rows <- (0 until 8).toList.traverse(_ => arrowDown >> settledScreen.map(_.caret._2))
      tops <- state.map(current => focusedBuffer(current).map(_.viewport.topLine))
    yield
      // The caret stays put and the document scrolls underneath it, rather than the caret walking to the bottom edge.
      rows.distinct should have size 1
      tops.getOrElse(0) should be > 0
  }

  it should "clamp to the top of the document rather than centring on an empty half-screen" in
    runTui(environment) {
      verify("document start") { screen =>
        // At the very start there is nothing above the first line to centre against, so the caret sits at the top.
        screen.caret._2 shouldBe 1
        screen.rowText(1) should include("Paragraph 0")
      }
    }

  it should "clamp to the bottom of the document at the end" in runTui(environment) {
    for
      _       <- pressAll(List.fill(140)(TuiKeys.ArrowDown)*)
      settled <- settledScreen
      _ <- verify("document end") { screen =>
        // The caret has reached the document's last line (47 of 47) and is still drawn on the text.
        screen.statusBar should include("Line 47")
        // Nothing left below to scroll into view, so the caret can no longer sit at the centre: it sits below it,
        // near the bottom of the body, rather than the viewport scrolling past the end of the document.
        screen.caret._2 should be > (screen.height - 1) / 2
        screen.caret._2 should be < screen.height - 1
        screen.rowText(screen.caret._2) should not be empty
      }
    yield settled.caret._2 should be > 0
  }

  // -- Page navigation over wrapped rows -----------------------------------------------------------------------------

  /** A screenful is a screenful of what the terminal shows: visual rows. Counted in logical lines, one PageDown in this
    * document (47 logical lines wrapping into some 150 rows, in a 54-row viewport) jumps past the end of it.
    */
  private def topLineOf(current: com.serenity.state.models.AppState): Int =
    focusedBuffer(current).map(_.viewport.topLine).getOrElse(-1)

  private def caretIsOnTheText(screen: TuiScreen): org.scalatest.Assertion =
    screen.caret._1 should be < screen.width
    screen.caret._2 should be > 0
    screen.caret._2 should be < screen.height - 1

  "PageDown" should "move the cursor a screenful of visual rows, not of logical lines" in runTui(environment) {
    for
      _       <- settledScreen
      _       <- press(TuiKeys.PageDown)
      settled <- settledScreen
      current <- state
    yield
      // Somewhere into the document, nowhere near its last line: a screenful of rows is a few paragraphs, not all of
      // them.
      cursorOf(current).map(_.line).getOrElse(-1) should be > 0
      cursorOf(current).map(_.line).getOrElse(-1) should be < 40
      caretIsOnTheText(settled)
  }

  it should "bring the viewport with it, leaving the cursor on screen" in runTui(environment) {
    for
      _       <- settledScreen
      _       <- press(TuiKeys.PageDown)
      settled <- settledScreen
      current <- state
    yield
      topLineOf(current) should be > 0
      settled.containsText("Paragraph 0") shouldBe false
      caretIsOnTheText(settled)
  }

  it should "keep going on the next press rather than snapping the viewport back to the top" in runTui(environment) {
    for
      _       <- settledScreen
      _       <- press(TuiKeys.PageDown)
      first   <- state
      _       <- press(TuiKeys.PageDown)
      settled <- settledScreen
      second  <- state
    yield
      cursorOf(second).map(_.line).getOrElse(-1) should be > cursorOf(first).map(_.line).getOrElse(-1)
      topLineOf(second) should be > topLineOf(first)
      caretIsOnTheText(settled)
  }

  it should "clamp at the document's last row rather than running off the end" in runTui(environment) {
    for
      _       <- settledScreen
      _       <- pressAll(List.fill(8)(TuiKeys.PageDown)*)
      settled <- settledScreen
      current <- state
    yield
      cursorOf(current).map(_.line) shouldBe Some(46)
      settled.containsText("Paragraph 23") shouldBe true
      caretIsOnTheText(settled)
  }

  "PageUp" should "undo PageDown, row for row" in runTui(environment) {
    for
      _      <- settledScreen
      before <- state
      _      <- press(TuiKeys.PageDown)
      _      <- press(TuiKeys.PageUp)
      after  <- state
    yield
      cursorOf(after).map(_.line) shouldBe cursorOf(before).map(_.line)
      topLineOf(after) shouldBe topLineOf(before)
  }

  it should "clamp at the first row when there is less than a screenful above" in runTui(environment) {
    for
      _       <- settledScreen
      _       <- pressAll(TuiKeys.PageDown, TuiKeys.PageUp, TuiKeys.PageUp)
      settled <- settledScreen
      current <- state
    yield
      cursorOf(current).map(_.line) shouldBe Some(0)
      topLineOf(current) shouldBe 0
      settled.containsText("Paragraph 0") shouldBe true
      caretIsOnTheText(settled)
  }
end TuiWrappedNavigationSpec
