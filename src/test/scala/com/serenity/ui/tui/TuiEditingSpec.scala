package com.serenity.ui.tui

import cats.syntax.all.*

import TuiScenarios.*

/** Editing a document through the terminal: what reaches the buffer, what reaches the cells, and where the terminal's
  * own caret ends up after each step. Every assertion is made against the rendered grid as well as state, because the
  * two agreeing is the actual contract -- a cursor the state says is at column 6 but the terminal draws at column 3 is
  * exactly the class of bug #1215 was.
  */
class TuiEditingSpec extends TuiSpec:

  /** The first content column: the gutter is `" 1 "` for a single-digit document. */
  private val ContentColumn = 3
  private val FirstLineRow  = 1

  "typing" should "insert characters at the caret and advance it one cell each" in runTui() {
    for
      _ <- typeText("hello")
      _ <- verify("typed line") { screen =>
        screen.rowText(FirstLineRow).stripTrailing shouldBe " 1 hello"
        screen.caret shouldBe (ContentColumn + 5, FirstLineRow)
        screen.statusBar should include("Line 1, Col 6")
      }
      text <- documentText
    yield text shouldBe Some("hello")
  }

  it should "start a new document line on Enter, with its own gutter number" in runTui() {
    for
      _ <- typeDocument("first", "second", "third")
      _ <- verify("three lines") { screen =>
        screen.rowText(1).stripTrailing shouldBe " 1 first"
        screen.rowText(2).stripTrailing shouldBe " 2 second"
        screen.rowText(3).stripTrailing shouldBe " 3 third"
        screen.caret shouldBe (ContentColumn + 5, 3)
        screen.statusBar should include("Line 3, Col 6")
      }
      text <- documentText
    yield text shouldBe Some("first\nsecond\nthird")
  }

  it should "delete backwards over the caret on Backspace, and forwards on Delete" in runTui() {
    for
      _ <- typeText("abcdef")
      _ <- backspace
      _ <- backspace
      _ <- verify("after backspace")(screen => screen.rowText(1).stripTrailing shouldBe " 1 abcd")
      _ <- arrowLeft
      _ <- arrowLeft
      _ <- delete
      _ <- verify("after delete")(screen => screen.rowText(1).stripTrailing shouldBe " 1 abd")
      text <- documentText
    yield text shouldBe Some("abd")
  }

  it should "join lines when Backspace is pressed at the start of one" in runTui() {
    for
      _    <- typeDocument("above", "below")
      _    <- lineStart
      _    <- backspace
      text <- documentText
      _ <- verify("joined") { screen =>
        screen.rowText(1).stripTrailing shouldBe " 1 abovebelow"
        screen.rowText(2).strip shouldBe ""
      }
    yield text shouldBe Some("abovebelow")
  }

  "the caret" should "follow arrow keys across and between lines, and the status bar with it" in runTui() {
    for
      _ <- typeDocument("alpha", "beta")
      _ <- arrowUp
      _ <- verify("moved up") { screen =>
        screen.caret shouldBe (ContentColumn + 4, 1)
        screen.statusBar should include("Line 1, Col 5")
      }
      _ <- arrowLeft >> arrowLeft
      _ <- verify("moved left")(screen => screen.caret shouldBe (ContentColumn + 2, 1))
      _ <- arrowDown
      _ <- verify("moved down")(screen => screen.caret shouldBe (ContentColumn + 2, 2))
    yield ()
  }

  it should "jump to the start and end of the line on Home and End" in runTui() {
    for
      _ <- typeText("a longer line of text")
      _ <- lineStart
      _ <- verify("at start") { screen =>
        screen.caret shouldBe (ContentColumn, FirstLineRow)
        screen.statusBar should include("Col 1")
      }
      _ <- lineEnd
      _ <- verify("at end")(screen => screen.caret shouldBe (ContentColumn + 21, FirstLineRow))
    yield ()
  }

  "undo and redo" should "step the document back and forward, repainting each time" in runTui() {
    for
      _       <- typeText("first edit")
      _       <- undo
      undone  <- documentText
      _       <- verify("undone")(screen => screen.rowText(1) should not include "first edit")
      _       <- redo
      redone  <- documentText
      _       <- verify("redone")(screen => screen.rowText(1).stripTrailing shouldBe " 1 first edit")
    yield
      undone shouldBe Some("")
      redone shouldBe Some("first edit")
  }

  "select all" should "cover the whole document and paint the selection with its own background" in runTui() {
    for
      _       <- typeText("selected text")
      before  <- screen
      _       <- selectAll
      after   <- screen
      _ <- verifyState("selection in state") { current =>
        focusedBuffer(current).flatMap(_.editing.selection).map(_.end.column) shouldBe Some("selected text".length)
      }
    yield
      // The selected run must actually look different, not merely be recorded in state.
      val selectionCells = (ContentColumn until ContentColumn + "selected text".length).map(col => (col, FirstLineRow))
      selectionCells.foreach(cell => after.backgroundAt(cell._1, cell._2) should not be before.backgroundAt(cell._1, cell._2))
  }

  "a long single line" should "wrap across rows, marking each continuation row in the gutter" in
    runTui(TuiEnvironment.withFile("x" * 5000)) {
      verify("wrapped line") { screen =>
        screen.rowText(FirstLineRow).take(ContentColumn) shouldBe " 1 "
        // Continuation rows carry a vertical rule instead of a line number, so a wrapped line still reads as one line.
        screen.rowText(FirstLineRow + 1).take(ContentColumn) shouldBe " \u2502 "
        screen.rowText(FirstLineRow + 2).take(ContentColumn) shouldBe " \u2502 "
        // Every character of the document reaches a cell: 5000 of them, across ceil(5000 / (200 - 3)) rows.
        screen.rows.map(_.count(_ == 'x')).sum shouldBe 5000
        screen.rowsContaining("x") should have size 26
      }
    }

  it should "reflow that same line when the terminal gets narrower" in
    runTui(TuiEnvironment.withFile("y" * 300)) {
      for
        _ <- verify("wide terminal")(screen => screen.rowsContaining("y") should have size 2)
        _ <- resize(TuiViewport.Small)
        _ <- verify("narrow terminal") { screen =>
          screen.rows.map(_.count(_ == 'y')).sum shouldBe 300
          screen.rowsContaining("y").size should be > 2
        }
      yield ()
    }

  "a large document" should "render the window that fits and keep the rest addressable" in
    runTui(TuiEnvironment.withLines(10000)) {
      for
        _ <- verify("first window") { screen =>
          screen.rowText(1).stripTrailing shouldBe "    1 line 0"
          screen.containsText("line 9999") shouldBe false
        }
        _ <- pressAll(List.fill(5)(TuiKeys.PageDown)*)
        _ <- verify("scrolled") { screen =>
          screen.containsText("line 0") shouldBe false
          screen.rowText(1).strip should startWith regex "\\d+ line \\d+"
        }
        text <- documentText
      yield text.map(_.linesIterator.size) shouldBe Some(10000)
    }

  "typing into a document with no trailing newline" should "leave the file's own structure intact" in
    runTui(TuiEnvironment.withFile("one\ntwo")) {
      for
        _    <- lineEnd
        _    <- typeText("!")
        text <- documentText
        _    <- verify("edited")(screen => screen.rowText(1).stripTrailing shouldBe " 1 one!")
      yield text shouldBe Some("one!\ntwo")
    }
end TuiEditingSpec
