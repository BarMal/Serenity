package com.serenity.ui.tui

import TuiScenarios.*

/** The input a terminal delivers that is not a keystroke: SGR mouse reports, bracketed paste, focus reporting, and the
  * OSC 52 clipboard writes that go back out the same wire. Each is exercised as the bytes a real terminal sends, so the
  * decoder, the handler and the surfaces above them are all in the loop.
  */
class TuiInputChannelsSpec extends TuiSpec:

  private val ContentColumn = 3

  "a mouse click in the document" should "move the caret to that cell" in
    runTui(TuiEnvironment.withFile("alpha\nbeta\ngamma")) {
      for
        _ <- click(ContentColumn + 3, 3)
        _ <- verify("caret moved") { screen =>
          screen.caret shouldBe (ContentColumn + 3, 3)
          screen.statusBar should include("Line 3, Col 4")
        }
      yield ()
    }

  it should "put the caret at the end of a short line when clicked past its text" in
    runTui(TuiEnvironment.withFile("ab\nlonger line")) {
      for
        _ <- click(40, 1)
        _ <- verify("clamped to the line end")(screen => screen.statusBar should include("Line 1, Col 3"))
      yield ()
    }

  "a mouse drag" should "extend a selection from where the press landed" in
    runTui(TuiEnvironment.withFile("selectable text here")) {
      for
        _ <- press(TuiKeys.mousePress(ContentColumn, 1))
        _ <- dragMouse(ContentColumn + 10, 1)
        _ <- verifyState("selection while dragging") { current =>
          val selection = focusedBuffer(current).flatMap(_.editing.selection)
          selection.map(_.start.column) shouldBe Some(0)
          selection.map(_.end.column) shouldBe Some(10)
        }
        _ <- verify("selection is painted") { screen =>
          val backgrounds = (ContentColumn until ContentColumn + 10).map(col => screen.backgroundAt(col, 1)).distinct
          backgrounds should have size 1
          backgrounds.head should not be screen.backgroundAt(ContentColumn + 15, 1)
        }
      yield ()
    }

  it should "keep that selection when the button is released, as the Swing path does" in
    runTui(TuiEnvironment.withFile("selectable text here")) {
      for
        _ <- press(TuiKeys.mousePress(ContentColumn, 1))
        _ <- dragMouse(ContentColumn + 10, 1)
        _ <- press(TuiKeys.mouseRelease(ContentColumn + 10, 1))
        _ <- verifyState("selection after release") { current =>
          val selection = focusedBuffer(current).flatMap(_.editing.selection)
          selection.map(_.start.column) shouldBe Some(0)
          selection.map(_.end.column) shouldBe Some(10)
        }
      yield ()
    }

  it should "not leave the next release swallowed: a plain click after one still moves the caret" in
    runTui(TuiEnvironment.withFile("selectable text here")) {
      for
        _ <- press(TuiKeys.mousePress(ContentColumn, 1))
        _ <- dragMouse(ContentColumn + 10, 1)
        _ <- press(TuiKeys.mouseRelease(ContentColumn + 10, 1))
        _ <- click(ContentColumn + 4, 1)
        _ <- verifyState("caret after the following click") { current =>
          focusedBuffer(current).flatMap(_.editing.selection) shouldBe empty
          focusedBuffer(current).flatMap(_.editing.cursors.headOption).map(_.column) shouldBe Some(4)
        }
      yield ()
    }

  // The wheel is the one mouse report the decoder used to drop on the floor: `ScrollUp`/`ScrollDown` existed and the
  // reducer handled them, but nothing in either shell ever produced one.

  private val scrollable = TuiEnvironment.withFile((0 until 200).map(index => s"line $index").mkString("\n"))

  "a wheel notch" should "scroll the viewport by the configured number of lines" in
    runTui(scrollable.withConfig(_.withWheelScrollLines(4))) {
      for
        _ <- press(TuiKeys.wheelDown(10, 5))
        _ <- verifyState("after one notch down")(current =>
          focusedBuffer(current).map(_.viewport.topLine) shouldBe Some(4)
        )
        _ <- press(TuiKeys.wheelDown(10, 5))
        _ <- verifyState("after two")(current => focusedBuffer(current).map(_.viewport.topLine) shouldBe Some(8))
        _ <- press(TuiKeys.wheelUp(10, 5))
        _ <- verifyState("after one back up")(current =>
          focusedBuffer(current).map(_.viewport.topLine) shouldBe Some(4)
        )
      yield ()
    }

  it should "default to three lines" in runTui(scrollable) {
    for
      _ <- press(TuiKeys.wheelDown(10, 5))
      _ <- verifyState("default notch")(current => focusedBuffer(current).map(_.viewport.topLine) shouldBe Some(3))
    yield ()
  }

  it should "stop at the top of the document rather than scrolling past it" in runTui(scrollable) {
    for
      _ <- press(TuiKeys.wheelUp(10, 5))
      _ <- verifyState("already at the top")(current => focusedBuffer(current).map(_.viewport.topLine) shouldBe Some(0))
    yield ()
  }

  "a bracketed paste" should "insert the text once, not replay it as keystrokes" in runTui() {
    for
      _    <- typeText("start ")
      _    <- paste("pasted text")
      text <- documentText
      _    <- verify("pasted once")(screen => screen.rowText(1).stripTrailing shouldBe " 1 start pasted text")
    yield text shouldBe Some("start pasted text")
  }

  it should "insert a multi-line paste as whole lines, without firing hotkeys on its control characters" in
    runTui() {
      for
        _    <- paste("one\ntwo\nthree")
        text <- documentText
        _ <- verify("three lines") { screen =>
          screen.rowText(1).stripTrailing shouldBe " 1 one"
          screen.rowText(2).stripTrailing shouldBe " 2 two"
          screen.rowText(3).stripTrailing shouldBe " 3 three"
        }
      yield text shouldBe Some("one\ntwo\nthree")
    }

  "copying a selection" should "reach the terminal as an OSC 52 clipboard write" in
    runTui(TuiEnvironment.withFile("copy this")) {
      for
        _         <- selectAll
        _         <- copy
        current   <- screen
        clipboard <- clipboardText
      yield
        // With no display reachable, ClipboardStrategy resolves to OSC 52 through the terminal's own writer -- which
        // is what makes a copy work over SSH, and what a spec can observe on the wire.
        current.terminal.osc52Payloads should contain("copy this")
        clipboard shouldBe Some("copy this")
    }

  "cutting a selection" should "remove it from the document and put it on the clipboard" in
    runTui(TuiEnvironment.withFile("cut this")) {
      for
        _         <- selectAll
        _         <- cut
        text      <- documentText
        clipboard <- clipboardText
        _         <- verify("emptied")(screen => screen.rowText(1).strip shouldBe "1")
      yield
        text shouldBe Some("")
        clipboard shouldBe Some("cut this")
    }

  "pasting from the clipboard" should "insert what a copy put there" in runTui(TuiEnvironment.withFile("round trip")) {
    for
      _    <- selectAll
      _    <- copy
      _    <- lineEnd
      _    <- typeText(" ")
      _    <- pasteClipboard
      text <- documentText
    yield text shouldBe Some("round trip round trip")
  }

  "terminal focus reporting" should "be absorbed silently, leaving the document and caret untouched" in
    runTui(TuiEnvironment.withFile("focus test")) {
      for
        before <- settledScreen
        _      <- focusOut
        _      <- focusIn
        after  <- settledScreen
        text   <- documentText
      yield
        // CSI O / CSI I must never reach the document as keystrokes (issue #1171).
        text shouldBe Some("focus test")
        after.changedCells(before) shouldBe empty
    }

  it should "leave editing working after a focus round trip" in runTui(TuiEnvironment.withFile("")) {
    for
      _    <- focusOut
      _    <- focusIn
      _    <- typeText("still typing")
      text <- documentText
      _    <- verify("typed after refocus")(screen => screen.rowText(1).stripTrailing shouldBe " 1 still typing")
    yield text shouldBe Some("still typing")
  }

  "the input modes a TUI session enables" should "be active for the whole session" in runTui() {
    for
      _ <- typeText("anything")
      _ <- verify("modes still on") { screen =>
        screen.mouseTrackingEnabled shouldBe true
        screen.bracketedPasteEnabled shouldBe true
        screen.focusReportingEnabled shouldBe true
      }
    yield ()
  }
end TuiInputChannelsSpec
