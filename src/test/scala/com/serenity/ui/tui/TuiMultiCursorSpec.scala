package com.serenity.ui.tui

import cats.syntax.all.*

import TuiScenarios.*

/** Creating cursors from the keyboard and typing into all of them at once (#1994, #1951): the occurrence commands are
  * driven by their default keys and by the palette, and every assertion is made on the text the cursors then produced.
  */
class TuiMultiCursorSpec extends TuiSpec:

  private val skipOccurrence = press(TuiKey("Ctrl+Alt+K", Array(0x1b.toByte, 0x0b.toByte)))

  private val cursorCount =
    state.map(current => focusedBuffer(current).map(_.editing.cursors.size))

  "Ctrl+D" should "select the word under the caret, then each further occurrence, and typing replaces them all" in
    runTui() {
      for
        _     <- typeText("cat dog cat bird cat")
        _     <- lineStart
        _     <- ctrl('d') >> ctrl('d')
        count <- cursorCount
        _     <- typeText("fox")
        text  <- documentText
      yield
        count shouldBe Some(2)
        text shouldBe Some("fox dog fox bird cat")
    }

  it should "match whole words only when the selection started from a collapsed caret" in runTui() {
    for
      _     <- typeText("cat concatenate cat")
      _     <- lineStart
      _     <- ctrl('d') >> ctrl('d') >> ctrl('d')
      count <- cursorCount
      _     <- typeText("fox")
      text  <- documentText
    yield
      count shouldBe Some(2)
      text shouldBe Some("fox concatenate fox")
  }

  it should "match inside longer words when the selection was made by the user" in runTui() {
    for
      _     <- typeText("cat concatenate")
      _     <- lineStart
      _     <- press(modified(ArrowRight, shift = true)) >> press(modified(ArrowRight, shift = true))
      _     <- press(modified(ArrowRight, shift = true))
      _     <- ctrl('d')
      count <- cursorCount
      _     <- typeText("X")
      text  <- documentText
    yield
      count shouldBe Some(2)
      text shouldBe Some("X conXenate")
  }

  "Select All Occurrences (Ctrl+Shift+L)" should "put a cursor on every whole-word match across lines" in runTui() {
    for
      _     <- typeDocument("cat dog", "a concatenate cat", "cat")
      _     <- arrowUp >> arrowUp >> lineStart
      _     <- ctrlShift('l')
      count <- cursorCount
      _     <- typeText("fox")
      text  <- documentText
    yield
      count shouldBe Some(3)
      text shouldBe Some("fox dog\na concatenate fox\nfox")
  }

  "Skip occurrence" should "leave the newest match unselected and move on to the one after it" in runTui() {
    for
      _     <- typeText("cat cat cat")
      _     <- lineStart
      _     <- ctrl('d') >> ctrl('d')
      _     <- skipOccurrence
      count <- cursorCount
      _     <- typeText("X")
      text  <- documentText
    yield
      count shouldBe Some(2)
      text shouldBe Some("X cat X")
  }

  "The palette" should "select the next occurrence" in runTui() {
    for
      _     <- typeText("cat cat")
      _     <- lineStart
      _     <- runCommand("select next occurrence")
      _     <- runCommand("select next occurrence")
      count <- cursorCount
      _     <- typeText("X")
      text  <- documentText
    yield
      count shouldBe Some(2)
      text shouldBe Some("X X")
  }

  "Ctrl+Alt+V" should "split the pane where Ctrl+D used to" in runTui() {
    for
      before <- state.map(_.persisted.layout.editorPanes.size)
      _      <- press(TuiKey("Ctrl+Alt+V", Array(0x1b.toByte, 0x16.toByte)))
      after  <- state.map(_.persisted.layout.editorPanes.size)
    yield (after - before) shouldBe 1
  }
