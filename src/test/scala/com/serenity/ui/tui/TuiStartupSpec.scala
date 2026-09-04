package com.serenity.ui.tui
import com.serenity.state.models.Focus

/** What a TUI session puts on the terminal before the user has done anything: the file it was told to open, or the
  * start page when it was told to open nothing -- chrome, gutter, content and caret, asserted as cells rather than as
  * substrings of a byte stream.
  */
class TuiStartupSpec extends TuiSpec:

  "opening a file" should "draw its lines into the editor body, one document line per row" in
    runTui(TuiEnvironment.withFile("alpha\nbeta\ngamma")) {
      verify("document body") { screen =>
        screen.rowText(1).stripTrailing shouldBe " 1 alpha"
        screen.rowText(2).stripTrailing shouldBe " 2 beta"
        screen.rowText(3).stripTrailing shouldBe " 3 gamma"
        screen.rowText(4).strip shouldBe ""
      }
    }

  it should "name the file in the buffer header and the status bar" in
    runTui(TuiEnvironment.withFile("content", name = "notes.md")) {
      verify("chrome") { screen =>
        screen.titleBar should include("notes.md")
        screen.statusBar should include("notes.md")
        screen.statusBar should include("Line 1, Col 1")
        screen.statusBar should include("Markdown")
      }
    }

  it should "place the caret on the first cell of the first line, past the gutter" in
    runTui(TuiEnvironment.withFile("alpha")) {
      for
        current <- screen
        _ <- verify("caret") { screen =>
          screen.caretVisible shouldBe true
          screen.caret shouldBe (3, 1)
          screen.textAt(0, 1, 4) shouldBe " 1 a"
        }
      yield current.cellAt(3, 1).text shouldBe "a"
    }

  it should "size the gutter to the widest line number in the document" in
    runTui(TuiEnvironment.withLines(12)) {
      verify("two-digit gutter") { screen =>
        screen.rowText(1).stripTrailing shouldBe " 1 line 0"
        screen.rowText(10).stripTrailing shouldBe "10 line 9"
        screen.caret shouldBe (3, 1)
      }
    }

  it should "widen that gutter again once the document reaches three digits" in
    runTui(TuiEnvironment.withLines(120)) {
      verify("three-digit gutter") { screen =>
        screen.rowText(1).stripTrailing shouldBe "  1 line 0"
        screen.rowText(11).stripTrailing shouldBe " 11 line 10"
        screen.caret shouldBe (4, 1)
      }
    }

  it should "show an empty document as a placeholder rather than a blank screen" in
    runTui(TuiEnvironment.withFile("")) {
      verify("empty document") { screen =>
        screen.containsText("Empty document") shouldBe true
        screen.statusBar should include("Line 1, Col 1")
      }
    }

  "the first frame" should "enter the alternate screen, then clear and repaint it inside synchronized-update brackets" in
    runTui(TuiEnvironment.withFile("first frame")) {
      val esc = 0x1b.toChar
      for first <- screen
      yield
        // The shell's own startup comes first: alternate screen, hidden hardware cursor, focus reporting, the
        // keyboard-protocol negotiation, then the input modes the handler enables.
        first.emitted should startWith(s"$esc[?1049h")
        first.emitted should include(s"$esc[?25l")
        // Only then the frame itself: no previous frame to diff against, so a full clear-and-repaint.
        first.emitted should include(s"$esc[?2026h$esc[2J$esc[H")
        first.emitted should endWith(s"$esc[?2026l")
        first.emitted.indexOf(s"$esc[?1049h") should be < first.emitted.indexOf(s"$esc[2J")
    }

  it should "leave the session on the alternate screen with the hardware cursor under the app's control" in
    runTui(TuiEnvironment.withFile("alt screen")) {
      verify("terminal state") { screen =>
        screen.inAlternateScreen shouldBe true
        screen.cursor.shape shouldBe Some(1) // DECSCUSR: blinking block, the app's default caret
      }
    }

  "starting with no file" should "open the start page, centred, with no caret in a document" in runTuiStartPage {
    for
      _ <- verify("start page") { screen =>
        screen.containsText("Welcome to Serenity") shouldBe true
        screen.containsText("[1] New document") shouldBe true
        screen.containsText("[2] Open file or folder") shouldBe true
        screen.caretVisible shouldBe false
      }
      _ <- verifyState("focus")(current => current.persisted.focus should not be a[Focus.EditorPane])
    yield ()
  }

  it should "centre the start page in whatever terminal it is given" in
    runTui(TuiEnvironment.startPage.withViewport(TuiViewport.Small)) {
      verify("small terminal start page") { screen =>
        val welcomeRow = screen.rowOf("Welcome to Serenity").getOrElse(fail("no welcome row"))
        welcomeRow should be < screen.height
        val indent = screen.rowText(welcomeRow).indexOf("Welcome")
        indent should be > 10
      }
    }

  "a session opened in TUI mode" should "carry the TUI flag the settings surface annotates from" in
    runTui() {
      verifyState("tui flag")(current => current.runtime.isTuiMode shouldBe true)
    }
end TuiStartupSpec
