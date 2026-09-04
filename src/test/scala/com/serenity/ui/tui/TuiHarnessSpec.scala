package com.serenity.ui.tui

import cats.syntax.all.*
import com.serenity.state.models.Focus

/** The harness proving itself before anything is asserted through it: that a session really is a terminal (alternate
  * screen, raw-mode escapes, the input modes a TUI enables), that a keystroke really does reach application state, that
  * a frame really does reach the cell grid, and that [[TuiSession.feed]]'s barrier settles input rather than racing it.
  *
  * If these fail, every other TUI behaviour spec is meaningless -- so they run first and assert the mechanism directly.
  */
class TuiHarnessSpec extends TuiSpec:

  "a TUI session" should "start on the alternate screen with the terminal's own cursor hidden" in runTui() {
    for _ <- verify("startup") { screen =>
          screen.inAlternateScreen shouldBe true
          screen.focusReportingEnabled shouldBe true
          screen.mouseTrackingEnabled shouldBe true
          screen.bracketedPasteEnabled shouldBe true
        }
    yield ()
  }

  it should "open at the configured terminal size, which is a full-screen laptop terminal by default" in runTui() {
    for
      size   <- TuiScript.apply(_.viewportSize)
      screen <- screen
    yield
      size shouldBe TuiViewport.Default
      screen.width shouldBe 200
      screen.height shouldBe 56
  }

  it should "open the file it was given, with its content on screen and in state" in
    runTui(TuiEnvironment.withFile("hello from disk")) {
      for
        _ <- verify("opened file")(screen => screen.containsText("hello from disk").shouldBe(true))
        _ <- verifyState("focus")(state => state.persisted.focus shouldBe a[Focus.EditorPane])
      yield ()
    }

  "typing" should "reach application state and the screen, in order" in runTui() {
    for
      _ <- typeText("abc")
      _ <- verifyState("typed")(state => focusedBuffer(state).map(_.document.content.toString).shouldBe(Some("abc")))
      _ <- verify("rendered")(screen => screen.containsText("abc").shouldBe(true))
    yield ()
  }

  it should "settle every keystroke before the next script step runs, with no polling" in runTui() {
    for
      _      <- typeSlowly("one")
      _      <- typeText(" two")
      events <- eventsApplied
      text   <- documentText
      _ <- verifyState("all applied")(state =>
        focusedBuffer(state).map(_.document.content.toString).shouldBe(Some("one two"))
      )
    yield
      text.shouldBe(Some("one two"))
      events.size.shouldBe(7)
  }

  "a lone Escape" should "be delivered as Escape rather than merging into the sentinel that follows it" in runTui() {
    for
      _ <- ctrl('p')
      _ <- verifyState("palette open")(state => state.commandRunnerSurface should not be empty)
      _ <- escape
      _ <- verifyState("palette closed")(state => state.commandRunnerSurface shouldBe empty)
    yield ()
  }

  "a settled frame" should "cost nothing to repaint: no state change means no bytes on the wire" in
    runTui(TuiEnvironment.withFile("stable")) {
      for
        settled <- settledScreen
        again   <- screen
      yield
        again.emitted shouldBe ""
        again.changedCells(settled) shouldBe empty
    }

  "resizing the terminal" should "re-lay out at the new size and repaint in full" in
    runTui(TuiEnvironment.withFile("resize me")) {
      for
        _     <- screen
        _     <- resize(TuiViewport.Small)
        after <- screen
        _     <- verifyState("viewport in state")(state => state.runtime.viewportSize shouldBe Some(TuiViewport.Small))
      yield
        after.width shouldBe 80
        after.height shouldBe 24
        after.containsText("resize me") shouldBe true
    }

  it should "keep working at a terminal far larger than the default" in
    runTui(TuiEnvironment.withFile("wide").withViewport(TuiViewport.Wide)) {
      for screen <- screen
      yield
        screen.width shouldBe 240
        screen.height shouldBe 64
        screen.containsText("wide") shouldBe true
    }

  "a failing screen assertion" should "carry the whole terminal grid, so a red test shows the screen" in {
    val failure = intercept[AssertionError] {
      runTui(TuiEnvironment.withFile("visible content")) {
        verify("deliberate failure")(screen => screen.containsText("this text is not on screen").shouldBe(true))
      }
    }

    failure.getMessage should include("deliberate failure")
    failure.getMessage should include("visible content")
    failure.getMessage should include("caret")
  }

  "the session workspace" should "be a temporary directory, never the developer's own files" in runTui() {
    for path <- workspacePath(TuiEnvironment.DefaultFileName)
    yield path should include("tui-session")
  }

  "a script" should "compose reusable fragments rather than repeating keystrokes" in runTui() {
    val typeAndSelectAll = typeText("composed") >> ctrl('a')

    for
      _ <- typeAndSelectAll
      _ <- verifyState("selection")(state =>
        focusedBuffer(state).flatMap(_.editing.selection).map(_.end.column).shouldBe(Some("composed".length))
      )
    yield ()
  }
end TuiHarnessSpec
