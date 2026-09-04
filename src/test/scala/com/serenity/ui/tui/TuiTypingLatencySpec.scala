package com.serenity.ui.tui

import com.serenity.animation.WindowSitterConfig

/** Typed text has to be on screen in the frame that follows the keystroke.
  *
  * It is not, today, and this spec says so by asserting what is wanted and letting `pendingUntilFixed` report the gap:
  * the pending tests below start passing the moment the defect is fixed, at which point `pendingUntilFixed` itself
  * fails and asks for the marker to be removed. Nothing here encodes the broken behaviour as expected.
  *
  * The defect, for whoever picks it up:
  *
  *   - `AppRuntime.inputEventPhase` marks the window sitter active on every `InsertChar` (`observeWindowSitterTyping`);
  *   - the fast render phase then paints the cursor-only path for every frame where the sitter is active and
  *     `needsFullContentRender` is false -- which moves the caret but paints no content;
  *   - `needsFullContentRender` is always false in TUI mode: it looks for character-reveal buffer animations, a theme
  *     transition, or surface animations, and the first never reaches the surface-generic render entry points a
  *     terminal uses at all (`docs/tui-mode.md`, "Known degradations").
  *
  * So a terminal session paints no typed text until the sitter decays, then repaints the insertion and its reflow
  * together -- which is exactly the reported symptom: the caret moves as you type, the characters arrive late and all
  * at once, and the surrounding text jumps. The cursor-only branch exists for a window-sitter glyph that lives in the
  * Swing window chrome and never touches the canvas; a terminal has no chrome to draw it in, so in TUI mode the branch
  * costs every keystroke its visibility and buys nothing.
  *
  * The control tests at the bottom run the same scenarios with the window sitter disabled. They pass today, which is
  * what identifies the sitter as the cause rather than the renderer.
  */
class TuiTypingLatencySpec extends TuiSpec:

  private val paragraph = "the quick brown fox jumps over the lazy dog and keeps running onward " * 6
  private val prose     = (0 until 8).map(index => s"Paragraph $index. $paragraph").mkString("\n\n")

  private val wrappedProse =
    TuiEnvironment.withFile(prose).withConfig(_.withWordWrap(true).withVisualLineCursorNavigation(true))

  /** The same session with the window sitter switched off -- the control case for every pending assertion above it. */
  private val withoutWindowSitter =
    wrappedProse.withConfig(_.withWindowSitterConfig(WindowSitterConfig.default.copy(enabled = false)))

  // -- What is wanted (pending until the defect is fixed) -------------------------------------------------------------

  "typing a character" should "put it on screen in the very next frame the runtime paints" in pendingUntilFixed {
    runTui(wrappedProse) {
      for
        _      <- settledScreen
        before <- runtimeScreen
        _      <- typeText("Z")
        after  <- runtimeScreen
        text   <- documentText
      yield
        text.exists(_.contains("Z")) shouldBe true
        after.caret._1 shouldBe before.caret._1 + 1
        after.rowText(1).contains("Z") shouldBe true
    }
  }

  it should "never leave the runtime painting the caret without its content" in pendingUntilFixed {
    runTui(wrappedProse) {
      for
        _          <- settledScreen
        _          <- typeText("Z")
        cursorOnly <- paintsCursorOnly
      yield
        // The window sitter has no visual representation in a terminal, so it must not suppress content painting there.
        cursorOnly shouldBe false
    }
  }

  "a burst of typing" should "appear as it is typed rather than arriving in one later frame" in pendingUntilFixed {
    runTui(wrappedProse) {
      for
        _      <- settledScreen
        _      <- typeSlowly("HELLO")
        during <- runtimeScreen
        text   <- documentText
      yield
        text.exists(_.contains("HELLO")) shouldBe true
        during.rowText(1).contains("HELLO") shouldBe true
    }
  }

  "an insertion that reflows the wrapped tail" should "reflow it in the frame after the keystroke" in
    pendingUntilFixed {
      runTui(wrappedProse) {
        for
          _      <- pressAll(List.fill(190)(TuiKeys.ArrowRight)*)
          before <- settledScreen
          _      <- typeText("QQQQQQQQQQ")
          after  <- runtimeScreen
        yield
          after.containsText("QQQQQQQQQQ") shouldBe true
          // The following visual row is pushed along in the same frame, rather than jumping a moment later.
          after.rowText(2) should not be before.rowText(2)
      }
    }

  // -- Control cases: the same scenarios with the window sitter out of the way ---------------------------------------

  "the same session with the window sitter disabled" should "paint typed text in the very next frame" in
    runTui(withoutWindowSitter) {
      for
        _          <- settledScreen
        _          <- typeText("Z")
        cursorOnly <- paintsCursorOnly
        after      <- runtimeScreen
      yield
        cursorOnly shouldBe false
        after.rowText(1).contains("Z") shouldBe true
    }

  it should "paint a burst of typing immediately too, reflow included" in runTui(withoutWindowSitter) {
    for
      _      <- pressAll(List.fill(190)(TuiKeys.ArrowRight)*)
      before <- settledScreen
      _      <- typeText("QQQQQQQQQQ")
      after  <- runtimeScreen
    yield
      after.containsText("QQQQQQQQQQ") shouldBe true
      after.rowText(2) should not be before.rowText(2)
  }

  "the full-repaint render path" should "always contain the typed character, whatever the sitter is doing" in
    runTui(wrappedProse) {
      for
        _          <- settledScreen
        _          <- typeText("Z")
        forcedFull <- screen
      yield
        // The content itself is fine: it is the runtime's choice of render path, not the renderer, that withholds it.
        forcedFull.rowText(1).contains("Z") shouldBe true
    }

  "the document" should "always receive the keystroke immediately, whatever is painted" in runTui(wrappedProse) {
    for
      _    <- settledScreen
      _    <- typeSlowly("HELLO")
      text <- documentText
    yield text.exists(_.contains("HELLO")) shouldBe true
  }
end TuiTypingLatencySpec
