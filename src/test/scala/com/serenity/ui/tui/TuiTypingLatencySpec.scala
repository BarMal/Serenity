package com.serenity.ui.tui

import com.serenity.animation.WindowSitterConfig
import com.serenity.app.AppRuntime

/** Typed text must reach the terminal in the very next frame the runtime paints, window sitter or not.
  *
  * It did not, and this is why: `AppRuntime.inputEventPhase` marks the window sitter active on every `InsertChar`
  * (`observeWindowSitterTyping`) for as long as `WindowSitterConfig.activeTicks`, and the fast render phase then took
  * the cursor-only path -- caret moved, no content painted -- for every frame where the sitter was active and
  * `needsFullContentRender` was false. That predicate is always false in TUI mode: it looks for character-reveal buffer
  * animations, which never reach the surface-generic render entry points a terminal uses (`docs/tui-mode.md`, "Known
  * degradations"), a theme transition, or surface animations. So a terminal session painted no typed text until the
  * sitter decayed, then repainted the insertion and its reflow together.
  *
  * The stand-down itself is sound where it applies -- the sitter's glyph lives in Swing window chrome and never touches
  * the canvas -- but only on a frame with nothing else to paint, which is what `AppRuntime.canStandDownToCursorOnly`
  * now requires. These tests hold the fix in place from the outside: through the runtime's own render-path choice
  * (`TuiSession.runtimeScreen`), not a restatement of it.
  */
class TuiTypingLatencySpec extends TuiSpec:

  private val paragraph = "the quick brown fox jumps over the lazy dog and keeps running onward " * 6
  private val prose     = (0 until 8).map(index => s"Paragraph $index. $paragraph").mkString("\n\n")

  private val wrappedProse =
    TuiEnvironment.withFile(prose).withConfig(_.withWordWrap(true).withVisualLineCursorNavigation(true))

  /** The same session with the window sitter switched off -- the control case for every assertion below. */
  private val withoutWindowSitter =
    wrappedProse.withConfig(_.withWindowSitterConfig(WindowSitterConfig.default.copy(enabled = false)))

  "typing a character" should "leave the runtime painting content, not the cursor-only path" in
    runTui(wrappedProse) {
      for
        _          <- settledScreen
        _          <- typeText("Z")
        cursorOnly <- paintsCursorOnly
        current    <- state
        s          <- TuiScript.session
        animations <- liftIO(s.stateManager.getBufferAnimations)
      yield
        // The sitter is still ticking, and nothing else asks for a full content repaint -- the keystroke's own damage
        // is what keeps the frame on the content path.
        current.runtime.windowSitter.isActive shouldBe true
        AppRuntime.needsFullContentRender(current, animations) shouldBe false
        animations shouldBe empty
        cursorOnly shouldBe false
    }

  it should "move the caret and paint the character in the same frame" in runTui(wrappedProse) {
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

  it should "need no animation ticks before the character is on screen" in runTui(wrappedProse) {
    for
      _       <- settledScreen
      _       <- typeText("Z")
      duringA <- runtimeScreen
      duringB <- runtimeScreen
      settled <- runtimeScreen
    yield
      duringA.rowText(1).contains("Z") shouldBe true
      // And it stays put: the frames that follow, painted while the sitter is still decaying, do not lose it again.
      duringB.rowText(1).contains("Z") shouldBe true
      settled.rowText(1).contains("Z") shouldBe true
  }

  it should "paint a whole burst of typing as it arrives" in runTui(wrappedProse) {
    for
      _      <- settledScreen
      _      <- typeSlowly("HELLO")
      during <- runtimeScreen
      text   <- documentText
    yield
      text.exists(_.contains("HELLO")) shouldBe true
      during.rowText(1).contains("HELLO") shouldBe true
  }

  it should "reflow the wrapped tail in that same frame" in runTui(wrappedProse) {
    for
      _      <- pressAll(List.fill(190)(TuiKeys.ArrowRight)*)
      before <- settledScreen
      _      <- typeText("QQQQQQQQQQ")
      during <- runtimeScreen
    yield
      // The insertion and the rows it pushes along land together, in the frame the keystroke produced -- what used to
      // arrive a sitter's worth of ticks later, all at once, and read as a jump rather than typing.
      during.rowText(2) should not be before.rowText(2)
      during.containsText("QQQQQQQQQQ") shouldBe true
  }

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

  "an idle frame while the sitter ticks" should "still leave the keystroke's damage for the next content frame" in
    runTui(wrappedProse) {
      for
        _          <- settledScreen
        _          <- typeText("Z")
        cursorOnly <- paintsCursorOnly
        // Asking twice must not consume the pending damage: only a painted frame does that, and the frame that
        // consumes it is the one that draws the character.
        again <- paintsCursorOnly
        after <- runtimeScreen
      yield
        cursorOnly shouldBe false
        again shouldBe false
        after.rowText(1).contains("Z") shouldBe true
    }

  "the full-repaint render path" should "always contain the typed character, whatever the sitter is doing" in
    runTui(wrappedProse) {
      for
        _          <- settledScreen
        _          <- typeText("Z")
        forcedFull <- screen
      yield forcedFull.rowText(1).contains("Z") shouldBe true
    }
end TuiTypingLatencySpec
