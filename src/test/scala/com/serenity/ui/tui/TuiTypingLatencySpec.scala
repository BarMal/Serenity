package com.serenity.ui.tui
import com.serenity.animation.WindowSitterConfig
import com.serenity.app.AppRuntime

/** KNOWN DEFECT, reproduced end to end: typed text does not appear until the window sitter stops animating.
  *
  * The reported symptom is that the caret moves as you type but the characters arrive a moment later, all at once, with
  * the surrounding text reflowing around them. This is why:
  *
  *   - `AppRuntime.inputEventPhase` marks the window sitter active on every `InsertChar` (`observeWindowSitterTyping`),
  *     for as long as `WindowSitterConfig.activeTicks`;
  *   - the fast render phase then paints the cursor-only path for every frame where the sitter is active and
  *     `needsFullContentRender` is false -- which moves the caret but paints no content;
  *   - `needsFullContentRender` is false in TUI mode because it looks for character-reveal buffer animations, a theme
  *     transition, or surface animations, and the first of those never reaches the surface-generic render entry points
  *     the terminal uses at all (`docs/tui-mode.md`, "Known degradations").
  *
  * So a terminal session paints no typed text until the sitter decays, and then repaints the lot. The exception the
  * cursor-only branch was written for -- a window-sitter glyph living in the Swing window chrome, never on the canvas
  * -- does not exist in a terminal: there is no chrome to draw it in, so the branch buys nothing and costs every
  * keystroke its visibility.
  *
  * These tests assert the behaviour as it is today. When the fast phase stops taking that branch in TUI mode, the two
  * marked expectations flip: the character appears in the first runtime frame, and `paintsCursorOnly` is false.
  */
class TuiTypingLatencySpec extends TuiSpec:

  private val paragraph = "the quick brown fox jumps over the lazy dog and keeps running onward " * 6
  private val prose     = (0 until 8).map(index => s"Paragraph $index. $paragraph").mkString("\n\n")

  private val wrappedProse =
    TuiEnvironment.withFile(prose).withConfig(_.withWordWrap(true).withVisualLineCursorNavigation(true))

  /** The same session with the window sitter switched off -- the control case for every assertion below. */
  private val withoutWindowSitter =
    wrappedProse.withConfig(_.withWindowSitterConfig(WindowSitterConfig.default.copy(enabled = false)))

  "typing a character" should "leave the runtime painting the cursor-only path, which draws no content" in
    runTui(wrappedProse) {
      for
        _          <- settledScreen
        _          <- typeText("Z")
        cursorOnly <- paintsCursorOnly
        current    <- state
        s          <- TuiScript.session
        animations <- liftIO(s.stateManager.getBufferAnimations)
      yield
        current.runtime.windowSitter.isActive shouldBe true
        // Nothing else asks for a full content repaint, so the sitter's own activity decides the branch.
        AppRuntime.needsFullContentRender(current, animations) shouldBe false
        animations shouldBe empty
        cursorOnly shouldBe true // flips to false when the TUI stops taking the cursor-only branch
    }

  it should "move the caret in that frame but leave the character off the screen" in runTui(wrappedProse) {
    for
      _      <- settledScreen
      before <- runtimeScreen
      _      <- typeText("Z")
      after  <- runtimeScreen
      text   <- documentText
    yield
      // The keystroke reached the document immediately...
      text.exists(_.contains("Z")) shouldBe true
      // ...and the caret moved with it...
      after.caret._1 shouldBe before.caret._1 + 1
      // ...but the frame the runtime actually painted has no 'Z' on it. This is the delay the user sees.
      after.rowText(1).contains("Z") shouldBe false
      after.rowText(1) shouldBe before.rowText(1)
  }

  it should "show the character only once the window sitter has decayed" in runTui(wrappedProse) {
    for
      _       <- settledScreen
      _       <- typeText("Z")
      duringA <- runtimeScreen
      duringB <- runtimeScreen
      _       <- advanceUntilFullRepaint
      settled <- runtimeScreen
    yield
      duringA.rowText(1).contains("Z") shouldBe false
      duringB.rowText(1).contains("Z") shouldBe false
      // Only after the sitter's own animation budget runs out does the text arrive.
      settled.rowText(1).contains("Z") shouldBe true
  }

  it should "hold back a whole burst of typing, then paint it in one frame" in runTui(wrappedProse) {
    for
      _      <- settledScreen
      _      <- typeSlowly("HELLO")
      during <- runtimeScreen
      text   <- documentText
      _      <- advanceUntilFullRepaint
      after  <- runtimeScreen
    yield
      text.exists(_.contains("HELLO")) shouldBe true
      during.rowText(1).contains("HELLO") shouldBe false
      after.rowText(1).contains("HELLO") shouldBe true
  }

  it should "reflow the wrapped tail in that same delayed frame" in runTui(wrappedProse) {
    for
      _      <- pressAll(List.fill(190)(TuiKeys.ArrowRight)*)
      before <- settledScreen
      _      <- typeText("QQQQQQQQQQ")
      during <- runtimeScreen
      _      <- advanceUntilFullRepaint
      after  <- runtimeScreen
    yield
      // Nothing at all changes while the sitter is active -- neither the inserted text nor the rows it pushes along.
      during.rowText(2) shouldBe before.rowText(2)
      // Then the insertion and the reflow of the following visual row land together, which is what makes the delay
      // look like a jump rather than a keystroke.
      after.rowText(2) should not be before.rowText(2)
      after.containsText("QQQQQQQQQQ") shouldBe true
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

  "the full-repaint render path" should "always contain the typed character, whatever the sitter is doing" in
    runTui(wrappedProse) {
      for
        _          <- settledScreen
        _          <- typeText("Z")
        forcedFull <- screen
      yield
        // Proof that the content itself is fine: it is the runtime's choice of render path, not the renderer, that
        // withholds it.
        forcedFull.rowText(1).contains("Z") shouldBe true
    }
end TuiTypingLatencySpec
