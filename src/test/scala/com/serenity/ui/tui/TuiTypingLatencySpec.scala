package com.serenity.ui.tui

/** Typed text must reach the terminal in the very next frame the runtime paints.
  *
  * This used to regress specifically when the window sitter was active: `AppRuntime.inputEventPhase` marked it active
  * on every `InsertChar` for as long as `WindowSitterConfig.activeTicks`, and the fast render phase took a cursor-only
  * path -- caret moved, no content painted -- for every frame where the sitter was active and no other animation
  * needed a full content repaint. That predicate was always false in TUI mode (`docs/tui-mode.md`, "Known
  * degradations"), so a terminal session painted no typed text until the sitter decayed.
  *
  * Issue #934 v2 retired the window sitter and, with it, the fast phase's only cursor-only shortcut -- see
  * `AppRuntime.needsFullContentRender`'s doc. The fast phase now always takes the full-repaint path, so the bug class
  * this spec guards against can no longer occur structurally; these tests are kept as a plain regression guard against
  * a similar shortcut being reintroduced without checking pending keystroke damage.
  */
class TuiTypingLatencySpec extends TuiSpec:

  private val paragraph = "the quick brown fox jumps over the lazy dog and keeps running onward " * 6
  private val prose     = (0 until 8).map(index => s"Paragraph $index. $paragraph").mkString("\n\n")

  private val wrappedProse =
    TuiEnvironment.withFile(prose).withConfig(_.withWordWrap(true).withVisualLineCursorNavigation(true))

  "typing a character" should "move the caret and paint the character in the same frame" in runTui(wrappedProse) {
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
      during.rowText(2) should not be before.rowText(2)
      during.containsText("QQQQQQQQQQ") shouldBe true
  }
end TuiTypingLatencySpec
