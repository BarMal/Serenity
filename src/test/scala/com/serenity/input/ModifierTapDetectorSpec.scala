package com.serenity.input

import com.serenity.keystroke.Modifier
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Covers the shell-agnostic double-tap detector ported from `SwingInputHandler`'s inline
  * `pendingModifierTap`/`doubleTapWindowMillis` logic (`SwingInputHandler.scala:63-65,288-317`), including the
  * `bf5de9e0` regression ("tap requires release"): a second press of the same modifier without an intervening release
  * must never fire, no matter how quickly it follows the first.
  */
class ModifierTapDetectorSpec extends AnyFlatSpec with Matchers:

  private val Window = ModifierTapDetector.WindowMillis

  "a press, release, press of the same modifier within the window" should "emit that modifier's bare keystroke" in {
    val s0                                      = ModifierTapState.empty
    val ModifierTapDetector.Outcome.Pending(s1) = ModifierTapDetector.modifierPressed(s0, Modifier.Ctrl, 0L): @unchecked
    val s2                                      = ModifierTapDetector.modifierReleased(s1, Modifier.Ctrl, 5L)
    val outcome                                 = ModifierTapDetector.modifierPressed(s2, Modifier.Ctrl, Window)

    outcome shouldBe ModifierTapDetector.Outcome.Emit(ModifierTapState.empty)
  }

  "a second press without an intervening release (auto-repeat)" should "be ignored and preserve the pending tap" in {
    val s0                                      = ModifierTapState.empty
    val ModifierTapDetector.Outcome.Pending(s1) = ModifierTapDetector.modifierPressed(s0, Modifier.Ctrl, 0L): @unchecked
    val outcome                                 = ModifierTapDetector.modifierPressed(s1, Modifier.Ctrl, 10L)

    outcome shouldBe ModifierTapDetector.Outcome.Pending(s1)
  }

  "a repeated press after the repeat is finally released and re-pressed in time" should "still emit" in {
    val s0                                      = ModifierTapState.empty
    val ModifierTapDetector.Outcome.Pending(s1) = ModifierTapDetector.modifierPressed(s0, Modifier.Ctrl, 0L): @unchecked
    val ModifierTapDetector.Outcome.Pending(s2) =
      ModifierTapDetector.modifierPressed(s1, Modifier.Ctrl, 10L): @unchecked
    val s3      = ModifierTapDetector.modifierReleased(s2, Modifier.Ctrl, 15L)
    val outcome = ModifierTapDetector.modifierPressed(s3, Modifier.Ctrl, 100L)

    outcome shouldBe ModifierTapDetector.Outcome.Emit(ModifierTapState.empty)
  }

  "a press-press with no release in between, ever" should "never fire even much later (bf5de9e0)" in {
    val s0                                      = ModifierTapState.empty
    val ModifierTapDetector.Outcome.Pending(s1) = ModifierTapDetector.modifierPressed(s0, Modifier.Ctrl, 0L): @unchecked
    val outcome                                 = ModifierTapDetector.modifierPressed(s1, Modifier.Ctrl, 10_000L)

    outcome shouldBe ModifierTapDetector.Outcome.Pending(s1)
  }

  "a second press of the same modifier outside the window" should "not emit, and restart tracking as a fresh tap" in {
    val s0                                      = ModifierTapState.empty
    val ModifierTapDetector.Outcome.Pending(s1) = ModifierTapDetector.modifierPressed(s0, Modifier.Ctrl, 0L): @unchecked
    val s2                                      = ModifierTapDetector.modifierReleased(s1, Modifier.Ctrl, 5L)
    val outcome                                 = ModifierTapDetector.modifierPressed(s2, Modifier.Ctrl, Window + 1)

    outcome should matchPattern { case ModifierTapDetector.Outcome.Pending(_) => }
    outcome should not be ModifierTapDetector.Outcome.Emit(ModifierTapState.empty)
  }

  "a release of a modifier with no pending tap for it" should "be a no-op" in {
    val s0 = ModifierTapState.empty
    ModifierTapDetector.modifierReleased(s0, Modifier.Ctrl, 5L) shouldBe s0
  }

  "a release of a different modifier than the one pending" should "leave the pending tap untouched" in {
    val s0                                      = ModifierTapState.empty
    val ModifierTapDetector.Outcome.Pending(s1) = ModifierTapDetector.modifierPressed(s0, Modifier.Ctrl, 0L): @unchecked
    val s2                                      = ModifierTapDetector.modifierReleased(s1, Modifier.Alt, 5L)

    s2 shouldBe s1
  }

  "interleaving two different modifiers" should "not cross-fire a double-tap" in {
    val s0                                      = ModifierTapState.empty
    val ModifierTapDetector.Outcome.Pending(s1) = ModifierTapDetector.modifierPressed(s0, Modifier.Ctrl, 0L): @unchecked
    val s2                                      = ModifierTapDetector.modifierReleased(s1, Modifier.Ctrl, 5L)
    val outcome                                 = ModifierTapDetector.modifierPressed(s2, Modifier.Alt, 10L)

    outcome should not be a[ModifierTapDetector.Outcome.Emit]
  }

  "pressing any other (non-modifier) key" should "cancel a pending tap, matching SwingInputHandler.scala:241" in {
    val s0                                      = ModifierTapState.empty
    val ModifierTapDetector.Outcome.Pending(s1) = ModifierTapDetector.modifierPressed(s0, Modifier.Ctrl, 0L): @unchecked
    val s2                                      = ModifierTapDetector.modifierReleased(s1, Modifier.Ctrl, 5L)
    val s3                                      = ModifierTapDetector.otherKeyPressed(s2)

    val outcome = ModifierTapDetector.modifierPressed(s3, Modifier.Ctrl, 10L)
    outcome should not be a[ModifierTapDetector.Outcome.Emit]
  }

  "pressing an unrelated key with no pending tap" should "remain a harmless no-op" in {
    ModifierTapDetector.otherKeyPressed(ModifierTapState.empty) shouldBe ModifierTapState.empty
  }
