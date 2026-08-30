package com.serenity.input

import com.serenity.keystroke.Modifier
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Covers `CursorPeekDetector`'s hold-vs-double-tap-vs-neither classification for the *configured* peek modifier,
  * mirroring `ModifierTapDetectorSpec`'s press/release/window coverage (including the `bf5de9e0` auto-repeat guard,
  * inherited by composition) plus cases specific to this detector: the optimistic peek on first press, upgrading a peek
  * to a full open on a second tap within the window, ending a peek on release or an unrelated key press, and isolation
  * from other modifiers' press/release traffic.
  */
class CursorPeekDetectorSpec extends AnyFlatSpec with Matchers:

  private val Window = ModifierTapDetector.WindowMillis
  private val Peek   = Modifier.Meta
  private val Other  = Modifier.Ctrl

  private def press(state: CursorPeekState, modifier: Modifier, atMillis: Long) =
    CursorPeekDetector.modifierPressed(state, modifier, Peek, atMillis)

  private def release(state: CursorPeekState, modifier: Modifier, atMillis: Long) =
    CursorPeekDetector.modifierReleased(state, modifier, Peek, atMillis)

  "a first bare press of the configured modifier" should "begin a peek immediately" in {
    val outcome = press(CursorPeekState.empty, Peek, 0L)

    outcome shouldBe a[CursorPeekDetector.Outcome.PeekBegin]
  }

  "a press, release, press of the configured modifier within the window" should "upgrade to a double-tap open" in {
    val CursorPeekDetector.Outcome.PeekBegin(s1) = press(CursorPeekState.empty, Peek, 0L): @unchecked
    val CursorPeekDetector.Outcome.PeekEnd(s2)   = release(s1, Peek, 5L): @unchecked
    val outcome                                  = press(s2, Peek, Window)

    outcome shouldBe a[CursorPeekDetector.Outcome.DoubleTapOpen]
  }

  "releasing the configured modifier while a peek is showing" should "signal the peek to end" in {
    val CursorPeekDetector.Outcome.PeekBegin(s1) = press(CursorPeekState.empty, Peek, 0L): @unchecked
    val outcome                                  = release(s1, Peek, 5L)

    outcome shouldBe a[CursorPeekDetector.Outcome.PeekEnd]
  }

  "auto-repeat of an already-held configured modifier (no intervening release)" should "not re-fire PeekBegin" in {
    val CursorPeekDetector.Outcome.PeekBegin(s1) = press(CursorPeekState.empty, Peek, 0L): @unchecked
    val outcome                                  = press(s1, Peek, 10L)

    outcome shouldBe CursorPeekDetector.Outcome.Unchanged(s1)
  }

  "holding the configured modifier well past the double-tap window, with repeated auto-repeat presses" should
    "never upgrade to a double-tap open (bf5de9e0)" in {
      val CursorPeekDetector.Outcome.PeekBegin(s1) = press(CursorPeekState.empty, Peek, 0L): @unchecked
      val outcome                                  = press(s1, Peek, 10_000L)

      outcome should not be a[CursorPeekDetector.Outcome.DoubleTapOpen]
    }

  "releasing after a long, repeatedly auto-repeated hold" should "still signal the peek to end" in {
    val CursorPeekDetector.Outcome.PeekBegin(s1) = press(CursorPeekState.empty, Peek, 0L): @unchecked
    val CursorPeekDetector.Outcome.Unchanged(s2) = press(s1, Peek, 10_000L): @unchecked
    val outcome                                  = release(s2, Peek, 10_050L)

    outcome shouldBe a[CursorPeekDetector.Outcome.PeekEnd]
  }

  "a second press of the configured modifier exactly at the window boundary" should "still upgrade to a double-tap open" in {
    val CursorPeekDetector.Outcome.PeekBegin(s1) = press(CursorPeekState.empty, Peek, 0L): @unchecked
    val CursorPeekDetector.Outcome.PeekEnd(s2)   = release(s1, Peek, 5L): @unchecked
    val outcome                                  = press(s2, Peek, Window)

    outcome shouldBe a[CursorPeekDetector.Outcome.DoubleTapOpen]
  }

  "a second press of the configured modifier just outside the window" should "restart tracking as a fresh peek, not a double-tap" in {
    val CursorPeekDetector.Outcome.PeekBegin(s1) = press(CursorPeekState.empty, Peek, 0L): @unchecked
    val CursorPeekDetector.Outcome.PeekEnd(s2)   = release(s1, Peek, 5L): @unchecked
    val outcome                                  = press(s2, Peek, Window + 1)

    outcome shouldBe a[CursorPeekDetector.Outcome.PeekBegin]
  }

  "a press of a different modifier" should "be a no-op that leaves the detector's state untouched" in {
    val CursorPeekDetector.Outcome.PeekBegin(s1) = press(CursorPeekState.empty, Peek, 0L): @unchecked
    val outcome                                  = press(s1, Other, 10L)

    outcome shouldBe CursorPeekDetector.Outcome.Unchanged(s1)
  }

  "a release of a different modifier than the configured one" should "be a no-op" in {
    val CursorPeekDetector.Outcome.PeekBegin(s1) = press(CursorPeekState.empty, Peek, 0L): @unchecked
    val outcome                                  = release(s1, Other, 5L)

    outcome shouldBe CursorPeekDetector.Outcome.Unchanged(s1)
  }

  "interleaving a different modifier between the two taps of the configured modifier" should
    "not prevent the double-tap from firing" in {
      val CursorPeekDetector.Outcome.PeekBegin(s1) = press(CursorPeekState.empty, Peek, 0L): @unchecked
      val CursorPeekDetector.Outcome.PeekEnd(s2)   = release(s1, Peek, 5L): @unchecked
      val CursorPeekDetector.Outcome.Unchanged(s3) = press(s2, Other, 10L): @unchecked
      val outcome                                  = press(s3, Peek, 15L)

      outcome shouldBe a[CursorPeekDetector.Outcome.DoubleTapOpen]
    }

  "a release with no pending tap" should "be a harmless no-op" in {
    val outcome = release(CursorPeekState.empty, Peek, 5L)

    outcome shouldBe CursorPeekDetector.Outcome.Unchanged(CursorPeekState.empty)
  }

  "pressing any other (non-modifier) key while a peek is pending" should "cancel it and signal the peek to end" in {
    val CursorPeekDetector.Outcome.PeekBegin(s1) = press(CursorPeekState.empty, Peek, 0L): @unchecked
    val outcome                                  = CursorPeekDetector.otherKeyPressed(s1)

    outcome shouldBe a[CursorPeekDetector.Outcome.PeekEnd]
  }

  "pressing any other (non-modifier) key with no pending tap" should "remain a harmless no-op" in {
    val outcome = CursorPeekDetector.otherKeyPressed(CursorPeekState.empty)

    outcome shouldBe CursorPeekDetector.Outcome.Unchanged(CursorPeekState.empty)
  }

  "a double-tap open" should "reset tracking back to empty" in {
    val CursorPeekDetector.Outcome.PeekBegin(s1)     = press(CursorPeekState.empty, Peek, 0L): @unchecked
    val CursorPeekDetector.Outcome.PeekEnd(s2)       = release(s1, Peek, 5L): @unchecked
    val CursorPeekDetector.Outcome.DoubleTapOpen(s3) = press(s2, Peek, Window): @unchecked

    s3 shouldBe CursorPeekState.empty
  }
