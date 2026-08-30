package com.serenity.input

import com.serenity.keystroke.Modifier

/** Tracking state for [[CursorPeekDetector]]: wraps a [[ModifierTapState]] dedicated to the configured cursor-peek
  * modifier, entirely separate from `SwingInputHandler.pendingModifierTap` (which drives `ctrl+ctrl`-style hotkey
  * double-taps). The two state machines run in parallel over the same raw key events without seeing each other.
  */
final case class CursorPeekState private[input] (tap: ModifierTapState)

object CursorPeekState:
  val empty: CursorPeekState = CursorPeekState(ModifierTapState.empty)

/** Distinguishes, for the *configured* command-runner cursor-peek modifier specifically, a held bare press (-> "peek")
  * from a double-tap (-> "open fully") from neither -- by composing [[ModifierTapDetector]]'s existing
  * press/release/window state machine rather than re-implementing its timing logic (the window is
  * [[ModifierTapDetector.WindowMillis]], matched by `SurfaceConfig.commandRunnerCursorPeekTapWindowMillis`'s default of
  * 200L).
  *
  * ===Optimistic peek, upgraded to a double-tap===
  *
  * The first bare press of the configured modifier always begins a peek immediately ([[Outcome.PeekBegin]]) -- there is
  * no way to know yet whether it is the start of a hold or the first half of a double-tap, so the caller shows the peek
  * optimistically. If a second press of the same modifier follows within the window (with an intervening release, per
  * `ModifierTapDetector`'s `bf5de9e0` guard against mistaking held-key auto-repeat for a second tap), that peek is
  * upgraded to a full open ([[Outcome.DoubleTapOpen]]) instead. Releasing the modifier while a peek is showing signals
  * the caller to begin the settle-out ([[Outcome.PeekEnd]]) -- for a plain hold this is the end of the gesture; for the
  * first half of a double-tap it is superseded moments later by [[Outcome.DoubleTapOpen]] if the second press arrives
  * in time, which the caller (reducer/effect-handler wiring, not this pure detector) is responsible for reconciling.
  *
  * ===Isolation from other modifiers===
  *
  * Only press/release events for the configured peek modifier touch this detector's state at all -- pressing or
  * releasing any other modifier (including ones with their own pending `ctrl+ctrl`-style tap in `SwingInputHandler`'s
  * separate tracking) is a complete no-op here, leaving any in-progress peek tracking untouched. A non-modifier key
  * press still cancels a pending peek, matching
  * `ModifierTapDetector.otherKeyPressed`/`SwingInputHandler.translatePressed`'s existing behaviour.
  */
object CursorPeekDetector:

  enum Outcome:
    /** A bare press of the configured modifier while nothing was pending: begin the peek presentation immediately.
      * Optimistic -- may still be superseded by [[DoubleTapOpen]] if a second press follows within the window.
      */
    case PeekBegin(state: CursorPeekState)

    /** The second press of the configured modifier within the window, following an intervening release: abandon any
      * in-progress peek and open the command runner fully, exactly like today's normal open.
      */
    case DoubleTapOpen(state: CursorPeekState)

    /** A release that ended an in-progress peek (or a non-modifier key press that cancelled a pending one): begin the
      * settle-out presentation.
      */
    case PeekEnd(state: CursorPeekState)

    /** Nothing the caller needs to act on: a different modifier's press/release, auto-repeat of an already-held
      * configured-modifier press, or a release/otherKeyPressed with no peek in progress.
      */
    case Unchanged(state: CursorPeekState)

  /** A press of `modifier` at `atMillis`, given the currently configured `peekModifier`. */
  def modifierPressed(
    state: CursorPeekState,
    modifier: Modifier,
    peekModifier: Modifier,
    atMillis: Long
  ): Outcome =
    if modifier != peekModifier then Outcome.Unchanged(state)
    else
      ModifierTapDetector.modifierPressed(state.tap, modifier, atMillis) match
        case ModifierTapDetector.Outcome.Emit(next) =>
          Outcome.DoubleTapOpen(CursorPeekState(next))
        case ModifierTapDetector.Outcome.Pending(next) =>
          // A fresh press (first ever, or a restart after the window lapsed) lands a *new* PendingTap stamped with
          // this event's own `atMillis`; auto-repeat of an already-held press leaves the original pending tap (and
          // its original timestamp) untouched. Comparing timestamps is how we tell the two apart without
          // duplicating `ModifierTapDetector`'s own held-vs-fresh branching.
          if next.pending.exists(_.pressedAtMillis == atMillis) then Outcome.PeekBegin(CursorPeekState(next))
          else Outcome.Unchanged(CursorPeekState(next))

  /** A release of `modifier` at `atMillis`, given the currently configured `peekModifier`. */
  def modifierReleased(
    state: CursorPeekState,
    modifier: Modifier,
    peekModifier: Modifier,
    atMillis: Long
  ): Outcome =
    if modifier != peekModifier then Outcome.Unchanged(state)
    else
      val wasHeld = state.tap.pending.exists(tap => tap.modifier == peekModifier && !tap.released)
      val next    = CursorPeekState(ModifierTapDetector.modifierReleased(state.tap, modifier, atMillis))
      if wasHeld then Outcome.PeekEnd(next) else Outcome.Unchanged(next)

  /** Any non-modifier key press: cancels a pending peek, matching `ModifierTapDetector.otherKeyPressed`. */
  def otherKeyPressed(state: CursorPeekState): Outcome =
    if state.tap.pending.isEmpty then Outcome.Unchanged(state)
    else Outcome.PeekEnd(CursorPeekState(ModifierTapDetector.otherKeyPressed(state.tap)))
