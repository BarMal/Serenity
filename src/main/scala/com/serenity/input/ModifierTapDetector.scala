package com.serenity.input

import com.serenity.keystroke.Modifier

/** Tracking state for [[ModifierTapDetector]]: at most one modifier's tap is ever pending at a time, matching
  * `SwingInputHandler`'s single `pendingModifierTap` slot (`SwingInputHandler.scala:63`) -- pressing a second,
  * different modifier while one is pending simply replaces it rather than tracking both.
  */
final case class ModifierTapState private[input] (pending: Option[ModifierTapState.PendingTap])

object ModifierTapState:
  final case class PendingTap(modifier: Modifier, pressedAtMillis: Long, released: Boolean)

  val empty: ModifierTapState = ModifierTapState(None)

/** Shell-agnostic double-tap-modifier detector: a bare press-release-press of the same modifier within [[WindowMillis]]
  * emits that modifier's bare [[com.serenity.keystroke.KeyStrokeInfo]] (`ctrl+ctrl`-style hotkey bindings). Ported out
  * of `SwingInputHandler.translateModifierPressed`/`translateModifierReleased` (`SwingInputHandler.scala:288-317`) so
  * `TerminalInputHandler` can drive the identical state machine over kitty-protocol bare-modifier press/release events,
  * instead of duplicating the 200ms-window logic.
  *
  * Pure state-transition functions rather than a mutable class: `SwingInputHandler` wraps this over an
  * `AtomicReference` (matching its existing style for that handler), `TerminalInputHandler` over a `Ref[IO, _]`.
  *
  * ===The `bf5de9e0` regression: a tap requires a release===
  *
  * A second press of the same modifier is only ever treated as the second half of a double-tap if a release of that
  * same modifier was observed in between. Held-down auto-repeat -- the platform/terminal re-sending "pressed" for a key
  * that was never released -- must never be mistaken for a double-tap, however long it repeats for; see
  * [[modifierPressed]]'s middle case.
  */
object ModifierTapDetector:

  /** How long a release is allowed to wait for the second press before the pending tap expires. Mirrors
    * `SwingInputHandler.doubleTapWindowMillis` (`SwingInputHandler.scala:65`) exactly, so `ctrl+ctrl` bindings behave
    * identically in both input modes.
    */
  val WindowMillis: Long = 200L

  enum Outcome:
    /** The double-tap fired: the caller should emit `KeyStrokeInfo(modifier's InputKey, None, Set.empty)`. The tap
      * tracking resets to empty.
      */
    case Emit(state: ModifierTapState)

    /** Nothing to emit yet; `state` is the tracking state to carry into the next call. */
    case Pending(state: ModifierTapState)

  /** A press of `modifier` at `atMillis`. */
  def modifierPressed(state: ModifierTapState, modifier: Modifier, atMillis: Long): Outcome =
    state.pending match
      case Some(tap)
          if tap.modifier == modifier && tap.released && atMillis >= tap.pressedAtMillis &&
            atMillis - tap.pressedAtMillis <= WindowMillis =>
        Outcome.Emit(ModifierTapState.empty)
      case Some(tap) if tap.modifier == modifier && !tap.released =>
        // Auto-repeat of a still-held modifier: ignore, and keep the original pending tap unchanged rather than
        // resetting its timestamp -- the bf5de9e0 regression this guards against.
        Outcome.Pending(state)
      case _ =>
        Outcome.Pending(ModifierTapState(Some(ModifierTapState.PendingTap(modifier, atMillis, released = false))))

  /** A release of `modifier` at `atMillis`. A no-op unless `modifier` is the one currently pending and not yet marked
    * released.
    */
  def modifierReleased(state: ModifierTapState, modifier: Modifier, atMillis: Long): ModifierTapState =
    state.pending match
      case Some(tap) if tap.modifier == modifier && !tap.released =>
        ModifierTapState(Some(tap.copy(released = true)))
      case _ => state

  /** Any non-modifier key press cancels a pending tap, matching `SwingInputHandler.translatePressed`'s unconditional
    * `pendingModifierTap.set(None)` (`SwingInputHandler.scala:241`) before dispatching a real key.
    */
  def otherKeyPressed(state: ModifierTapState): ModifierTapState = ModifierTapState.empty
