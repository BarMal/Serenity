package com.serenity.input

import com.serenity.config.{ChromeElement, SummonConfig}
import com.serenity.keystroke.{InputKey, Modifier}

/** Follows the summon chord from raw key presses and releases: which elements it holds, whether a lock press came
  * during the hold, and which keys it consumes so they neither type nor trigger a hotkey.
  *
  * Pure, with the clock passed in, so the timing is tested with chosen timestamps. It knows no AWT: the Swing handler
  * turns a `KeyEvent` into a [[RawKey]] and the modifiers held.
  *
  * ===Auto-repeat===
  *
  * X11 and XWayland deliver a held key's auto-repeat as a release followed at once by a press of the same key, while
  * Windows and macOS send presses alone. A release is therefore not final until [[RepeatGapMillis]] have passed with no
  * press of that key: it waits as pending, a press of the same key inside the gap cancels it, and anything else settles
  * it, whether another key arriving or [[flushed]] being called when the handler's timer lapses.
  */
object SummonChordTracker:

  /** Longer than the few milliseconds the pair of a repeat spans, far shorter than a person can release and press. */
  val RepeatGapMillis: Long = 12L

  enum Symbol:
    case Glyph(character: Char)
    case Named(key: InputKey)
    case Mod(modifier: Modifier)
    case Other

  /** `code` identifies the physical key, so a repeat's press matches its release. */
  final case class RawKey(code: Int, symbol: Symbol)

  enum Signal:
    case Held(element: ChromeElement)
    case Released(element: ChromeElement)
    case Locked

  final case class State(
      held: Map[Int, ChromeElement],
      pendingRelease: Map[Int, Long],
      swallowTyped: Boolean
  )

  object State:
    val empty: State = State(Map.empty, Map.empty, swallowTyped = false)

  final case class Result(state: State, signals: List[Signal], consumed: Boolean)

  def pressed(
    state: State,
    config: SummonConfig,
    key: RawKey,
    modifiers: Set[Modifier],
    atMillis: Long
  ): Result =
    val isRepeat = state.pendingRelease.get(key.code).exists(releasedAt => atMillis - releasedAt <= RepeatGapMillis)
    val (settled, ended) = settle(state, repeating = Option.when(isRepeat)(key.code))
    val inChord          = modifiers == config.modifiers
    val chordElement = key.symbol match
      case Symbol.Glyph(character) if inChord => config.elementForLetter(character)
      case _                                  => None
    chordElement match
      case Some(element) =>
        val next = settled.copy(held = settled.held.updated(key.code, element), swallowTyped = true)
        Result(next, ended ++ Option.unless(settled.held.contains(key.code))(Signal.Held(element)), consumed = true)
      case None if inChord && settled.held.nonEmpty && isLock(config, key) =>
        Result(settled.copy(swallowTyped = true), ended :+ Signal.Locked, consumed = true)
      case None => Result(settled.copy(swallowTyped = false), ended, consumed = false)

  def released(state: State, config: SummonConfig, key: RawKey, atMillis: Long): Result =
    val cleared = state.copy(swallowTyped = false)
    key.symbol match
      case Symbol.Mod(modifier) if config.modifiers.contains(modifier) && state.held.nonEmpty => ended(cleared)
      case _ if state.held.contains(key.code) =>
        Result(cleared.copy(pendingRelease = cleared.pendingRelease.updated(key.code, atMillis)), Nil, consumed = false)
      case _ => Result(cleared, Nil, consumed = false)

  /** Settles every pending release: the repeat window has lapsed with no press. */
  def flushed(state: State): Result =
    val (settled, signals) = settle(state, repeating = None)
    Result(settled, signals, consumed = false)

  /** Ends everything held, as when the window loses focus and the releases will never arrive. */
  def reset(state: State): Result = ended(state)

  /** Whether the character the OS typed for the last consumed key is to be dropped, and the state with that spent. */
  def typed(state: State): (State, Boolean) = (state.copy(swallowTyped = false), state.swallowTyped)

  private def ended(state: State): Result =
    Result(State.empty, elementsOf(state.held).map(Signal.Released(_)), consumed = false)

  private def elementsOf(held: Map[Int, ChromeElement]): List[ChromeElement] =
    held.values.toList.distinct.sortBy(_.ordinal)

  /** Every pending release ends its element, except the key whose press showed the release was a repeat. */
  private def settle(state: State, repeating: Option[Int]): (State, List[Signal]) =
    val finished = state.pendingRelease.keySet -- repeating
    (
      state.copy(held = state.held -- finished, pendingRelease = Map.empty),
      elementsOf(state.held.filter((code, _) => finished.contains(code))).map(Signal.Released(_))
    )

  private def isLock(config: SummonConfig, key: RawKey): Boolean =
    key.symbol match
      case Symbol.Glyph(character) =>
        config.lock.keyType == InputKey.Character && config.lock.character.contains(character.toLower)
      case Symbol.Named(named) => config.lock.keyType == named
      case _                   => false
