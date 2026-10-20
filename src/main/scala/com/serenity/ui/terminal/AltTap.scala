package com.serenity.ui.terminal

import java.awt.event.{InputEvent, KeyEvent}

/** Tells a tap of Alt on its own from Alt used as a modifier, so a shortcut such as Alt+F neither shows nor hides the
  * menu bar. A tap is Alt pressed and released with nothing in between: no other key, no click.
  */
object AltTap:

  enum Input:
    /** `alone` is false when another modifier was already held, as it is for AltGr on Windows (Ctrl+Alt). */
    case AltDown(alone: Boolean)
    case OtherDown
    case AltUp

  final case class State(armed: Boolean)

  object State:
    val Idle: State = State(armed = false)

  /** The next state, and whether this input completed a tap. Holding Alt auto-repeats its press, which stays armed. */
  def step(state: State, input: Input): (State, Boolean) =
    input match
      case Input.AltDown(alone) => (State(armed = alone), false)
      case Input.OtherDown      => (State.Idle, false)
      case Input.AltUp          => (State.Idle, state.armed)

  private val otherModifiers = InputEvent.CTRL_DOWN_MASK | InputEvent.SHIFT_DOWN_MASK | InputEvent.META_DOWN_MASK |
    InputEvent.ALT_GRAPH_DOWN_MASK

  /** What a key event means to [[step]]. A release of any key but Alt and a typed character mean nothing to it. */
  def inputOf(event: KeyEvent): Option[Input] =
    (event.getID, event.getKeyCode) match
      case (KeyEvent.KEY_PRESSED, KeyEvent.VK_ALT) =>
        Some(Input.AltDown(alone = (event.getModifiersEx & otherModifiers) == 0))
      case (KeyEvent.KEY_RELEASED, KeyEvent.VK_ALT) => Some(Input.AltUp)
      case (KeyEvent.KEY_PRESSED, _)                => Some(Input.OtherDown)
      case _                                        => None
