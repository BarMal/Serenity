package com.serenity.keystroke.events

sealed trait PanelInputEvent

object PanelInputEvent:
  final case class Navigate(direction: Direction) extends PanelInputEvent

  /** Leave the panel for the editor because the key was meant for it: typing, Delete, Tab. */
  case object ReturnFocus extends PanelInputEvent

  /** Leave the panel on Escape, for wherever the current app mode's setting says Escape returns focus. */
  case object Dismiss extends PanelInputEvent

  case object Activate extends PanelInputEvent
  case object NoOp     extends PanelInputEvent
  case object First    extends PanelInputEvent
  case object Last     extends PanelInputEvent

  /** A page down (positive `delta`) or up (negative). */
  final case class Page(delta: Int) extends PanelInputEvent

  /** Grows (positive `delta`) or shrinks (negative) the focused panel (issue #1310) -- the keyboard leg of the
    * command/keyboard/drag resize trio, all of which end up at the same `PanelStateReducer.resize`.
    */
  final case class Resize(delta: Int) extends PanelInputEvent

  given SurfaceInput[PanelInputEvent] with

    def fromIntent(intent: FocusIntent): Option[PanelInputEvent] =
      intent match
        case FocusIntent.Navigate(direction) => Some(Navigate(direction))
        case FocusIntent.Submit              => Some(Activate)
        case FocusIntent.Dismiss             => Some(Dismiss)
        case FocusIntent.Insert(_) | FocusIntent.DeleteBackward | FocusIntent.DeleteForward | FocusIntent.NextGroup |
            FocusIntent.PreviousGroup =>
          Some(ReturnFocus)
        case FocusIntent.DeleteWordBackward | FocusIntent.DeleteWordForward | FocusIntent.Paste => None

  def fromEvent(event: Event): Option[PanelInputEvent] =
    event match
      case panelEvent: PanelInputEvent => Some(panelEvent)
      case other                       => SurfaceInput.translate[PanelInputEvent](other)
