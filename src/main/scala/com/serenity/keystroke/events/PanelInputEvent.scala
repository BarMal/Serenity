package com.serenity.keystroke.events

sealed trait PanelInputEvent

object PanelInputEvent:
  final case class Navigate(direction: Direction) extends PanelInputEvent
  case object ReturnFocus                         extends PanelInputEvent
  case object Activate                            extends PanelInputEvent
  case object NoOp                                extends PanelInputEvent

  /** Input the panel cannot use hands focus back to the editor rather than being swallowed. The word deletes are
    * declined outright: the panel has no text to delete, so returning focus on them would be a guess.
    */
  given SurfaceInput[PanelInputEvent] with

    def fromIntent(intent: FocusIntent): Option[PanelInputEvent] =
      intent match
        case FocusIntent.Navigate(direction) => Some(Navigate(direction))
        case FocusIntent.Submit              => Some(Activate)
        case FocusIntent.Insert(_) | FocusIntent.DeleteBackward | FocusIntent.DeleteForward | FocusIntent.NextGroup |
            FocusIntent.PreviousGroup | FocusIntent.Dismiss =>
          Some(ReturnFocus)
        case FocusIntent.DeleteWordBackward | FocusIntent.DeleteWordForward | FocusIntent.Paste => None

  def fromEvent(event: Event): Option[PanelInputEvent] =
    event match
      case panelEvent: PanelInputEvent => Some(panelEvent)
      case other                       => SurfaceInput.translate[PanelInputEvent](other)
