package com.serenity.keystroke.events

sealed trait PeekInputEvent

object PeekInputEvent:
  final case class Navigate(direction: Direction) extends PeekInputEvent
  case object Accept                              extends PeekInputEvent
  case object Dismiss                             extends PeekInputEvent
  case object OtherInput                          extends PeekInputEvent

  given SurfaceInput[PeekInputEvent] with

    def fromIntent(intent: FocusIntent): Option[PeekInputEvent] =
      intent match
        case FocusIntent.Navigate(direction) => Some(Navigate(direction))
        case FocusIntent.Submit              => Some(Accept)
        case FocusIntent.Dismiss             => Some(Dismiss)
        case FocusIntent.Insert(_) | FocusIntent.DeleteBackward | FocusIntent.DeleteForward | FocusIntent.NextGroup |
            FocusIntent.PreviousGroup =>
          Some(OtherInput)
        case FocusIntent.DeleteWordBackward | FocusIntent.DeleteWordForward | FocusIntent.Paste => None

  def fromEvent(event: Event): Option[PeekInputEvent] =
    event match
      case peekEvent: PeekInputEvent => Some(peekEvent)
      case other                     => SurfaceInput.translate[PeekInputEvent](other)
