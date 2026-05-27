package com.serenity.state.components

import com.serenity.keystroke.events.*
import com.serenity.state.models.AppState

class PeekOverlayComponent() extends TypedFocusedComponent[PeekInputEvent]:

  protected def decodeEvent(event: Event): Option[PeekInputEvent] =
    PeekInputEvent.fromEvent(event)

  protected def processTypedEvent(event: PeekInputEvent, currentState: AppState): ComponentResult =
    event match
      case PeekInputEvent.Navigate(_) =>
        ComponentResult.dismiss
      case PeekInputEvent.Accept =>
        ComponentResult.dismiss
      case PeekInputEvent.Dismiss =>
        ComponentResult.dismiss
      case PeekInputEvent.OtherInput =>
        ComponentResult.dismiss
