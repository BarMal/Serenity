package com.serenity.state.components

import com.serenity.keystroke.events.Event
import com.serenity.state.models.AppState

class StartupPageComponent extends TypedFocusedComponent[Event]:

  protected def decodeEvent(event: Event): Option[Event] =
    Some(event)

  protected def processTypedEvent(event: Event, currentState: AppState): ComponentResult =
    ComponentResult.noChange
