package com.serenity.state.components

import com.serenity.keystroke.events.Event
import com.serenity.state.models.AppState

trait FocusedComponent:
  def processEvent(event: Event, currentState: AppState): ComponentResult

trait ComponentState

object ComponentState:
  case object Empty extends ComponentState
