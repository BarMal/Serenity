package com.serenity.state.components

import com.serenity.keystroke.events.{Event, ResizeEvent}
import com.serenity.state.models.AppState
import com.serenity.ui.layout.LayoutEngine

class ResizeComponent extends FocusedComponent:

  def processEvent(event: Event, state: AppState): ComponentResult =
    event match
      case ResizeEvent(newSize) =>
        ComponentResult.StateChange { currentState =>
          LayoutEngine.syncViewportDimensions(currentState, newSize).copy(viewportSize = Some(newSize))
        }
      case _ => ComponentResult.NoChange
