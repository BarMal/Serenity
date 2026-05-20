package com.serenity.state.components

import com.serenity.keystroke.events.{Event, ResizeEvent}
import com.serenity.state.models.AppState
import com.serenity.ui.layout.LayoutEngine

class ResizeComponent extends FocusedComponent:

  def processEvent(event: Event, state: AppState): ComponentResult =
    event match
      case ResizeEvent(newSize) =>
        ComponentResult.StateChange { currentState =>
          // Update all pane viewports based on new terminal size
          val newLayout = LayoutEngine.calculateLayout(currentState, newSize)
          val updatedPanes = currentState.layout.editorPanes.map {
            case (paneId, pane) =>
              val updatedViewport = LayoutEngine.updateViewportDimensions(pane.viewport, newLayout.editorPanelRect)
              paneId -> pane.copy(viewport = updatedViewport)
          }

          currentState.copy(
            layout = currentState.layout.copy(editorPanes = updatedPanes),
            terminalSize = Some(newSize)
          )
        }
      case _ => ComponentResult.NoChange
