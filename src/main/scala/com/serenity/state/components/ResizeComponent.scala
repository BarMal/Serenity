package com.serenity.state.components

import com.serenity.keystroke.events.{Event, ResizeEvent}
import com.serenity.state.models.AppState
import com.serenity.ui.layout.LayoutEngine

class ResizeComponent extends FocusedComponent:

  def processEvent(event: Event, state: AppState): ComponentResult =
    event match
      case ResizeEvent(newSize) =>
        ComponentResult.StateChange { currentState =>
          val newLayout = LayoutEngine.calculateLayout(currentState, newSize)
          val updatedBuffers = currentState.layout.editorPanes.values.foldLeft(currentState.buffers) {
            case (buffers, pane) =>
              pane.bufferId.flatMap(buffers.get) match
                case Some(buffer) =>
                  val updatedViewport =
                    LayoutEngine.updateViewportDimensions(buffer.viewport, newLayout.editorPanelRect)
                  buffers + (buffer.id -> buffer.copy(viewport = updatedViewport))
                case None =>
                  buffers
          }

          currentState.copy(
            buffers = updatedBuffers,
            viewportSize = Some(newSize)
          )
        }
      case _ => ComponentResult.NoChange
