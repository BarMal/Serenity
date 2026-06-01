package com.serenity.state.reducers

import com.serenity.keystroke.events.{LspEvent, ResizeEvent, SystemEvent}
import com.serenity.state.models.AppState
import com.serenity.ui.layout.LayoutEngine

object SystemEventReducer:

  def reduce(event: SystemEvent, state: AppState): ReducerResult =
    event match
      case ResizeEvent(newSize) =>
        val newLayout = LayoutEngine.calculateLayout(state, newSize)
        val updatedBuffers = state.layout.editorPanes.values.foldLeft(state.buffers) { (buffers, pane) =>
          pane.bufferId.flatMap(buffers.get) match
            case Some(buffer) =>
              val updatedViewport =
                LayoutEngine.updateViewportDimensions(buffer.viewport, newLayout.editorPanelRect)
              buffers + (buffer.id -> buffer.copy(viewport = updatedViewport))
            case None =>
              buffers
        }

        ReducerResult.noEffects(
          state.copy(
            buffers = updatedBuffers,
            viewportSize = Some(newSize)
          )
        )

      case LspEvent.LspDiagnosticsReceived(uri, diagnostics) =>
        ReducerResult.noEffects(state.copy(diagnostics = state.diagnostics + (uri -> diagnostics)))

      case _ =>
        ReducerResult.noEffects(state)
