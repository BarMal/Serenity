package com.serenity.state.manager

import com.serenity.state.models.*
import com.serenity.ui.layout.{ScrollLimits, WrappedLineCache}

/** Keeps a wheel-scrolled pane from resting past the end of its document. The reducer cannot: how many rows the pane
  * fits and how the last paragraph wraps need font measurement, which belongs at the effect boundary (see
  * [[CursorViewport]]).
  */
object ScrollBoundary:

  def clampPane(state: AppState, paneId: PaneId, wrapCache: WrappedLineCache = WrappedLineCache.Uncached): AppState =
    val surfaceConfig = state.persisted.config.surfaceConfig
    val columnMode    = surfaceConfig.columnModeEnabled && surfaceConfig.wordWrapEnabled
    val shown = state.persisted.layout.editorPanes.get(paneId).flatMap(_.bufferId).flatMap(state.persisted.buffers.get)
    shown match
      case Some(buffer) if !columnMode =>
        val viewport = buffer.viewport
        val limit = ScrollLimits.last(
          CursorViewport.visualRowsFor(buffer, state, wrapCache),
          CursorViewport.rowsThatFit(buffer, state)
        )
        val (line, row) = limit.clamp(viewport.topLine, viewport.topVisualLine)
        if line == viewport.topLine && row == viewport.topVisualLine then state
        else
          val moved = buffer.copy(viewport = viewport.scrolledTo(line, viewport.leftColumn, row))
          state.copy(persisted = state.persisted.copy(buffers = state.persisted.buffers + (buffer.id -> moved)))
      case _ => state
