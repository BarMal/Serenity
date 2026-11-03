package com.serenity.state.manager

import com.serenity.state.models.*
import com.serenity.ui.layout.{ScrollLimits, WrappedLineCache}

/** Keeps a wheel-scrolled pane from resting past the end of its document. The reducer cannot: how many rows the pane
  * fits and how the last paragraph wraps need font measurement, which belongs at the effect boundary (see
  * [[CursorViewport]]).
  */
object ScrollBoundary:

  /** Whether the wheel moves rows. Column mode pages by column and keeps its own logical-line scroll. */
  def scrollsByRows(state: AppState): Boolean =
    val surfaceConfig = state.persisted.config.surfaceConfig
    !(surfaceConfig.columnModeEnabled && surfaceConfig.wordWrapEnabled)

  /** Moves the pane `rows` on-screen rows down, or up when negative, then clamps it. A line that wraps into many rows
    * is scrolled through row by row, which a step of whole lines cannot do for a document of one long line.
    */
  def scrollRows(
    state: AppState,
    paneId: PaneId,
    rows: Int,
    wrapCache: WrappedLineCache = WrappedLineCache.Uncached
  ): AppState =
    val shown = state.persisted.layout.editorPanes.get(paneId).flatMap(_.bufferId).flatMap(state.persisted.buffers.get)
    shown match
      case Some(buffer) if rows != 0 =>
        val viewport = buffer.viewport
        val counts   = CursorViewport.visualRowsFor(buffer, state, wrapCache)
        val (line, row) =
          if rows > 0 then
            counts
              .rowBelow(viewport.topLine, viewport.topVisualLine + rows)
              .getOrElse((counts.lineCount - 1, Int.MaxValue))
          else if -rows <= viewport.topVisualLine then (viewport.topLine, viewport.topVisualLine + rows)
          else counts.rowAbove(viewport.topLine, -rows - viewport.topVisualLine)
        val moved = buffer.copy(viewport = viewport.scrolledTo(line, viewport.leftColumn, row))
        clampPane(
          state.copy(persisted = state.persisted.copy(buffers = state.persisted.buffers + (buffer.id -> moved))),
          paneId,
          wrapCache
        )
      case _ => state

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
