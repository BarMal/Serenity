package com.serenity.state.manager

import cats.effect.IO
import com.serenity.state.models.*
import com.serenity.state.reducers.{AppEventReducer, SystemEventReducer}
import com.serenity.ui.fonts.FontLoader
import com.serenity.ui.fonts.FontLoader.FontConfig
import com.serenity.ui.layout.{CellMetrics, TextLayoutSnapshot, ViewportSize}

private[manager] trait StateManagerViewportBehavior extends StateManagerSurfaceFacadeBehavior:
  this: StateManager =>

  def ensureCursorVisible(paneId: PaneId): IO[Unit] =
    stateRef.update { state =>
      state.layout.editorPanes.get(paneId) match
        case Some(pane) =>
          pane.bufferId.flatMap(state.buffers.get) match
            case Some(buffer) =>
              val cursor         = buffer.cursors.headOption.getOrElse(CursorPosition(0, 0))
              val viewport       = buffer.viewport
              val font           = previewFontForBuffer(buffer, state.config.fontConfig)
              val visibleWidthPx = viewport.visibleColumns * CellMetrics.fromFont(font).charWidth
              val lineText       = buffer.content.getLine(cursor.line).getOrElse("")
              val newLeftColumn =
                if usesMeasuredHorizontalViewport(buffer) then
                  TextLayoutSnapshot.leftColumnForCursorVisibility(lineText, cursor.column, visibleWidthPx, font)
                else
                  val measuredLeftColumn =
                    TextLayoutSnapshot.leftColumnForCursorVisibility(lineText, cursor.column, visibleWidthPx, font)
                  val minimumVisibleColumn = math.max(0, cursor.column - viewport.visibleColumns + 1)
                  math.max(minimumVisibleColumn, measuredLeftColumn)
              val newTopLine =
                if cursor.line < viewport.topLine then cursor.line
                else if cursor.line >= viewport.topLine + viewport.visibleLines then
                  cursor.line - viewport.visibleLines + 1
                else viewport.topLine
              val newViewport = viewport.copy(
                topLine = math.max(0, newTopLine),
                leftColumn = math.max(0, newLeftColumn)
              )
              val updatedBuffer = buffer.copy(viewport = newViewport)
              state.copy(buffers = state.buffers + (buffer.id -> updatedBuffer))
            case None => state
        case None => state
    }

  def smoothScrollTo(paneId: PaneId, targetLine: Int): IO[Unit] =
    stateRef.update { state =>
      state.layout.editorPanes.get(paneId) match
        case Some(pane) =>
          val updatedPane = pane.copy(
            smoothScrolling = Some(SmoothScrollState(targetTopLine = targetLine, progress = 0.0))
          )
          state.copy(layout =
            state.layout.copy(
              editorPanes = state.layout.editorPanes + (paneId -> updatedPane)
            )
          )
        case None => state
    }

  def progressSmoothScroll(paneId: PaneId, progress: Double): IO[Unit] =
    stateRef.update { state =>
      state.layout.editorPanes.get(paneId) match
        case Some(pane) =>
          pane.bufferId.flatMap(state.buffers.get) match
            case Some(buffer) =>
              pane.smoothScrolling match
                case Some(SmoothScrollState(targetTopLine, _)) =>
                  val currentTopLine = buffer.viewport.topLine
                  val (newTopLine, newSmoothing) =
                    if progress >= 1.0 then (targetTopLine, None)
                    else
                      val interpolated =
                        math.round(currentTopLine + progress * (targetTopLine - currentTopLine)).toInt
                      (interpolated, Some(SmoothScrollState(targetTopLine, progress)))

                  val updatedBuffer = buffer.copy(viewport = buffer.viewport.copy(topLine = newTopLine))
                  val updatedPane   = pane.copy(smoothScrolling = newSmoothing)

                  state.copy(
                    buffers = state.buffers + (buffer.id -> updatedBuffer),
                    layout = state.layout.copy(
                      editorPanes = state.layout.editorPanes + (paneId -> updatedPane)
                    )
                  )
                case None => state
            case None => state
        case None => state
    }

  def clickMinimap(paneId: PaneId, targetLine: Int): IO[Unit] =
    stateRef.update { state =>
      state.layout.editorPanes.get(paneId) match
        case Some(pane) =>
          pane.bufferId.flatMap(state.buffers.get) match
            case Some(buffer) =>
              val halfVisible = buffer.viewport.visibleLines / 2
              val newTopLine  = math.max(0, targetLine - halfVisible)
              val updatedBuffer = buffer.copy(
                cursors = List(CursorPosition(targetLine, 0)),
                viewport = buffer.viewport.copy(topLine = newTopLine)
              )
              state.copy(buffers = state.buffers + (buffer.id -> updatedBuffer))
            case None => state
        case None => state
    }

  def handleViewportResize(newSize: ViewportSize): IO[Unit] =
    for
      _            <- logger.debug(s"Handling viewport resize to ${newSize.width}x${newSize.height}")
      currentState <- stateRef.get
      resizedState = SystemEventReducer.reduce(com.serenity.keystroke.events.ResizeEvent(newSize), currentState).state
      rebalancedState = AppEventReducer.rebalancePanes(resizedState, resizedState.focusedBufferId)
      _ <- validateAndUpdateState(rebalancedState, currentState)
    yield ()

  private def previewFontForBuffer(
    buffer: Buffer,
    config: FontConfig
  ): java.awt.Font =
    if buffer.usesTextFont then FontLoader.previewTextFont(config)
    else FontLoader.previewCodeFont(config)

  private def usesMeasuredHorizontalViewport(buffer: Buffer): Boolean =
    buffer.usesTextFont
