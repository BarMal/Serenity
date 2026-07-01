package com.serenity.state.reducers

import com.serenity.state.models.*
import com.serenity.ui.fonts.FontLoader
import com.serenity.ui.layout.{CellMetrics, TextLayoutSnapshot}

object CursorViewport:

  def adjustForCursor(
    buffer: Buffer,
    currentState: AppState,
    cursor: CursorPosition
  ): Viewport =
    val wordWrapEnabled  = currentState.config.wordWrapEnabled
    val viewport         = buffer.viewport
    val halfVisibleLines = viewport.visibleLines / 2
    val font             = previewFontForBuffer(buffer, currentState.config.fontConfig)
    val visibleWidthPx   = viewport.visibleColumns * CellMetrics.fromFont(font).charWidth
    val lineText         = buffer.content.getLine(cursor.line).getOrElse("")
    val measuredCursorVisualLine =
      if buffer.usesTextFont then
        TextLayoutSnapshot.visualLineIndexForCursor(
          lineText,
          cursor.column,
          visibleWidthPx,
          font,
          wordWrapEnabled = wordWrapEnabled
        )
      else cursor.column / math.max(1, viewport.visibleColumns)
    val cursorVisualLine =
      if !wordWrapEnabled then 0
      else measuredCursorVisualLine
    val targetTopLine =
      if cursorVisualLine > halfVisibleLines then cursor.line
      else cursor.line - halfVisibleLines
    val clampedTopLine = math.max(0, targetTopLine)
    val topVisualLine =
      if clampedTopLine == cursor.line then math.max(0, cursorVisualLine - halfVisibleLines)
      else 0
    val clampedLeftColumn =
      if wordWrapEnabled then 0
      else
        val measuredLeftColumn =
          TextLayoutSnapshot.leftColumnForCursorVisibility(lineText, cursor.column, visibleWidthPx, font)
        val minimumVisibleColumn = math.max(0, cursor.column - viewport.visibleColumns + 1)
        math.max(minimumVisibleColumn, measuredLeftColumn)

    viewport.copy(
      topLine = clampedTopLine,
      leftColumn = clampedLeftColumn,
      topVisualLine = topVisualLine
    )

  private def previewFontForBuffer(
    buffer: Buffer,
    config: FontLoader.FontConfig
  ): java.awt.Font =
    FontLoader.previewFontForRole(config, buffer.typographyRole)
