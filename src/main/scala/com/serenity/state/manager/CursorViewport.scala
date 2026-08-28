package com.serenity.state.manager

import com.serenity.state.models.*
import com.serenity.ui.fonts.FontLoader
import com.serenity.ui.layout.TextLayoutSnapshot

/** Java2D/font measurement for cursor-visibility scrolling belongs at the effect boundary, not in a reducer -- a
  * reducer runs mid-edit against content the effect boundary has not seen yet. `adjustForCursor` is the shared
  * measurement (also used directly by mouse-click and vertical-navigation effect handlers); `ensureVisibleCursors` is
  * the boundary pass that re-applies it after a pure reduce, for every buffer whose primary cursor moved.
  */
object CursorViewport:

  def ensureVisibleCursors(before: AppState, after: AppState): AppState =
    after.persisted.buffers.foldLeft(after) {
      case (state, (bufferId, buffer)) =>
        val cursorMoved =
          before.persisted.buffers
            .get(bufferId)
            .exists(_.editing.cursors.headOption != buffer.editing.cursors.headOption)
        if !cursorMoved then state
        else
          buffer.editing.cursors.headOption match
            case Some(cursor) =>
              val updatedBuffer = buffer.copy(viewport = adjustForCursor(buffer, state, cursor))
              state.copy(persisted =
                state.persisted.copy(buffers = state.persisted.buffers + (bufferId -> updatedBuffer))
              )
            case None => state
    }

  def adjustForCursor(
    buffer: Buffer,
    currentState: AppState,
    cursor: CursorPosition
  ): Viewport =
    val wordWrapEnabled  = currentState.persisted.config.wordWrapEnabled
    val viewport         = buffer.viewport
    val halfVisibleLines = viewport.visibleLines / 2
    val font             = previewFontForBuffer(buffer, currentState.persisted.config.editorConfig.fontConfig)
    val visibleWidthPx =
      TextLayoutSnapshot.gridWrapWidthPx(viewport.visibleColumns, currentState.persisted.config.editorConfig.fontConfig)
    val lineText = buffer.document.content.getLine(cursor.line).getOrElse("")
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
