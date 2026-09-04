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
    val wordWrapEnabled  = currentState.persisted.config.surfaceConfig.wordWrapEnabled
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

    // The number of visual rows a logical line occupies on screen -- 1 unless word wrap folds it across several
    // rows, in which case it must be measured the same way `cursorVisualLine` above was, or the two disagree.
    def visualRowCountForLine(lineIndex: Int): Int =
      if !wordWrapEnabled then 1
      else
        val text = buffer.document.content.getLine(lineIndex).getOrElse("")
        if buffer.usesTextFont then
          TextLayoutSnapshot.boundedVisualLinesForText(text, lineIndex, visibleWidthPx, font).length.max(1)
        else (text.length / math.max(1, viewport.visibleColumns)) + 1

    // Desired top: walk backward from the cursor's own line in visual rows (not logical lines) until
    // halfVisibleLines rows of context above the cursor's own visual row have been accounted for, or the buffer
    // start is reached. This is what `cursor.line - halfVisibleLines` was trying to approximate, but that
    // subtraction conflated a logical-line count with a visual-row count.
    val scrollUpBudget = halfVisibleLines - cursorVisualLine
    def walkBackward(line: Int, remainingBudget: Int): Int =
      if line <= 0 || remainingBudget <= 0 then line
      else
        val previousLineRows = visualRowCountForLine(line - 1)
        if previousLineRows >= remainingBudget then line - 1
        else walkBackward(line - 1, remainingBudget - previousLineRows)
    val rawTopLine = walkBackward(cursor.line, scrollUpBudget)
    val rawTopVisualLine =
      if rawTopLine == cursor.line then math.max(0, cursorVisualLine - halfVisibleLines) else 0

    // Bottom clamp: the latest (line, visual-row) start that still fills the viewport with real content, found by
    // walking backward from the buffer's last line until visibleLines rows of content have been accounted for.
    // Without this, a cursor near the end of a short-ish document can leave blank rows below the last line.
    val lineCount = buffer.document.content.lineCount
    def bottomAlignedWindow(line: Int, remaining: Int): (Int, Int) =
      val rows = visualRowCountForLine(line)
      if remaining <= rows || line == 0 then (line, math.max(0, rows - remaining))
      else bottomAlignedWindow(line - 1, remaining - rows)
    val (bottomLine, bottomVisualLine) =
      if lineCount <= 0 then (0, 0) else bottomAlignedWindow(lineCount - 1, viewport.visibleLines)

    val exceedsBottom =
      rawTopLine > bottomLine || (rawTopLine == bottomLine && rawTopVisualLine > bottomVisualLine)
    val (clampedTopLine, topVisualLine) =
      if exceedsBottom then (bottomLine, bottomVisualLine) else (rawTopLine, rawTopVisualLine)
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
