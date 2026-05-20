package com.serenity.state.components

import com.serenity.keystroke.events.*
import com.serenity.rope.Rope
import com.serenity.state.models.*

class EditorPaneComponent(
    paneId: PaneId
)(using balance: com.serenity.rope.Balance)
    extends FocusedComponent:

  def processEvent(event: Event, currentState: AppState): ComponentResult =
    currentState.layout.editorPanes.get(paneId) match
      case Some(pane) => processEventForPane(event, pane, currentState)
      case None       => ComponentResult.noChange

  private def processEventForPane(
    event: Event,
    pane: EditorPane,
    currentState: AppState
  ): ComponentResult =
    event match
      case textEvent: TextEntryEvent => processTextEvent(textEvent, pane, currentState)
      case _                         => ComponentResult.noChange

  private def processTextEvent(
    event: TextEntryEvent,
    pane: EditorPane,
    currentState: AppState
  ): ComponentResult =
    pane.bufferId match
      case Some(bufferId) =>
        currentState.buffers.get(bufferId) match
          case Some(buffer) => processTextEventForBuffer(event, buffer, pane, currentState)
          case None         => ComponentResult.noChange
      case None => handleEventWithoutBuffer(event, pane, currentState)

  private def processTextEventForBuffer(
    event: TextEntryEvent,
    buffer: Buffer,
    pane: EditorPane,
    currentState: AppState
  ): ComponentResult =
    // Get the primary cursor (first in the list)
    pane.cursors.headOption match
      case Some(cursor) =>
        event match
          case InsertChar(char) =>
            val offset          = lineColumnToOffset(buffer.content, cursor.line, cursor.column)
            val newContent      = buffer.content.insert(offset, char.toString)
            val updatedBuffer   = buffer.copy(content = newContent, isDirty = true)
            val newCursor       = cursor.copy(column = cursor.column + 1)
            val updatedViewport = adjustViewportForCursor(pane.viewport, newCursor)
            val updatedPane     = pane.copy(cursors = newCursor :: pane.cursors.tail, viewport = updatedViewport)

            ComponentResult.updateState { state =>
              state.copy(
                buffers = state.buffers + (buffer.id -> updatedBuffer),
                layout = state.layout.copy(
                  editorPanes = state.layout.editorPanes + (paneId -> updatedPane)
                )
              )
            }

          case DeleteBackward =>
            val offset = lineColumnToOffset(buffer.content, cursor.line, cursor.column)
            if offset > 0 then
              val newContent    = buffer.content.delete(offset - 1, offset)
              val updatedBuffer = buffer.copy(content = newContent, isDirty = true)
              val newCursor =
                if cursor.column > 0 then cursor.copy(column = cursor.column - 1)
                else if cursor.line > 0 then
                  // Move to end of previous line (before deletion)
                  val prevLineEnd = findLineEnd(buffer.content, cursor.line - 1)
                  cursor.copy(line = cursor.line - 1, column = prevLineEnd)
                else cursor
              val updatedViewport = adjustViewportForCursor(pane.viewport, newCursor)
              val updatedPane     = pane.copy(cursors = newCursor :: pane.cursors.tail, viewport = updatedViewport)

              ComponentResult.updateState { state =>
                state.copy(
                  buffers = state.buffers + (buffer.id -> updatedBuffer),
                  layout = state.layout.copy(
                    editorPanes = state.layout.editorPanes + (paneId -> updatedPane)
                  )
                )
              }
            else ComponentResult.noChange

          case DeleteForward =>
            val offset = lineColumnToOffset(buffer.content, cursor.line, cursor.column)
            if offset < buffer.content.weight then
              val newContent    = buffer.content.delete(offset, offset + 1)
              val updatedBuffer = buffer.copy(content = newContent, isDirty = true)

              ComponentResult.updateState { state =>
                state.copy(
                  buffers = state.buffers + (buffer.id -> updatedBuffer)
                )
              }
            else ComponentResult.noChange

          case MoveLeft =>
            val newCursor =
              if cursor.column > 0 then cursor.moveLeft
              else if cursor.line > 0 then
                // Move to end of previous line
                val prevLineEnd = findLineEnd(buffer.content, cursor.line - 1)
                cursor.copy(line = cursor.line - 1, column = prevLineEnd)
              else cursor
            val updatedViewport = adjustViewportForCursor(pane.viewport, newCursor)
            val updatedPane     = pane.copy(cursors = newCursor :: pane.cursors.tail, viewport = updatedViewport)

            ComponentResult.updateState { state =>
              state.copy(
                layout = state.layout.copy(
                  editorPanes = state.layout.editorPanes + (paneId -> updatedPane)
                )
              )
            }

          case MoveRight =>
            val currentLineEnd = findLineEnd(buffer.content, cursor.line)
            val newCursor =
              if cursor.column < currentLineEnd then cursor.moveRight
              else
                // Move to start of next line
                val totalLines = countLines(buffer.content)
                if cursor.line < totalLines - 1 then cursor.copy(line = cursor.line + 1, column = 0)
                else cursor
            val updatedViewport = adjustViewportForCursor(pane.viewport, newCursor)
            val updatedPane     = pane.copy(cursors = newCursor :: pane.cursors.tail, viewport = updatedViewport)

            ComponentResult.updateState { state =>
              state.copy(
                layout = state.layout.copy(
                  editorPanes = state.layout.editorPanes + (paneId -> updatedPane)
                )
              )
            }

          case MoveUp =>
            // Use visual line navigation for wrapped text
            // Calculate actual panel width from current terminal size
            val terminalSize = com.serenity.ui.layout.TerminalSize(80, 24) // TODO: get actual terminal size
            val layout       = com.serenity.ui.layout.LayoutEngine.calculateLayout(currentState, terminalSize)
            val panelWidth   = layout.editorPanelRect.width

            val newCursor       = moveUpVisualLine(cursor, buffer.content, panelWidth)
            val updatedViewport = adjustViewportForCursor(pane.viewport, newCursor)
            val updatedPane     = pane.copy(cursors = newCursor :: pane.cursors.tail, viewport = updatedViewport)

            ComponentResult.updateState { state =>
              state.copy(
                layout = state.layout.copy(
                  editorPanes = state.layout.editorPanes + (paneId -> updatedPane)
                )
              )
            }

          case MoveDown =>
            // Use visual line navigation for wrapped text
            // Calculate actual panel width from current terminal size
            val terminalSize = com.serenity.ui.layout.TerminalSize(80, 24) // TODO: get actual terminal size
            val layout       = com.serenity.ui.layout.LayoutEngine.calculateLayout(currentState, terminalSize)
            val panelWidth   = layout.editorPanelRect.width

            val newCursor       = moveDownVisualLine(cursor, buffer.content, panelWidth)
            val updatedViewport = adjustViewportForCursor(pane.viewport, newCursor)
            val updatedPane     = pane.copy(cursors = newCursor :: pane.cursors.tail, viewport = updatedViewport)

            ComponentResult.updateState { state =>
              state.copy(
                layout = state.layout.copy(
                  editorPanes = state.layout.editorPanes + (paneId -> updatedPane)
                )
              )
            }

          case NewLine | Enter =>
            val offset          = lineColumnToOffset(buffer.content, cursor.line, cursor.column)
            val newContent      = buffer.content.insert(offset, "\n")
            val updatedBuffer   = buffer.copy(content = newContent, isDirty = true)
            val newCursor       = cursor.copy(line = cursor.line + 1, column = 0)
            val updatedViewport = adjustViewportForCursor(pane.viewport, newCursor)
            val updatedPane     = pane.copy(cursors = newCursor :: pane.cursors.tail, viewport = updatedViewport)

            ComponentResult.updateState { state =>
              state.copy(
                buffers = state.buffers + (buffer.id -> updatedBuffer),
                layout = state.layout.copy(
                  editorPanes = state.layout.editorPanes + (paneId -> updatedPane)
                )
              )
            }

          case MoveToStart =>
            val newCursor       = cursor.copy(column = 0)
            val updatedViewport = adjustViewportForCursor(pane.viewport, newCursor)
            val updatedPane     = pane.copy(cursors = newCursor :: pane.cursors.tail, viewport = updatedViewport)

            ComponentResult.updateState { state =>
              state.copy(
                layout = state.layout.copy(
                  editorPanes = state.layout.editorPanes + (paneId -> updatedPane)
                )
              )
            }

          case MoveToEnd =>
            val lineEnd         = findLineEnd(buffer.content, cursor.line)
            val newCursor       = cursor.copy(column = lineEnd)
            val updatedViewport = adjustViewportForCursor(pane.viewport, newCursor)
            val updatedPane     = pane.copy(cursors = newCursor :: pane.cursors.tail, viewport = updatedViewport)

            ComponentResult.updateState { state =>
              state.copy(
                layout = state.layout.copy(
                  editorPanes = state.layout.editorPanes + (paneId -> updatedPane)
                )
              )
            }

          case ToggleSyntaxHighlighting =>
            // Toggle syntax highlighting for the entire application
            ComponentResult.updateState { state =>
              state.copy(syntaxHighlightingEnabled = !state.syntaxHighlightingEnabled)
            }
            
          case _: HotkeyEvent =>
            // TODO: Implement other hotkey handling (save, quit, etc.)
            ComponentResult.noChange

          case _ =>
            // Handle unimplemented TextEntryEvents
            ComponentResult.noChange

      case None =>
        // No cursors - create a default cursor
        val defaultCursor = CursorPosition(0, 0)
        val updatedPane   = pane.copy(cursors = List(defaultCursor))
        ComponentResult.updateState { state =>
          state.copy(
            layout = state.layout.copy(
              editorPanes = state.layout.editorPanes + (paneId -> updatedPane)
            )
          )
        }

  private def handleEventWithoutBuffer(
    event: TextEntryEvent,
    pane: EditorPane,
    currentState: AppState
  ): ComponentResult =
    // Create a scratch buffer for text entry events when no buffer exists
    event match
      case InsertChar(char) =>
        val bufferId    = currentState.nextBufferId
        val buffer      = Buffer.fromString(bufferId, char.toString).copy(isDirty = true)
        val newCursor   = CursorPosition(0, 1)
        val updatedPane = pane.copy(bufferId = Some(bufferId), cursors = List(newCursor))

        ComponentResult.updateState { state =>
          state.copy(
            buffers = state.buffers + (bufferId -> buffer),
            layout = state.layout.copy(
              editorPanes = state.layout.editorPanes + (paneId -> updatedPane)
            ),
            nextBufferId = BufferId(bufferId.value + 1)
          )
        }
      case _ =>
        // For other events without a buffer, ensure we still return a state change
        // to maintain immutability expectations
        ComponentResult.updateState(identity)

  // Helper functions for coordinate conversion
  private def lineColumnToOffset(rope: Rope, line: Int, column: Int): Int =
    val lines               = rope.collect().split('\n')
    val previousLinesLength = lines.take(line).map(_.length + 1).sum // +1 for newline
    val lineStart           = if line == 0 then 0 else previousLinesLength
    Math.min(lineStart + column, rope.weight)

  private def findLineEnd(rope: Rope, line: Int): Int =
    val content = rope.collect()
    val lines   = content.split('\n')
    if line >= 0 && line < lines.length then lines(line).length else 0

  private def countLines(rope: Rope): Int =
    val content = rope.collect()
    if content.isEmpty then 1 else content.count(_ == '\n') + 1

  // Viewport adjustment to keep cursor visible
  private def adjustViewportForCursor(viewport: Viewport, cursor: CursorPosition): Viewport =
    // Functional approach: calculate all adjustments then apply them
    val horizontalAdjustment =
      if cursor.column < viewport.leftColumn then
        // Cursor is to the left of visible area - scroll left
        viewport.copy(leftColumn = cursor.column)
      else if cursor.column >= viewport.leftColumn + viewport.visibleColumns then
        // Cursor is to the right of visible area - scroll right
        val newLeftColumn = cursor.column - viewport.visibleColumns + 1
        viewport.copy(leftColumn = math.max(0, newLeftColumn))
      else viewport

    // Apply vertical scrolling to the horizontally adjusted viewport
    if cursor.line < horizontalAdjustment.topLine then
      // Cursor is above visible area - scroll up
      horizontalAdjustment.copy(topLine = cursor.line)
    else if cursor.line >= horizontalAdjustment.topLine + horizontalAdjustment.visibleLines then
      // Cursor is below visible area - scroll down
      val newTopLine = cursor.line - horizontalAdjustment.visibleLines + 1
      horizontalAdjustment.copy(topLine = math.max(0, newTopLine))
    else horizontalAdjustment

  /** Move cursor up by one visual line, handling wrapped text */
  private def moveUpVisualLine(cursor: CursorPosition, rope: Rope, panelWidth: Int): CursorPosition =
    if cursor.line == 0 && cursor.column < panelWidth then
      // Already at top of first line
      cursor.copy(column = 0)
    else
      val currentLineContent        = rope.getLine(cursor.line).getOrElse("")
      val currentVisualLineInBuffer = cursor.column / panelWidth

      if currentVisualLineInBuffer > 0 then
        // Move up within the same buffer line
        val newColumn = cursor.column - panelWidth
        cursor.copy(column = math.max(0, newColumn))
      else
        // Move to previous buffer line
        if cursor.line > 0 then
          val prevLineContent = rope.getLine(cursor.line - 1).getOrElse("")
          if prevLineContent.length <= panelWidth then
            // Previous line doesn't wrap
            val newColumn = math.min(cursor.column, prevLineContent.length)
            cursor.copy(line = cursor.line - 1, column = newColumn)
          else
            // Previous line wraps - go to its last visual line
            val lastVisualLineInPrev   = (prevLineContent.length - 1) / panelWidth
            val baseColumnInLastVisual = lastVisualLineInPrev * panelWidth
            val newColumn = math.min(baseColumnInLastVisual + (cursor.column % panelWidth), prevLineContent.length)
            cursor.copy(line = cursor.line - 1, column = newColumn)
        else cursor // Can't move up from first line

  /** Move cursor down by one visual line, handling wrapped text */
  private def moveDownVisualLine(cursor: CursorPosition, rope: Rope, panelWidth: Int): CursorPosition =
    val currentLineContent        = rope.getLine(cursor.line).getOrElse("")
    val currentVisualLineInBuffer = cursor.column / panelWidth
    val totalVisualLinesInCurrent = math.max(1, (currentLineContent.length + panelWidth - 1) / panelWidth)

    if currentVisualLineInBuffer < totalVisualLinesInCurrent - 1 then
      // Move down within the same buffer line
      val newColumn = cursor.column + panelWidth
      cursor.copy(column = math.min(newColumn, currentLineContent.length))
    else
      // Move to next buffer line
      if cursor.line < rope.lineCount - 1 then
        val nextLineContent      = rope.getLine(cursor.line + 1).getOrElse("")
        val targetColumnInVisual = cursor.column % panelWidth
        val newColumn            = math.min(targetColumnInVisual, nextLineContent.length)
        cursor.copy(line = cursor.line + 1, column = newColumn)
      else cursor // Can't move down from last line
