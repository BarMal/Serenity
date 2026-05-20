package com.serenity.ui.renderer

import com.googlecode.lanterna.TextColor
import com.googlecode.lanterna.graphics.TextGraphics
import com.googlecode.lanterna.screen.Screen
import com.serenity.state.models.*
import com.serenity.ui.layout.*

case class RenderContext(
    screen: Screen,
    graphics: TextGraphics,
    layout: CalculatedLayout
)

object Renderer:

  def render(state: AppState, screen: Screen): Unit =
    val graphics     = screen.newTextGraphics()
    val terminalSize = TerminalSize(screen.getTerminalSize.getColumns, screen.getTerminalSize.getRows)
    val layout       = LayoutEngine.calculateLayout(state, terminalSize)

    // Update viewport dimensions based on actual panel size
    val updatedState = updateViewportDimensions(state, layout)
    val context      = RenderContext(screen, graphics, layout)

    // Clear screen
    graphics.setBackgroundColor(TextColor.ANSI.BLACK)
    graphics.fillRectangle(com.googlecode.lanterna.TerminalPosition.TOP_LEFT_CORNER, screen.getTerminalSize, ' ')

    // Render spacer columns (empty padding)
    renderSpacerColumns(context)

    // Render editor panes
    renderEditorPanes(updatedState, context)

    // Render floating panels if any
    renderFloatingPanels(updatedState, context)

    // Refresh screen
    screen.refresh()

  private def renderSpacerColumns(context: RenderContext): Unit =
    // Spacer columns are intentionally empty for padding
    // Could add subtle visual indicators here if needed (like subtle borders)
    ()

  private def renderEditorPanes(state: AppState, context: RenderContext): Unit =
    state.layout.editorPanes.foreach((paneId, pane) => renderEditorPane(pane, state, context))

  private def renderEditorPane(
    pane: EditorPane,
    state: AppState,
    context: RenderContext
  ): Unit =
    val rect = context.layout.editorPanelRect

    val buffer = pane.bufferId.flatMap(state.buffers.get)

    buffer match
      case Some(buf) => renderBufferContent(pane, buf, rect, state, context)
      case None      => renderEmptyPane(rect, context)

    // Render cursors with buffer data
    buffer.foreach(buf => renderCursors(pane, rect, context, buf.content))

  private def renderBufferContent(
    pane: EditorPane,
    buffer: Buffer,
    rect: LayoutRect,
    state: AppState,
    context: RenderContext
  ): Unit =
    val viewport   = pane.viewport
    val rope       = buffer.content
    val panelWidth = rect.width

    // Calculate all visual lines that should be displayed
    val visualLines = calculateVisualLinesInViewport(rope, viewport, panelWidth)

    // Render each visual line
    visualLines.zipWithIndex.foreach {
      case (visualLine, screenLineIndex) =>
        if screenLineIndex < rect.height then
          val screenY = rect.y + screenLineIndex
          val screenX = rect.x

          // Render the visual line content
          context.graphics.setForegroundColor(TextColor.ANSI.WHITE)

          // Ensure we don't render outside terminal bounds AND panel bounds
          if screenY < context.screen.getTerminalSize.getRows &&
              screenY >= 0 &&
              screenX < context.screen.getTerminalSize.getColumns &&
              screenX >= 0 &&
              screenY < rect.bottom &&
              screenX < rect.right
          then CharacterRenderer.renderStringWithTheme(context.graphics, screenX, screenY, visualLine.content, state.theme, state.syntaxHighlightingEnabled)
    }

  private case class VisualLine(content: String, bufferLine: Int, startColumn: Int, endColumn: Int)

  private def calculateVisualLinesInViewport(
    rope: com.serenity.rope.Rope,
    viewport: Viewport,
    panelWidth: Int
  ): List[VisualLine] =

    def processBufferLines(bufferLine: Int, currentVisualLine: Int): List[VisualLine] =
      if bufferLine >= rope.lineCount || currentVisualLine >= (viewport.topLine + viewport.visibleLines) then List.empty
      else
        val lineContent     = rope.getLine(bufferLine).getOrElse("")
        val wrappedSegments = wrapLineToSegments(lineContent, panelWidth)

        // Process segments for this buffer line
        val (segmentsInViewport, nextVisualLine) =
          wrappedSegments.foldLeft((List.empty[VisualLine], currentVisualLine)) {
            case ((acc, visualLine), (content, startCol, endCol)) =>
              val newVisualLine =
                if visualLine >= viewport.topLine && visualLine < (viewport.topLine + viewport.visibleLines) then
                  acc :+ VisualLine(content, bufferLine, startCol, endCol)
                else acc
              (newVisualLine, visualLine + 1)
          }

        segmentsInViewport ++ processBufferLines(bufferLine + 1, nextVisualLine)

    processBufferLines(0, 0)

  private def wrapLineToSegments(lineContent: String, panelWidth: Int): List[(String, Int, Int)] =
    if lineContent.isEmpty || panelWidth <= 0 then List(("", 0, 0))
    else
      def buildSegments(remaining: String, currentStartColumn: Int): List[(String, Int, Int)] =
        if remaining.isEmpty then List.empty
        else
          val segmentLength  = math.min(remaining.length, panelWidth)
          val segment        = remaining.substring(0, segmentLength)
          val endColumn      = currentStartColumn + segmentLength
          val currentSegment = (segment, currentStartColumn, endColumn)

          currentSegment :: buildSegments(remaining.substring(segmentLength), endColumn)

      buildSegments(lineContent, 0)

  private def renderEmptyPane(rect: LayoutRect, context: RenderContext): Unit =
    // Render empty pane indicator
    val message = "~ Empty ~"
    val centerX = rect.x + (rect.width - message.length) / 2
    val centerY = rect.y + rect.height / 2

    context.graphics.setForegroundColor(TextColor.ANSI.BLACK_BRIGHT)
    if centerY < context.screen.getTerminalSize.getRows && centerX >= 0 then
      CharacterRenderer.renderString(context.graphics, centerX, centerY, message)

  private def renderCursors(
    pane: EditorPane,
    rect: LayoutRect,
    context: RenderContext,
    rope: com.serenity.rope.Rope
  ): Unit =
    val viewport   = pane.viewport
    val panelWidth = rect.width

    pane.cursors.foreach { cursor =>
      // Calculate visual line position for cursor
      calculateCursorVisualPosition(cursor, rope, panelWidth, viewport) match
        case Some((visualLine, visualColumn)) =>
          val screenY = rect.y + (visualLine - viewport.topLine)
          val screenX = rect.x + visualColumn

          // Only render cursor if it's visible in the viewport AND within panel bounds
          if screenY >= rect.y && screenY < rect.bottom &&
              screenX >= rect.x && screenX < rect.right &&
              screenY >= 0 && screenY < context.screen.getTerminalSize.getRows &&
              screenX >= 0 && screenX < context.screen.getTerminalSize.getColumns
          then
            // Highlight cursor position
            context.graphics.setBackgroundColor(TextColor.ANSI.WHITE)
            context.graphics.setForegroundColor(TextColor.ANSI.BLACK)

            // Get character at cursor position or use space
            val cursorChar = ' ' // For now, just use space to show cursor
            CharacterRenderer.renderChar(context.graphics, screenX, screenY, cursorChar)

            // Reset colors
            context.graphics.setBackgroundColor(TextColor.ANSI.BLACK)
            context.graphics.setForegroundColor(TextColor.ANSI.WHITE)
        case None => // Cursor not visible, don't render
    }

  private def calculateCursorVisualPosition(
    cursor: CursorPosition,
    rope: com.serenity.rope.Rope,
    panelWidth: Int,
    viewport: Viewport
  ): Option[(Int, Int)] =

    def findCursorPosition(bufferLine: Int, currentVisualLine: Int): Option[(Int, Int)] =
      if bufferLine >= rope.lineCount then None
      else if bufferLine == cursor.line then
        // Found the buffer line containing the cursor
        val lineContent        = rope.getLine(bufferLine).getOrElse("")
        val visualLineInBuffer = cursor.column / panelWidth // Which visual line within this buffer line
        val visualColumnInLine = cursor.column % panelWidth // Column within that visual line
        val totalVisualLine    = currentVisualLine + visualLineInBuffer
        Some((totalVisualLine, visualColumnInLine))
      else
        // Count visual lines for this buffer line and continue
        val lineContent             = rope.getLine(bufferLine).getOrElse("")
        val visualLinesInThisBuffer = math.max(1, (lineContent.length + panelWidth - 1) / panelWidth)
        findCursorPosition(bufferLine + 1, currentVisualLine + visualLinesInThisBuffer)

    findCursorPosition(0, 0)

  private def updateViewportDimensions(state: AppState, layout: CalculatedLayout): AppState =
    val panelRect = layout.editorPanelRect

    // Update all editor panes to use dynamic viewport dimensions
    val updatedPanes = state.layout.editorPanes.map {
      case (paneId, pane) =>
        val updatedViewport = LayoutEngine.updateViewportDimensions(pane.viewport, panelRect)
        paneId -> pane.copy(viewport = updatedViewport)
    }

    val updatedLayout = state.layout.copy(editorPanes = updatedPanes)
    state.copy(layout = updatedLayout)

  private def renderFloatingPanels(state: AppState, context: RenderContext): Unit =
    context.layout.floatingPanelRect.foreach { rect =>
      // For now, just render a placeholder
      // In the future, this will render command runners, modals, etc. with transparency
      renderFloatingPanelPlaceholder(rect, context)
    }

  private def renderFloatingPanelPlaceholder(rect: LayoutRect, context: RenderContext): Unit =
    // Placeholder implementation - will be enhanced with actual panel content
    context.graphics.setBackgroundColor(TextColor.ANSI.BLACK_BRIGHT)
    context.graphics.setForegroundColor(TextColor.ANSI.WHITE)

    // Fill panel background with semi-transparent effect (simulated)
    for y <- rect.y until rect.bottom; x <- rect.x until rect.right do
      if y < context.screen.getTerminalSize.getRows && x < context.screen.getTerminalSize.getColumns then
        CharacterRenderer.renderChar(context.graphics, x, y, '░') // Light shade character for transparency effect
