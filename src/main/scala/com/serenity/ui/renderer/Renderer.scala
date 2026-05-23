package com.serenity.ui.renderer

import com.googlecode.lanterna.TextColor
import com.googlecode.lanterna.graphics.TextGraphics
import com.googlecode.lanterna.screen.Screen
import com.serenity.state.models.*
import com.serenity.ui.layout.*

case class RenderContext(
    screen: Screen,
    graphics: TextGraphics,
    layout: CalculatedLayout,
    cursorVisible: Boolean = true
)

object Renderer:

  def render(state: AppState, cursorVisible: Boolean, screen: Screen): Unit =
    val graphics     = screen.newTextGraphics()
    val terminalSize = TerminalSize(screen.getTerminalSize.getColumns, screen.getTerminalSize.getRows)
    val layout       = LayoutEngine.calculateLayout(state, terminalSize)
    val context      = RenderContext(screen, graphics, layout, cursorVisible)

    // Clear screen
    graphics.setBackgroundColor(TextColor.ANSI.BLACK)
    graphics.fillRectangle(com.googlecode.lanterna.TerminalPosition.TOP_LEFT_CORNER, screen.getTerminalSize, ' ')

    // Render spacer columns (empty padding)
    renderSpacerColumns(context)

    // Render editor panes
    renderEditorPanes(state, context)

    // Render floating panels if any
    renderFloatingPanels(state, context)

    // Render command runner overlay if active
    renderCommandRunner(state, context)

    // Refresh screen
    screen.refresh()

  def renderCursorOnly(state: AppState, cursorVisible: Boolean, screen: Screen): Unit =
    val graphics     = screen.newTextGraphics()
    val terminalSize = TerminalSize(screen.getTerminalSize.getColumns, screen.getTerminalSize.getRows)
    val layout       = LayoutEngine.calculateLayout(state, terminalSize)
    val paneLayouts  = LayoutEngine.calculatePaneLayouts(state, layout)

    for
      paneId <- state.layout.activeEditorPaneId
      pane   <- state.layout.editorPanes.get(paneId)
      rect   <- paneLayouts.get(paneId)
      buffer <- pane.bufferId.flatMap(state.buffers.get)
      cursor <- buffer.cursors.headOption
    do
      // Adjust for header: content area starts 1 line below the header
      val contentRect = LayoutRect(rect.x, rect.y + 1, rect.width, math.max(1, rect.height - 1))

      calculateCursorVisualPosition(cursor, buffer.content, contentRect.width, buffer.viewport) match
        case Some((visualLine, visualColumn)) =>
          val screenY = contentRect.y + (visualLine - buffer.viewport.topLine)
          val screenX = contentRect.x + visualColumn

          if screenY >= 0 && screenY < terminalSize.height &&
              screenX >= 0 && screenX < terminalSize.width
          then
            if cursorVisible then
              graphics.setBackgroundColor(TextColor.ANSI.WHITE)
              graphics.setForegroundColor(TextColor.ANSI.BLACK)
              CharacterRenderer.renderChar(graphics, screenX, screenY, ' ')
            else
              val charBeneath =
                buffer.content
                  .getLine(cursor.line)
                  .map(line => if cursor.column < line.length then line(cursor.column) else ' ')
                  .getOrElse(' ')
              graphics.setBackgroundColor(state.theme.backgroundColor)
              graphics.setForegroundColor(state.theme.foregroundColor)
              CharacterRenderer.renderChar(graphics, screenX, screenY, charBeneath)

        case None => ()

    screen.refresh()

  private def renderSpacerColumns(context: RenderContext): Unit =
    // Spacer columns are intentionally empty for padding
    // Could add subtle visual indicators here if needed (like subtle borders)
    ()

  private def renderEditorPanes(state: AppState, context: RenderContext): Unit =
    // Calculate individual pane layouts to prevent overlap
    val terminalSize = TerminalSize(context.screen.getTerminalSize.getColumns, context.screen.getTerminalSize.getRows)
    val paneLayouts  = LayoutEngine.calculatePaneLayouts(state, context.layout)

    // Render each pane to its own rectangle
    state.layout.editorPanes.foreach { (paneId, pane) =>
      paneLayouts.get(paneId) match
        case Some(paneRect) => renderEditorPane(pane, paneRect, state, context)
        case None           => // Pane not in layout (shouldn't happen)
    }

  private def renderEditorPane(
    pane: EditorPane,
    rect: LayoutRect,
    state: AppState,
    context: RenderContext
  ): Unit =

    val buffer = pane.bufferId.flatMap(state.buffers.get)

    // Render buffer header (1 line at top)
    renderBufferHeader(pane, buffer, rect, state, context)

    // Adjust content area to account for header
    val contentRect = LayoutRect(rect.x, rect.y + 1, rect.width, math.max(1, rect.height - 1))

    buffer match
      case Some(buf) if buf.content.weight == 0 && buf.isNewEmpty =>
        renderWelcomeText(contentRect, context)
      case Some(buf) if buf.content.weight == 0 =>
        renderEmptyPane(contentRect, context)
      case Some(buf) =>
        renderBufferContent(pane, buf, contentRect, state, context)
      case None =>
        renderEmptyPane(contentRect, context)

    // Render cursors with buffer data (adjusted for header)
    buffer.foreach(buf => renderCursors(buf, contentRect, context))

  private def renderBufferHeader(
    pane: EditorPane,
    buffer: Option[Buffer],
    rect: LayoutRect,
    state: AppState,
    context: RenderContext
  ): Unit =
    val graphics = context.graphics
    val isActive = state.layout.activeEditorPaneId.contains(pane.id)

    // Set header colors
    if isActive then
      graphics.setBackgroundColor(TextColor.ANSI.CYAN)
      graphics.setForegroundColor(TextColor.ANSI.BLACK)
    else
      graphics.setBackgroundColor(TextColor.ANSI.BLACK_BRIGHT)
      graphics.setForegroundColor(TextColor.ANSI.WHITE)

    // Generate buffer title
    val bufferTitle = buffer match
      case Some(buf) =>
        buf.filePath match
          case Some(path) =>
            val filename = path.getFileName.toString
            if buf.isDirty then s"$filename - unsaved" else filename
          case None =>
            if buf.isDirty then s"Buffer ${buf.id.value} - unsaved" else s"Buffer ${buf.id.value}"
      case None =>
        "No Buffer"

    // Truncate title if too long, leaving space for padding
    val maxTitleWidth = math.max(1, rect.width - 2) // Leave space for padding
    val displayTitle =
      if bufferTitle.length > maxTitleWidth then bufferTitle.take(maxTitleWidth - 3) + "..."
      else bufferTitle

    // Clear the header line
    val headerLine = " " * rect.width
    graphics.putString(rect.x, rect.y, headerLine)

    // Center the title in the header
    val paddingLeft = (rect.width - displayTitle.length) / 2
    val centeredX   = rect.x + paddingLeft
    graphics.putString(centeredX, rect.y, displayTitle)

    // Reset colors
    graphics.setBackgroundColor(TextColor.ANSI.BLACK)
    graphics.setForegroundColor(TextColor.ANSI.WHITE)

  private def renderBufferContent(
    pane: EditorPane,
    buffer: Buffer,
    rect: LayoutRect,
    state: AppState,
    context: RenderContext
  ): Unit =
    val viewport   = buffer.viewport
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
          then
            CharacterRenderer.renderStringWithAnimation(
              context.graphics,
              screenX,
              screenY,
              visualLine.content,
              state.theme,
              buffer.animations,
              state.syntaxHighlightingEnabled,
              bufferLine = visualLine.bufferLine,
              bufferStartColumn = visualLine.startColumn
            )
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

  private def renderWelcomeText(rect: LayoutRect, context: RenderContext): Unit =
    // Render welcome message for new empty buffers
    val lines = List(
      "Welcome to Serenity!",
      "",
      "Start typing to edit text.",
      "",
      "Press Ctrl+P for command palette"
    )

    val startY = rect.y + (rect.height - lines.length) / 2

    context.graphics.setForegroundColor(TextColor.ANSI.BLACK_BRIGHT)

    lines.zipWithIndex.foreach {
      case (line, index) =>
        val lineY   = startY + index
        val centerX = rect.x + (rect.width - line.length) / 2

        if lineY >= 0 && lineY < context.screen.getTerminalSize.getRows && centerX >= 0 then
          CharacterRenderer.renderString(context.graphics, centerX, lineY, line)
    }

  private def renderCursors(
    buffer: Buffer,
    rect: LayoutRect,
    context: RenderContext
  ): Unit =
    val viewport   = buffer.viewport
    val panelWidth = rect.width

    buffer.cursors.foreach { cursor =>
      // Calculate visual line position for cursor
      calculateCursorVisualPosition(cursor, buffer.content, panelWidth, viewport) match
        case Some((visualLine, visualColumn)) =>
          val screenY = rect.y + (visualLine - viewport.topLine)
          val screenX = rect.x + visualColumn

          // Only render cursor if it's visible in the viewport AND within panel bounds
          if screenY >= rect.y && screenY < rect.bottom &&
              screenX >= rect.x && screenX < rect.right &&
              screenY >= 0 && screenY < context.screen.getTerminalSize.getRows &&
              screenX >= 0 && screenX < context.screen.getTerminalSize.getColumns
          then
            if context.cursorVisible then
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

  private def renderCommandRunner(state: AppState, context: RenderContext): Unit =
    if state.commandRunner.isActive then
      val terminalSize = TerminalSize(context.screen.getTerminalSize.getColumns, context.screen.getTerminalSize.getRows)
      val rect         = context.layout.editorPanelRect

      // Find cursor screen position from active pane
      val cursorScreenPosition = state.layout.activeEditorPaneId
        .flatMap(paneId => state.layout.editorPanes.get(paneId))
        .flatMap { pane =>
          pane.cursors.headOption.flatMap { cursor =>
            pane.bufferId.flatMap(state.buffers.get).map { buffer =>
              // Calculate screen position using the same logic as cursor rendering
              calculateCursorVisualPosition(cursor, buffer.content, rect.width, pane.viewport)
                .map {
                  case (visualLine, visualColumn) =>
                    val screenY = rect.y + (visualLine - pane.viewport.topLine)
                    val screenX = rect.x + visualColumn
                    CursorPosition(screenY, screenX)
                }
                .getOrElse(CursorPosition(rect.y, rect.x)) // Fallback to pane top-left
            }
          }
        }
        .getOrElse(CursorPosition(rect.y, rect.x)) // Fallback to pane top-left

      CommandRunnerRenderer.render(
        context.graphics,
        state.commandRunner,
        state.theme,
        terminalSize,
        cursorScreenPosition
      )
