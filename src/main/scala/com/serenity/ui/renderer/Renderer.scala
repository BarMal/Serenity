package com.serenity.ui.renderer

import com.googlecode.lanterna.graphics.TextGraphics
import com.googlecode.lanterna.screen.Screen
import com.serenity.state.models.*
import com.serenity.ui.layout.*
import com.serenity.ui.theme.Theme
import org.slf4j.LoggerFactory

case class RenderContext(
    screen: Screen,
    graphics: TextGraphics,
    layout: CalculatedLayout,
    cursorVisible: Boolean = true
)

object Renderer:
  private val logger = LoggerFactory.getLogger("com.serenity.ui.renderer.Renderer")

  def render(state: AppState, cursorVisible: Boolean, screen: Screen): Unit =
    val graphics     = screen.newTextGraphics()
    val terminalSize = TerminalSize(screen.getTerminalSize.getColumns, screen.getTerminalSize.getRows)
    val layout       = LayoutEngine.calculateLayout(state, terminalSize)

    screen.setCursorPosition(null)
    graphics.setBackgroundColor(state.theme.background)
    graphics.fillRectangle(com.googlecode.lanterna.TerminalPosition.TOP_LEFT_CORNER, screen.getTerminalSize, ' ')

    state.startPageSurface.flatMap {
      _.content match
        case SurfaceContent.StartPage(page) => Some(page)
        case _                              => None
    } match
      case Some(page) =>
        renderStartPage(page, graphics, terminalSize, state.theme)
        val floatContext = RenderContext(screen, graphics, layout, cursorVisible)
        renderFloatingPanels(state, floatContext)
      case None =>
        val context = RenderContext(screen, graphics, layout, cursorVisible)
        renderSpacerColumns(context)
        renderLineNumbers(state, context)
        renderGutter(state, context)
        renderPinnedPanels(state, context)
        renderEditorPanes(state, context)
        renderFloatingPanels(state, context)

    screen.refresh()

  def renderCursorOnly(state: AppState, cursorVisible: Boolean, screen: Screen): Unit =
    val graphics     = screen.newTextGraphics()
    val terminalSize = TerminalSize(screen.getTerminalSize.getColumns, screen.getTerminalSize.getRows)
    val layout       = LayoutEngine.calculateLayout(state, terminalSize)
    val paneLayouts  = LayoutEngine.calculatePaneLayouts(state, layout)

    screen.setCursorPosition(null)
    state.focus match
      case Focus.EditorPane(focusedPaneId) =>
        for
          paneId <- state.layout.activeEditorPaneId if paneId == focusedPaneId
          pane   <- state.layout.editorPanes.get(paneId)
          rect   <- paneLayouts.get(paneId)
          buffer <- pane.bufferId.flatMap(state.buffers.get)
          cursor <- buffer.cursors.headOption
        do
          val contentRect = LayoutRect(rect.x, rect.y + 1, rect.width, math.max(1, rect.height - 1))

          calculateCursorVisualPosition(cursor, buffer.content, contentRect.width, buffer.viewport) match
            case Some((visualLine, visualColumn)) =>
              val screenY = contentRect.y + (visualLine - buffer.viewport.topLine)
              val screenX = contentRect.x + visualColumn

              if screenY >= 0 && screenY < terminalSize.height &&
                  screenX >= 0 && screenX < terminalSize.width
              then
                if cursorVisible then
                  graphics.setBackgroundColor(state.theme.cursor)
                  graphics.setForegroundColor(state.theme.background)
                  CharacterRenderer.renderChar(graphics, screenX, screenY, ' ')
                else
                  val charBeneath =
                    buffer.content
                      .getLine(cursor.line)
                      .map(line => if cursor.column < line.length then line(cursor.column) else ' ')
                      .getOrElse(' ')
                  graphics.setBackgroundColor(state.theme.background)
                  graphics.setForegroundColor(state.theme.foreground)
                  CharacterRenderer.renderChar(graphics, screenX, screenY, charBeneath)
            case None => ()
      case _ => ()

    screen.refresh()

  private def renderSpacerColumns(context: RenderContext): Unit = ()

  private def renderEditorPanes(state: AppState, context: RenderContext): Unit =
    val paneLayouts = LayoutEngine.calculatePaneLayouts(state, context.layout)

    state.layout.editorPanes.foreach { (paneId, pane) =>
      paneLayouts.get(paneId) match
        case Some(paneRect) => renderEditorPane(pane, paneRect, state, context)
        case None           => ()
    }

  private def renderEditorPane(
    pane: EditorPane,
    rect: LayoutRect,
    state: AppState,
    context: RenderContext
  ): Unit =
    val buffer = pane.bufferId.flatMap(state.buffers.get)

    renderBufferHeader(pane, buffer, rect, state, context)

    val contentRect = LayoutRect(rect.x, rect.y + 1, rect.width, math.max(1, rect.height - 1))

    buffer match
      case Some(buf) if buf.content.weight == 0 && buf.isNewEmpty =>
        renderWelcomeText(contentRect, state.theme, context)
      case Some(buf) if buf.content.weight == 0 =>
        renderEmptyPane(contentRect, state.theme, context)
      case Some(buf) =>
        renderBufferContent(pane, buf, contentRect, state, context)
      case None =>
        renderEmptyPane(contentRect, state.theme, context)

    buffer.foreach(buf => renderCursors(buf, contentRect, state.theme, context))

  private def renderBufferHeader(
    pane: EditorPane,
    buffer: Option[Buffer],
    rect: LayoutRect,
    state: AppState,
    context: RenderContext
  ): Unit =
    val graphics = context.graphics
    val isActive = state.layout.activeEditorPaneId.contains(pane.id)

    if isActive then
      graphics.setBackgroundColor(state.theme.highlighted.background)
      graphics.setForegroundColor(state.theme.highlighted.foreground)
    else
      graphics.setBackgroundColor(state.theme.panel.background)
      graphics.setForegroundColor(state.theme.panel.foreground)

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

    val maxTitleWidth = math.max(1, rect.width - 2)
    val displayTitle =
      if bufferTitle.length > maxTitleWidth then bufferTitle.take(maxTitleWidth - 3) + "..."
      else bufferTitle

    graphics.putString(rect.x, rect.y, " " * rect.width)

    val paddingLeft = (rect.width - displayTitle.length) / 2
    val centeredX   = rect.x + paddingLeft
    graphics.putString(centeredX, rect.y, displayTitle)

    graphics.setBackgroundColor(state.theme.background)
    graphics.setForegroundColor(state.theme.foreground)

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
    val visualLines = calculateVisualLinesInViewport(rope, viewport, panelWidth)

    visualLines.zipWithIndex.foreach {
      case (visualLine, screenLineIndex) =>
        if screenLineIndex < rect.height then
          val screenY = rect.y + screenLineIndex
          val screenX = rect.x

          context.graphics.setForegroundColor(state.theme.foreground)

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

  private def renderEmptyPane(rect: LayoutRect, theme: Theme, context: RenderContext): Unit =
    val message = "~ Empty ~"
    val centerX = rect.x + (rect.width - message.length) / 2
    val centerY = rect.y + rect.height / 2

    context.graphics.setForegroundColor(theme.muted)
    if centerY < context.screen.getTerminalSize.getRows && centerX >= 0 then
      CharacterRenderer.renderString(context.graphics, centerX, centerY, message)

  private def renderWelcomeText(rect: LayoutRect, theme: Theme, context: RenderContext): Unit =
    val lines = List(
      "Welcome to Serenity!",
      "",
      "Start typing to edit text.",
      "",
      "Press Ctrl+P for command palette"
    )

    val startY = rect.y + (rect.height - lines.length) / 2

    context.graphics.setForegroundColor(theme.placeholder)

    lines.zipWithIndex.foreach {
      case (line, index) =>
        val lineY   = startY + index
        val centerX = rect.x + (rect.width - line.length) / 2

        if lineY >= 0 && lineY < context.screen.getTerminalSize.getRows && centerX >= 0 then
          CharacterRenderer.renderString(context.graphics, centerX, lineY, line)
    }

  private def renderStartPage(
    page: StartupPage,
    graphics: TextGraphics,
    terminalSize: TerminalSize,
    theme: Theme
  ): Unit =
    val lines  = page.renderLines
    val startY = (terminalSize.height - lines.size) / 2
    
    // Calculate which line indices correspond to options
    val titleLines = 2 // title + empty line
    val optionStartIndex = titleLines
    val optionEndIndex = titleLines + page.options.size - 1

    lines.zipWithIndex.foreach { case (line, lineIndex) =>
      val y = startY + lineIndex
      val x = math.max(0, (terminalSize.width - line.length) / 2)

      if y >= 0 && y < terminalSize.height then
        // Check if this line is a selectable option
        val isOption = lineIndex >= optionStartIndex && lineIndex <= optionEndIndex
        val optionIndex = lineIndex - optionStartIndex
        val isSelected = isOption && optionIndex == page.selectedIndex
        
        if isSelected then
          // Render highlighted background across full width
          graphics.setForegroundColor(theme.highlighted.foreground)
          graphics.setBackgroundColor(theme.highlighted.background)
          CharacterRenderer.renderStringPlain(graphics, 0, y, " " * terminalSize.width)
          // Center the text over the highlighted background
          CharacterRenderer.renderString(graphics, x, y, line)
        else
          // Render normal text
          graphics.setForegroundColor(theme.placeholder)
          graphics.setBackgroundColor(theme.background)
          CharacterRenderer.renderString(graphics, x, y, line)
    }

  private def renderCursors(
    buffer: Buffer,
    rect: LayoutRect,
    theme: Theme,
    context: RenderContext
  ): Unit =
    val viewport   = buffer.viewport
    val panelWidth = rect.width

    buffer.cursors.foreach { cursor =>
      calculateCursorVisualPosition(cursor, buffer.content, panelWidth, viewport) match
        case Some((visualLine, visualColumn)) =>
          val screenY = rect.y + (visualLine - viewport.topLine)
          val screenX = rect.x + visualColumn

          if screenY >= rect.y && screenY < rect.bottom &&
              screenX >= rect.x && screenX < rect.right &&
              screenY >= 0 && screenY < context.screen.getTerminalSize.getRows &&
              screenX >= 0 && screenX < context.screen.getTerminalSize.getColumns
          then if context.cursorVisible then
            context.graphics.setBackgroundColor(theme.cursor)
            context.graphics.setForegroundColor(theme.background)
            CharacterRenderer.renderChar(context.graphics, screenX, screenY, ' ')
            context.graphics.setBackgroundColor(theme.background)
            context.graphics.setForegroundColor(theme.foreground)
        case None => ()
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
        val visualLineInBuffer = cursor.column / panelWidth
        val visualColumnInLine = cursor.column % panelWidth
        val totalVisualLine    = currentVisualLine + visualLineInBuffer
        Some((totalVisualLine, visualColumnInLine))
      else
        val lineContent             = rope.getLine(bufferLine).getOrElse("")
        val visualLinesInThisBuffer = math.max(1, (lineContent.length + panelWidth - 1) / panelWidth)
        findCursorPosition(bufferLine + 1, currentVisualLine + visualLinesInThisBuffer)

    findCursorPosition(0, 0)

  private def renderFloatingPanels(state: AppState, context: RenderContext): Unit =
    val overlays = OverlayViewModel.fromState(state, context.layout)

    overlays.aboveCursor.foreach { overlay =>
      logger.info(s"[OVERLAY RENDERED] placement=AboveCursor rect=${overlay.rect} title=${overlay.title.getOrElse("")}")
      TextOverlayRenderer.render(context.graphics, overlay, state.theme, context.cursorVisible)
    }
    overlays.belowCursor.foreach { overlay =>
      logger.info(s"[OVERLAY RENDERED] placement=BelowCursor rect=${overlay.rect} title=${overlay.title.getOrElse("")}")
      TextOverlayRenderer.render(context.graphics, overlay, state.theme, context.cursorVisible)
    }

    context.layout.floatingPanelRect.foreach { rect =>
      renderFloatingPanelPlaceholder(rect, state.theme, context)
    }

  private def renderPinnedPanels(state: AppState, context: RenderContext): Unit =
    PinnedPanelViewModel
      .fromLayout(context.layout, state.uiSurfaces)
      .foreach(panel => PinnedPanelRenderer.render(context.graphics, panel, state.theme))

  private def renderFloatingPanelPlaceholder(rect: LayoutRect, theme: Theme, context: RenderContext): Unit =
    context.graphics.setBackgroundColor(theme.panel.background)
    context.graphics.setForegroundColor(theme.border)

    for y <- rect.y until rect.bottom; x <- rect.x until rect.right do
      if y < context.screen.getTerminalSize.getRows && x < context.screen.getTerminalSize.getColumns then
        CharacterRenderer.renderChar(context.graphics, x, y, '.')

  private def renderLineNumbers(state: AppState, context: RenderContext): Unit =
    if state.config.showLineNumbers then
      context.layout.lineNumberRect foreach { lineRect =>
        val graphics = context.graphics

        graphics.setBackgroundColor(state.theme.panel.background)
        graphics.setForegroundColor(state.theme.muted)

        graphics.fillRectangle(
          com.googlecode.lanterna.TerminalPosition(lineRect.x, lineRect.y),
          com.googlecode.lanterna.TerminalSize(lineRect.width, lineRect.height),
          ' '
        )

        state.layout.editorPanes.foreach { (_, pane) =>
          pane.bufferId.flatMap(state.buffers.get).foreach { buffer =>
            val viewport     = buffer.viewport
            val startLine    = viewport.topLine
            val visibleLines = math.min(viewport.visibleLines, lineRect.height)

            for i <- 0 until visibleLines do
              val bufferLineIndex   = startLine + i
              val displayLineNumber = bufferLineIndex + 1
              val screenY           = lineRect.y + i

              if bufferLineIndex < buffer.content.lineCount then
                val lineNumberText = displayLineNumber.toString.padTo(lineRect.width - 1, ' ') + " "
                graphics.putString(lineRect.x, screenY, lineNumberText)
          }
        }
      }

  private def renderGutter(state: AppState, context: RenderContext): Unit =
    if state.config.showGutter then
      context.layout.gutterRect foreach { gutterRect =>
        val graphics = context.graphics

        graphics.setBackgroundColor(state.theme.panel.background)
        graphics.setForegroundColor(state.theme.panel.foreground)

        graphics.fillRectangle(
          com.googlecode.lanterna.TerminalPosition(gutterRect.x, gutterRect.y),
          com.googlecode.lanterna.TerminalSize(gutterRect.width, gutterRect.height),
          ' '
        )

        val gutterContent = buildGutterContent(state)
        val displayContent =
          if gutterContent.length > gutterRect.width then gutterContent.take(gutterRect.width - 3) + "..."
          else gutterContent.padTo(gutterRect.width, ' ')

        graphics.putString(gutterRect.x, gutterRect.y, displayContent)
      }

  private def buildGutterContent(state: AppState): String =
    state.focus match
      case Focus.EditorPane(paneId) =>
        state.layout.editorPanes
          .get(paneId)
          .flatMap(_.bufferId)
          .flatMap(state.buffers.get) match
          case Some(buffer) =>
            val cursor   = buffer.cursors.headOption.getOrElse(CursorPosition(0, 0))
            val position = s"Line ${cursor.line + 1}, Col ${cursor.column + 1}"

            val filePath = buffer.filePath match
              case Some(path) => s" | ${path.getFileName}"
              case None       => " | Not saved to file yet"

            s" $position$filePath "
          case None => " No active buffer "
      case _ => " No active editor pane "
