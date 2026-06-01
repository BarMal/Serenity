package com.serenity.ui.renderer

import com.serenity.animation.ThemeInterpolator
import com.serenity.state.models.*
import com.serenity.ui.layout.*
import com.serenity.ui.theme.Theme
import org.slf4j.LoggerFactory

case class RenderContext(
    surface: RenderSurface,
    layout: CalculatedLayout,
    cursorVisible: Boolean = true,
    cursorColorOverride: Option[java.awt.Color] = None
)

object Renderer:
  private val logger = LoggerFactory.getLogger("com.serenity.ui.renderer.Renderer")

  private def withEffectiveTheme(state: AppState): AppState =
    state.themeTransition match
      case None => state
      case Some(t) =>
        state.copy(theme = ThemeInterpolator.blend(t.previousTheme, state.theme, t.progress))

  def render(
    state: AppState,
    cursorVisible: Boolean,
    swingWin: com.serenity.ui.terminal.SwingWindow,
    font: java.awt.Font
  ): Unit =
    render(state, cursorVisible, swingWin, font, None)

  def render(
    state: AppState,
    cursorVisible: Boolean,
    swingWin: com.serenity.ui.terminal.SwingWindow,
    font: java.awt.Font,
    cursorColor: Option[java.awt.Color]
  ): Unit =
    val state0       = withEffectiveTheme(state)
    val surface      = Java2DRenderSurface.forFrame(swingWin.metrics, font, swingWin.canvas, swingWin.onImageReady)
    val viewportSize = swingWin.viewportSize
    val layout       = LayoutEngine.calculateLayout(state0, viewportSize)
    renderFrame(state0, cursorVisible, surface, viewportSize, layout, cursorColor)

  def render(state: AppState, cursorVisible: Boolean, surface: RenderSurface, viewportSize: ViewportSize): Unit =
    render(state, cursorVisible, surface, viewportSize, None)

  def render(
    state: AppState,
    cursorVisible: Boolean,
    surface: RenderSurface,
    viewportSize: ViewportSize,
    cursorColor: Option[java.awt.Color]
  ): Unit =
    val state0 = withEffectiveTheme(state)
    val layout = LayoutEngine.calculateLayout(state0, viewportSize)
    renderFrame(state0, cursorVisible, surface, viewportSize, layout, cursorColor)

  private def renderFrame(
    state: AppState,
    cursorVisible: Boolean,
    surface: RenderSurface,
    viewportSize: ViewportSize,
    layout: CalculatedLayout,
    cursorColor: Option[java.awt.Color] = None
  ): Unit =
    surface.hideCursor()
    surface.setBackgroundColor(state.theme.background)
    surface.fillRect(0, 0, surface.viewportWidth, surface.viewportHeight, ' ')

    state.startPageSurface.flatMap {
      _.content match
        case SurfaceContent.StartPage(page) => Some(page)
        case _                              => None
    } match
      case Some(page) =>
        renderStartPage(page, surface, viewportSize, state.theme)
        val floatContext = RenderContext(surface, layout, cursorVisible, cursorColor)
        renderFloatingPanels(state, floatContext)
      case None =>
        val context = RenderContext(surface, layout, cursorVisible, cursorColor)
        renderSpacerColumns(context)
        renderLineNumbers(state, context)
        renderGutter(state, context)
        renderPinnedPanels(state, context)
        renderEditorPanes(state, context)
        renderFloatingPanels(state, context)

    surface.flush()

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
    val surface  = context.surface
    val isActive = state.layout.activeEditorPaneId.contains(pane.id)

    if isActive then
      surface.setBackgroundColor(state.theme.highlighted.background)
      surface.setForegroundColor(state.theme.highlighted.foreground)
    else
      surface.setBackgroundColor(state.theme.panel.background)
      surface.setForegroundColor(state.theme.panel.foreground)

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

    surface.putString(rect.x, rect.y, " " * rect.width)

    val paddingLeft = (rect.width - displayTitle.length) / 2
    val centeredX   = rect.x + paddingLeft
    surface.putString(centeredX, rect.y, displayTitle)

    surface.setBackgroundColor(state.theme.background)
    surface.setForegroundColor(state.theme.foreground)

  private def renderBufferContent(
    pane: EditorPane,
    buffer: Buffer,
    rect: LayoutRect,
    state: AppState,
    context: RenderContext
  ): Unit =
    val viewport    = buffer.viewport
    val rope        = buffer.content
    val panelWidth  = rect.width
    val visualLines = calculateVisualLinesInViewport(rope, viewport, panelWidth)

    visualLines.zipWithIndex.foreach {
      case (visualLine, screenLineIndex) =>
        if screenLineIndex < rect.height then
          val screenY = rect.y + screenLineIndex
          val screenX = rect.x

          context.surface.setForegroundColor(state.theme.foreground)

          if screenY < context.surface.viewportHeight &&
              screenY >= 0 &&
              screenX < context.surface.viewportWidth &&
              screenX >= 0 &&
              screenY < rect.bottom &&
              screenX < rect.right
          then
            CharacterRenderer.renderStringWithAnimation(
              context.surface,
              screenX,
              screenY,
              visualLine.content,
              state.theme,
              buffer.animations,
              state.syntaxHighlightingEnabled,
              bufferLine = visualLine.bufferLine,
              bufferStartColumn = visualLine.startColumn
            )

            renderSelectionHighlights(
              context.surface,
              buffer,
              visualLine,
              rect,
              screenY,
              state.theme
            )

            val stringEnd = visualLine.startColumn + visualLine.content.length
            val lineAnims = buffer.animations.getLineAnimations(visualLine.bufferLine)
            lineAnims
              .filter((col, cell) => col >= stringEnd && cell.currentBackground.isDefined)
              .foreach { (col, cell) =>
                val bgScreenX = rect.x + (col - visualLine.startColumn)
                if bgScreenX >= 0 && bgScreenX < rect.right then
                  context.surface.setForegroundColor(state.theme.foreground)
                  context.surface.setBackgroundColor(cell.currentBackground.get)
                  context.surface.putString(bgScreenX, screenY, " ")
              }
    }

  private case class VisualLine(content: String, bufferLine: Int, startColumn: Int, endColumn: Int)

  private def renderSelectionHighlights(
    surface: RenderSurface,
    buffer: Buffer,
    visualLine: VisualLine,
    rect: LayoutRect,
    screenY: Int,
    theme: Theme
  ): Unit =
    buffer.selection.foreach { selection =>
      selectionColumnsForLine(selection, visualLine).foreach { case (selectionStart, selectionEnd) =>
        (selectionStart until selectionEnd).foreach { bufferColumn =>
          val relativeColumn = bufferColumn - visualLine.startColumn
          val screenX        = rect.x + relativeColumn
          if screenX >= rect.x && screenX < rect.right then
            val charIndex = bufferColumn - visualLine.startColumn
            val charToRender =
              if charIndex >= 0 && charIndex < visualLine.content.length then visualLine.content.charAt(charIndex)
              else ' '
            surface.setForegroundColor(theme.highlighted.foreground)
            surface.setBackgroundColor(theme.highlighted.background)
            CharacterRenderer.renderChar(surface, screenX, screenY, charToRender)
        }
      }
    }

  private def selectionColumnsForLine(selection: Selection, visualLine: VisualLine): Option[(Int, Int)] =
    if visualLine.bufferLine < selection.start.line || visualLine.bufferLine > selection.end.line then None
    else
      val lineSelectionStart =
        if visualLine.bufferLine == selection.start.line then selection.start.column else visualLine.startColumn
      val lineSelectionEnd =
        if visualLine.bufferLine == selection.end.line then selection.end.column else visualLine.endColumn

      val overlapStart = math.max(lineSelectionStart, visualLine.startColumn)
      val overlapEnd   = math.min(lineSelectionEnd, visualLine.endColumn)

      Option.when(overlapStart < overlapEnd)((overlapStart, overlapEnd))

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

    context.surface.setForegroundColor(theme.muted)
    if centerY < context.surface.viewportHeight && centerX >= 0 then
      CharacterRenderer.renderString(context.surface, centerX, centerY, message)

  private def renderWelcomeText(rect: LayoutRect, theme: Theme, context: RenderContext): Unit =
    val lines = List(
      "Welcome to Serenity!",
      "",
      "Start typing to edit text.",
      "",
      "Press Ctrl+P for command palette"
    )

    val startY = rect.y + (rect.height - lines.length) / 2

    context.surface.setForegroundColor(theme.placeholder)

    lines.zipWithIndex.foreach {
      case (line, index) =>
        val lineY   = startY + index
        val centerX = rect.x + (rect.width - line.length) / 2

        if lineY >= 0 && lineY < context.surface.viewportHeight && centerX >= 0 then
          CharacterRenderer.renderString(context.surface, centerX, lineY, line)
    }

  private def renderStartPage(
    page: StartupPage,
    surface: RenderSurface,
    viewportSize: ViewportSize,
    theme: Theme
  ): Unit =
    val lines  = page.renderLines
    val startY = (viewportSize.height - lines.size) / 2

    val titleLines       = 2
    val optionStartIndex = titleLines
    val optionEndIndex   = titleLines + page.options.size - 1

    lines.zipWithIndex.foreach {
      case (line, lineIndex) =>
        val y = startY + lineIndex
        val x = math.max(0, (viewportSize.width - line.length) / 2)

        if y >= 0 && y < viewportSize.height then
          val isOption    = lineIndex >= optionStartIndex && lineIndex <= optionEndIndex
          val optionIndex = lineIndex - optionStartIndex
          val isSelected  = isOption && optionIndex == page.selectedIndex

          if isSelected then
            surface.setForegroundColor(theme.highlighted.foreground)
            surface.setBackgroundColor(theme.highlighted.background)
            CharacterRenderer.renderStringPlain(surface, 0, y, " " * viewportSize.width)
            CharacterRenderer.renderString(surface, x, y, line)
          else
            surface.setForegroundColor(theme.placeholder)
            surface.setBackgroundColor(theme.background)
            CharacterRenderer.renderString(surface, x, y, line)
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
              screenY >= 0 && screenY < context.surface.viewportHeight &&
              screenX >= 0 && screenX < context.surface.viewportWidth
          then
            if context.cursorVisible then
              val effectiveCursorColor = context.cursorColorOverride.getOrElse(theme.cursor)
              context.surface.setBackgroundColor(effectiveCursorColor)
              context.surface.setForegroundColor(theme.background)
              CharacterRenderer.renderChar(context.surface, screenX, screenY, ' ')
              context.surface.setBackgroundColor(theme.background)
              context.surface.setForegroundColor(theme.foreground)
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
      val blurRadius = SurfaceMaterials.effectiveBlurRadius(state.config)
      if blurRadius > 0f then
        context.surface.blurRegion(
          overlay.rect.x,
          overlay.rect.y,
          overlay.rect.width,
          overlay.rect.height,
          blurRadius
        )
      TextOverlayRenderer.render(context.surface, overlay, state.theme, state.config, context.cursorVisible)
    }
    val belowOverlays = if overlays.belowCursorStack.nonEmpty then overlays.belowCursorStack else overlays.belowCursor.toList
    belowOverlays.foreach { overlay =>
      logger.info(s"[OVERLAY RENDERED] placement=BelowCursor rect=${overlay.rect} title=${overlay.title.getOrElse("")}")
      val blurRadius = SurfaceMaterials.effectiveBlurRadius(state.config)
      if blurRadius > 0f then
        context.surface.blurRegion(
          overlay.rect.x,
          overlay.rect.y,
          overlay.rect.width,
          overlay.rect.height,
          blurRadius
        )
      TextOverlayRenderer.render(context.surface, overlay, state.theme, state.config, context.cursorVisible)
    }

    context.layout.floatingPanelRect.foreach(rect => renderFloatingPanelPlaceholder(rect, state.theme, context))

  private def renderPinnedPanels(state: AppState, context: RenderContext): Unit =
    PinnedPanelViewModel
      .fromLayout(context.layout, state.uiSurfaces)
      .foreach { panel =>
        val blurRadius = SurfaceMaterials.effectiveBlurRadius(state.config)
        if blurRadius > 0f then
          context.surface.blurRegion(
            panel.rect.x,
            panel.rect.y,
            panel.rect.width,
            panel.rect.height,
            blurRadius
          )
        PinnedPanelRenderer.render(context.surface, panel, state.theme, state.config)
      }

  private def renderFloatingPanelPlaceholder(rect: LayoutRect, theme: Theme, context: RenderContext): Unit =
    context.surface.setBackgroundColor(theme.panel.background)
    context.surface.setForegroundColor(theme.border)

    for y <- rect.y until rect.bottom; x <- rect.x until rect.right do
      if y < context.surface.viewportHeight && x < context.surface.viewportWidth then
        CharacterRenderer.renderChar(context.surface, x, y, '.')

  private def renderLineNumbers(state: AppState, context: RenderContext): Unit =
    if state.config.showLineNumbers then
      context.layout.lineNumberRect foreach { lineRect =>
        val surface = context.surface

        surface.setBackgroundColor(state.theme.panel.background)
        surface.setForegroundColor(state.theme.muted)

        surface.fillRect(lineRect.x, lineRect.y, lineRect.width, lineRect.height, ' ')

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
                surface.putString(lineRect.x, screenY, lineNumberText)
                renderDiagnosticIndicator(surface, lineRect, screenY, bufferLineIndex, buffer, state)
          }
        }
      }

  private def renderDiagnosticIndicator(
    surface: RenderSurface,
    lineRect: LayoutRect,
    screenY: Int,
    bufferLineIndex: Int,
    buffer: Buffer,
    state: AppState
  ): Unit =
    val uriOpt = buffer.filePath.map(_.toUri.toString)
    uriOpt.foreach { uri =>
      val lineDiags = state.diagnostics
        .getOrElse(uri, Nil)
        .filter(d => d.range.start.line == bufferLineIndex)
      if lineDiags.nonEmpty then
        val worstCode = lineDiags.flatMap(_.severity).map(_.code).minOption
        val color = worstCode match
          case Some(1) => state.theme.error.foreground
          case Some(2) => state.theme.warning.foreground
          case _       => state.theme.muted
        surface.setForegroundColor(color)
        surface.setBackgroundColor(state.theme.panel.background)
        surface.putString(lineRect.x + lineRect.width - 1, screenY, "!")
    }

  private def renderGutter(state: AppState, context: RenderContext): Unit =
    if state.config.showGutter then
      context.layout.gutterRect foreach { gutterRect =>
        val surface = context.surface

        surface.setBackgroundColor(state.theme.panel.background)
        surface.setForegroundColor(state.theme.panel.foreground)

        surface.fillRect(gutterRect.x, gutterRect.y, gutterRect.width, gutterRect.height, ' ')

        val gutterContent = buildGutterContent(state)
        val displayContent =
          if gutterContent.length > gutterRect.width then gutterContent.take(gutterRect.width - 3) + "..."
          else gutterContent.padTo(gutterRect.width, ' ')

        surface.putString(gutterRect.x, gutterRect.y, displayContent)
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
