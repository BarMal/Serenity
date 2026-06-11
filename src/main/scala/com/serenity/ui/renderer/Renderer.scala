package com.serenity.ui.renderer

import java.awt.Font

import com.serenity.animation.ThemeInterpolator
import com.serenity.config.MarkdownViewMode
import com.serenity.lsp.config.LanguageId
import com.serenity.markdown.{MarkdownBlockLens, MarkdownDocumentPreview}
import com.serenity.state.models.*
import com.serenity.ui.layout.*
import com.serenity.ui.theme.Theme

case class RenderContext(
    surface: RenderSurface,
    layout: CalculatedLayout,
    cursorVisible: Boolean = true,
    cursorColorOverride: Option[java.awt.Color] = None,
    codeFont: java.awt.Font,
    textFont: java.awt.Font,
    uiFont: java.awt.Font,
    cellMetrics: CellMetrics,
    uiMetrics: CellMetrics
):
  def fontForBuffer(buffer: Buffer): java.awt.Font =
    if buffer.usesTextFont then textFont else codeFont

object Renderer:

  private def withEffectiveTheme(state: AppState): AppState =
    state.themeTransition match
      case None => state
      case Some(t) =>
        state.copy(theme = ThemeInterpolator.blend(t.previousTheme, state.theme, t.progress))

  def render(
    state: AppState,
    cursorVisible: Boolean,
    swingWin: com.serenity.ui.terminal.SwingWindow,
    codeFont: java.awt.Font,
    textFont: java.awt.Font,
    uiFont: java.awt.Font,
    uiMetrics: CellMetrics,
    cursorColor: Option[java.awt.Color]
  ): Unit =
    val state0       = withEffectiveTheme(state)
    val surface      = Java2DRenderSurface.forFrame(swingWin.metrics, codeFont, swingWin.canvas, swingWin.onImageReady)
    val viewportSize = swingWin.viewportSize
    val layout       = LayoutEngine.calculateLayout(state0, viewportSize)
    renderFrame(
      state0,
      cursorVisible,
      surface,
      viewportSize,
      layout,
      codeFont,
      textFont,
      uiFont,
      swingWin.metrics,
      uiMetrics,
      cursorColor
    )

  def render(
    state: AppState,
    cursorVisible: Boolean,
    surface: RenderSurface,
    viewportSize: ViewportSize,
    codeFont: java.awt.Font,
    textFont: java.awt.Font,
    cellMetrics: CellMetrics,
    cursorColor: Option[java.awt.Color]
  ): Unit =
    val defaultUiFont = Font(Font.SANS_SERIF, Font.PLAIN, codeFont.getSize).deriveFont(codeFont.getSize2D)
    render(
      state,
      cursorVisible,
      surface,
      viewportSize,
      codeFont,
      textFont,
      defaultUiFont,
      cellMetrics,
      CellMetrics.fromFont(defaultUiFont),
      cursorColor
    )

  def render(
    state: AppState,
    cursorVisible: Boolean,
    surface: RenderSurface,
    viewportSize: ViewportSize,
    codeFont: java.awt.Font,
    textFont: java.awt.Font,
    uiFont: java.awt.Font,
    cellMetrics: CellMetrics,
    uiMetrics: CellMetrics,
    cursorColor: Option[java.awt.Color]
  ): Unit =
    val state0 = withEffectiveTheme(state)
    val layout = LayoutEngine.calculateLayout(state0, viewportSize)
    renderFrame(
      state0,
      cursorVisible,
      surface,
      viewportSize,
      layout,
      codeFont,
      textFont,
      uiFont,
      cellMetrics,
      uiMetrics,
      cursorColor
    )

  def render(
    state: AppState,
    cursorVisible: Boolean,
    surface: RenderSurface,
    viewportSize: ViewportSize,
    cursorColor: Option[java.awt.Color] = None
  ): Unit =
    val defaultFont = Font(Font.MONOSPACED, Font.PLAIN, 12)
    render(
      state,
      cursorVisible,
      surface,
      viewportSize,
      defaultFont,
      defaultFont,
      CellMetrics.fromFont(defaultFont),
      cursorColor
    )

  private def renderFrame(
    state: AppState,
    cursorVisible: Boolean,
    surface: RenderSurface,
    viewportSize: ViewportSize,
    layout: CalculatedLayout,
    codeFont: java.awt.Font,
    textFont: java.awt.Font,
    uiFont: java.awt.Font,
    cellMetrics: CellMetrics,
    uiMetrics: CellMetrics,
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
        renderStartPage(page, surface, viewportSize, state.theme, uiFont, cellMetrics, uiMetrics)
        val floatContext =
          RenderContext(surface, layout, cursorVisible, cursorColor, codeFont, textFont, uiFont, cellMetrics, uiMetrics)
        renderFloatingPanels(state, floatContext)
      case None =>
        val context =
          RenderContext(surface, layout, cursorVisible, cursorColor, codeFont, textFont, uiFont, cellMetrics, uiMetrics)
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

    // Compute snapshot once so renderBufferContent and renderCursors share the same layout.
    val bufferSnapshot = buffer.map { buf =>
      val bufferFont   = context.fontForBuffer(buf)
      val panelWidthPx = contentRect.width * context.cellMetrics.charWidth
      context.surface.setFont(bufferFont)
      TextLayoutSnapshot.fromBuffer(
        buf,
        panelWidthPx,
        bufferFont,
        context.surface.fontRenderContext.getOrElse(TextLayoutSnapshot.defaultFontRenderContext())
      )
    }

    buffer match
      case Some(buf) if buf.content.weight == 0 && buf.isNewEmpty =>
        renderWelcomeText(contentRect, state.theme, context)
      case Some(buf) if buf.content.weight == 0 =>
        renderEmptyPane(contentRect, state.theme, context)
      case Some(buf) =>
        renderBufferContent(pane, buf, contentRect, state, context, bufferSnapshot.get)
      case None =>
        renderEmptyPane(contentRect, state.theme, context)

    val cursorContext =
      if state.hasCommandRunnerDomain then context.copy(cursorVisible = true)
      else context
    buffer.foreach { buf =>
      if isInlineMarkdownLens(buf, state) then
        renderMarkdownLensCursors(buf, contentRect, state.theme, cursorContext, bufferSnapshot.get)
      else renderCursors(buf, contentRect, state.theme, cursorContext, bufferSnapshot.get)
    }

  private def renderBufferHeader(
    pane: EditorPane,
    buffer: Option[Buffer],
    rect: LayoutRect,
    state: AppState,
    context: RenderContext
  ): Unit =
    context.surface.setFont(context.uiFont)
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
    context: RenderContext,
    snapshot: TextLayoutSnapshot
  ): Unit =
    if isInlineMarkdownLens(buffer, state) then
      val markdownLines = markdownSourceLines(buffer)
      renderInlineMarkdownPreview(buffer, rect, state, context)
      renderMarkdownRawLenses(buffer, rect, state, context, snapshot, markdownLines)
    else renderPlainBufferContent(pane, buffer, rect, state, context, snapshot)

  private def renderPlainBufferContent(
    pane: EditorPane,
    buffer: Buffer,
    rect: LayoutRect,
    state: AppState,
    context: RenderContext,
    snapshot: TextLayoutSnapshot
  ): Unit =
    val visualLines = snapshot.visualLines
    val xOriginPx   = context.cellMetrics.toPixelX(rect.x).toFloat

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
            if snapshot.usesMeasuredLayout then
              CharacterRenderer.renderMeasuredLineWithAnimation(
                context.surface,
                xOriginPx,
                context.cellMetrics.toPixelY(screenY),
                snapshot.lineHeightPx,
                snapshot.ascentPx,
                visualLine,
                state.theme,
                buffer.animations,
                state.syntaxHighlightingEnabled,
                buffer.language
              )
            else
              CharacterRenderer.renderStringWithAnimation(
                context.surface,
                screenX,
                screenY,
                visualLine.text,
                state.theme,
                buffer.animations,
                state.syntaxHighlightingEnabled,
                buffer.language,
                bufferLine = visualLine.bufferLine,
                bufferStartColumn = visualLine.startColumn
              )

            renderSelectionHighlights(
              context.surface,
              buffer,
              visualLine,
              rect,
              screenY,
              state.theme,
              context,
              snapshot
            )

            val stringEnd = visualLine.startColumn + visualLine.text.length
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

  private def renderInlineMarkdownPreview(
    buffer: Buffer,
    rect: LayoutRect,
    state: AppState,
    context: RenderContext
  ): Unit =
    val widthPx  = math.max(1, rect.width * context.cellMetrics.charWidth)
    val heightPx = math.max(1, rect.height * context.cellMetrics.lineHeight)
    val baseUri  = buffer.filePath.flatMap(path => Option(path.toAbsolutePath.getParent).map(_.toUri))
    val title    = buffer.filePath.flatMap(path => Option(path.getFileName).map(_.toString)).getOrElse("Untitled")
    val image = MarkdownDocumentPreview.renderImage(
      source = buffer.content.collect(),
      title = title,
      widthPx = widthPx,
      heightPx = heightPx,
      theme = state.theme,
      font = context.textFont,
      baseUri = baseUri
    )
    context.surface.drawImage(image, rect.x, rect.y, rect.width, rect.height)

  private def renderSelectionHighlights(
    surface: RenderSurface,
    buffer: Buffer,
    visualLine: TextVisualLine,
    rect: LayoutRect,
    screenY: Int,
    theme: Theme,
    context: RenderContext,
    snapshot: TextLayoutSnapshot
  ): Unit =
    buffer.allSelections.foreach { selection =>
      selectionColumnsForLine(selection, visualLine).foreach {
        case (selectionStart, selectionEnd) =>
          if snapshot.usesMeasuredLayout then
            val localStart = selectionStart - visualLine.startColumn
            val localEnd   = selectionEnd - visualLine.startColumn
            if localStart >= 0 && localStart < localEnd && localEnd <= visualLine.text.length then
              val selectedText = visualLine.text.substring(localStart, localEnd)
              val lineOriginPx = context.cellMetrics.toPixelX(rect.x).toFloat
              val startXPx     = lineOriginPx + visualLine.xForColumn(selectionStart).getOrElse(visualLine.widthPx)
              val endXPx       = lineOriginPx + visualLine.xForColumn(selectionEnd).getOrElse(visualLine.widthPx)
              surface.setForegroundColor(theme.highlighted.foreground)
              surface.setBackgroundColor(theme.highlighted.background)
              surface.drawRunPx(
                startXPx,
                context.cellMetrics.toPixelY(screenY),
                endXPx - startXPx,
                snapshot.lineHeightPx,
                snapshot.ascentPx,
                selectedText
              )
          else
            (selectionStart until selectionEnd).foreach { bufferColumn =>
              val relativeColumn = bufferColumn - visualLine.startColumn
              val screenX        = rect.x + relativeColumn
              if screenX >= rect.x && screenX < rect.right then
                val charIndex = bufferColumn - visualLine.startColumn
                val charToRender =
                  if charIndex >= 0 && charIndex < visualLine.text.length then visualLine.text.charAt(charIndex)
                  else ' '
                surface.setForegroundColor(theme.highlighted.foreground)
                surface.setBackgroundColor(theme.highlighted.background)
                CharacterRenderer.renderChar(surface, screenX, screenY, charToRender)
            }
      }
    }

  private def isInlineMarkdownLens(buffer: Buffer, state: AppState): Boolean =
    buffer.language.contains(LanguageId.Markdown) && state.config.markdownViewMode == MarkdownViewMode.InlineLens

  private def markdownSourceLines(buffer: Buffer): Vector[String] =
    (0 until buffer.content.lineCount).toVector.map(line => buffer.content.getLine(line).getOrElse(""))

  private def activeMarkdownBlockLineSet(lines: Vector[String], cursors: List[CursorPosition]): Set[Int] =
    cursors
      .map(_.line)
      .flatMap(line => MarkdownBlockLens.activeBlockLineSet(lines, Some(line)))
      .toSet

  private def activeMarkdownBlockRanges(lines: Vector[String], cursors: List[CursorPosition]): List[Range.Inclusive] =
    cursors
      .map(_.line)
      .filter(line => line >= 0 && line < lines.length)
      .map(line => MarkdownBlockLens.currentBlock(lines, line))
      .distinctBy(range => range.start -> range.end)

  private def renderMarkdownRawLenses(
    buffer: Buffer,
    rect: LayoutRect,
    state: AppState,
    context: RenderContext,
    snapshot: TextLayoutSnapshot,
    lines: Vector[String]
  ): Unit =
    activeMarkdownBlockRanges(lines, buffer.cursors).foreach { blockRange =>
      val blockVisualLines = snapshot.visualLines.filter(line => blockRange.contains(line.bufferLine))
      if blockVisualLines.nonEmpty then
        val lensTop =
          markdownLensTop(blockVisualLines, cursorForBlock(buffer.cursors, blockRange), rect.height, snapshot, lines)
        val lensY = rect.y + lensTop
        context.surface.setBackgroundColor(state.theme.panel.background)
        context.surface.fillRect(rect.x, lensY, rect.width, blockVisualLines.length, ' ')
        context.surface.strokeRoundRect(
          context.cellMetrics.toPixelX(rect.x),
          context.cellMetrics.toPixelY(lensY),
          rect.width * context.cellMetrics.charWidth,
          blockVisualLines.length * context.cellMetrics.lineHeight,
          arcPx = 0,
          state.theme.border
        )
        blockVisualLines.zipWithIndex.foreach {
          case (visualLine, index) =>
            val screenY = lensY + index
            if screenY >= rect.y && screenY < rect.bottom && screenY >= 0 && screenY < context.surface.viewportHeight
            then
              context.surface.setForegroundColor(state.theme.foreground)
              context.surface.setBackgroundColor(state.theme.panel.background)
              if snapshot.usesMeasuredLayout then
                CharacterRenderer.renderMeasuredLineWithAnimation(
                  context.surface,
                  context.cellMetrics.toPixelX(rect.x).toFloat,
                  context.cellMetrics.toPixelY(screenY),
                  snapshot.lineHeightPx,
                  snapshot.ascentPx,
                  visualLine,
                  state.theme.copy(background = state.theme.panel.background),
                  buffer.animations,
                  syntaxHighlightingEnabled = false,
                  language = None
                )
              else
                CharacterRenderer.renderStringWithAnimation(
                  context.surface,
                  rect.x,
                  screenY,
                  visualLine.text,
                  state.theme.copy(background = state.theme.panel.background),
                  buffer.animations,
                  syntaxHighlightingEnabled = false,
                  language = None,
                  bufferLine = visualLine.bufferLine,
                  bufferStartColumn = visualLine.startColumn
                )
        }
    }

  private def renderMarkdownLensCursors(
    buffer: Buffer,
    rect: LayoutRect,
    theme: Theme,
    context: RenderContext,
    snapshot: TextLayoutSnapshot
  ): Unit =
    val lines = markdownSourceLines(buffer)
    activeMarkdownBlockRanges(lines, buffer.cursors).foreach { blockRange =>
      val blockVisualLines = snapshot.visualLines.filter(line => blockRange.contains(line.bufferLine))
      if blockVisualLines.nonEmpty then
        val lensTop =
          markdownLensTop(blockVisualLines, cursorForBlock(buffer.cursors, blockRange), rect.height, snapshot, lines)
        buffer.cursors.zipWithIndex.foreach { (cursor, cursorIndex) =>
          val isPrimaryCursor = cursorIndex == 0
          val shouldRenderCursor =
            context.cursorVisible || (buffer.cursors.size > 1 && !isPrimaryCursor)
          blockVisualLines.zipWithIndex.collectFirst {
            case (line, visualIndex)
                if line.bufferLine == cursor.line && cursor.column >= line.startColumn && cursor.column <= line.endColumn =>
              val xPx = line.xForColumn(cursor.column).getOrElse(line.widthPx)
              (visualIndex, xPx)
          } match
            case Some((visualLine, xPx)) if shouldRenderCursor =>
              val screenYCell = rect.y + lensTop + visualLine
              if screenYCell >= rect.y && screenYCell < rect.bottom &&
                  screenYCell >= 0 && screenYCell < context.surface.viewportHeight
              then
                val effectiveCursorColor = context.cursorColorOverride.getOrElse(theme.cursor)
                val caretWidthPx         = math.max(2, math.round(context.cellMetrics.charWidth * 0.12f))
                val screenXPx            = context.cellMetrics.toPixelX(rect.x) + math.round(xPx)
                val screenYPx            = context.cellMetrics.toPixelY(screenYCell)
                context.surface.fillPixelRect(
                  screenXPx,
                  screenYPx,
                  caretWidthPx,
                  if snapshot.usesMeasuredLayout then snapshot.lineHeightPx else context.cellMetrics.lineHeight,
                  effectiveCursorColor
                )
            case _ => ()
        }
    }

  private def markdownLensTop(
    blockVisualLines: Vector[TextVisualLine],
    primaryCursor: Option[CursorPosition],
    visibleHeight: Int,
    snapshot: TextLayoutSnapshot,
    markdownLines: Vector[String]
  ): Int =
    val lensHeight = blockVisualLines.length
    val cursorVisualRow = primaryCursor.flatMap { cursor =>
      MarkdownDocumentPreview
        .previewRowForSourceLine(markdownLines, cursor.line)
        .orElse(calculateCursorVisualPosition(cursor, snapshot).map(_._1))
    }
    val desiredTop =
      cursorVisualRow.map(row => row - lensHeight / 2).getOrElse(blockVisualLines.headOption.fold(0)(_.bufferLine))
    desiredTop.max(0).min(math.max(0, visibleHeight - lensHeight))

  private def cursorForBlock(cursors: List[CursorPosition], blockRange: Range.Inclusive): Option[CursorPosition] =
    cursors.find(cursor => blockRange.contains(cursor.line)).orElse(cursors.headOption)

  private def selectionColumnsForLine(selection: Selection, visualLine: TextVisualLine): Option[(Int, Int)] =
    if visualLine.bufferLine < selection.start.line || visualLine.bufferLine > selection.end.line then None
    else
      val lineSelectionStart =
        if visualLine.bufferLine == selection.start.line then selection.start.column else visualLine.startColumn
      val lineSelectionEnd =
        if visualLine.bufferLine == selection.end.line then selection.end.column else visualLine.endColumn

      val overlapStart = math.max(lineSelectionStart, visualLine.startColumn)
      val overlapEnd   = math.min(lineSelectionEnd, visualLine.endColumn)

      Option.when(overlapStart < overlapEnd)((overlapStart, overlapEnd))

  private def renderEmptyPane(rect: LayoutRect, theme: Theme, context: RenderContext): Unit =
    context.surface.setFont(context.textFont)
    context.surface.setForegroundColor(theme.muted)
    context.surface.setBackgroundColor(theme.background)
    renderAlignedTextLine(
      surface = context.surface,
      line = "~ Empty ~",
      rect = rect,
      y = rect.y + rect.height / 2,
      font = context.textFont,
      cellMetrics = context.cellMetrics,
      textMetrics = CellMetrics.fromFont(context.textFont)
    )

  private def renderWelcomeText(rect: LayoutRect, theme: Theme, context: RenderContext): Unit =
    val lines = List(
      "Welcome to Serenity!",
      "",
      "Start typing to edit text.",
      "",
      "Press Ctrl+P for command palette"
    )

    val startY = rect.y + (rect.height - lines.length) / 2

    context.surface.setFont(context.textFont)
    context.surface.setForegroundColor(theme.placeholder)
    context.surface.setBackgroundColor(theme.background)

    lines.zipWithIndex.foreach {
      case (line, index) =>
        renderAlignedTextLine(
          surface = context.surface,
          line = line,
          rect = rect,
          y = startY + index,
          font = context.textFont,
          cellMetrics = context.cellMetrics,
          textMetrics = CellMetrics.fromFont(context.textFont)
        )
    }

  private def renderAlignedTextLine(
    surface: RenderSurface,
    line: String,
    rect: LayoutRect,
    y: Int,
    font: java.awt.Font,
    cellMetrics: CellMetrics,
    textMetrics: CellMetrics
  ): Unit =
    if y >= 0 && y < surface.viewportHeight then
      surface.fontRenderContext match
        case Some(frc) =>
          val placement = TextAlignment.placeLine(
            text = line,
            area = TextAreaPx(
              xPx = cellMetrics.toPixelX(rect.x).toFloat,
              yPx = cellMetrics.toPixelY(y),
              widthPx = rect.width * cellMetrics.charWidth,
              heightPx = cellMetrics.lineHeight
            ),
            font = font,
            lineHeightPx = cellMetrics.lineHeight,
            ascentPx = textMetrics.ascent,
            horizontal = TextHorizontalAlignment.Center,
            vertical = TextVerticalAlignment.Top,
            fontRenderContext = frc
          )
          surface.drawRunPx(
            placement.xPx,
            placement.yPx,
            placement.widthPx,
            placement.lineHeightPx,
            placement.ascentPx,
            line
          )
        case None =>
          val centerX = rect.x + (rect.width - line.length) / 2
          if centerX >= 0 then CharacterRenderer.renderString(surface, centerX, y, line)

  private def renderStartPage(
    page: StartupPage,
    surface: RenderSurface,
    viewportSize: ViewportSize,
    theme: Theme,
    uiFont: java.awt.Font,
    cellMetrics: CellMetrics,
    uiMetrics: CellMetrics
  ): Unit =
    surface.setFont(uiFont)
    val lines  = page.renderLines
    val startY = (viewportSize.height - lines.size) / 2

    val titleLines       = 2
    val optionStartIndex = titleLines
    val optionEndIndex   = titleLines + page.options.size - 1

    lines.zipWithIndex.foreach {
      case (line, lineIndex) =>
        val y = startY + lineIndex

        if y >= 0 && y < viewportSize.height then
          val isOption    = lineIndex >= optionStartIndex && lineIndex <= optionEndIndex
          val optionIndex = lineIndex - optionStartIndex
          val isSelected  = isOption && optionIndex == page.selectedIndex

          if isSelected then
            surface.setForegroundColor(theme.highlighted.foreground)
            surface.setBackgroundColor(theme.highlighted.background)
            CharacterRenderer.renderStringPlain(surface, 0, y, " " * viewportSize.width)
            renderCenteredStartPageLine(surface, line, y, viewportSize, uiFont, cellMetrics, uiMetrics)
          else
            surface.setForegroundColor(theme.placeholder)
            surface.setBackgroundColor(theme.background)
            renderCenteredStartPageLine(surface, line, y, viewportSize, uiFont, cellMetrics, uiMetrics)
    }

  private def renderCenteredStartPageLine(
    surface: RenderSurface,
    line: String,
    y: Int,
    viewportSize: ViewportSize,
    uiFont: java.awt.Font,
    cellMetrics: CellMetrics,
    uiMetrics: CellMetrics
  ): Unit =
    surface.fontRenderContext match
      case Some(frc) =>
        val placement = TextAlignment.placeLine(
          text = line,
          area = TextAreaPx(
            xPx = 0.0f,
            yPx = cellMetrics.toPixelY(y),
            widthPx = viewportSize.width * cellMetrics.charWidth,
            heightPx = cellMetrics.lineHeight
          ),
          font = uiFont,
          lineHeightPx = cellMetrics.lineHeight,
          ascentPx = uiMetrics.ascent,
          horizontal = TextHorizontalAlignment.Center,
          vertical = TextVerticalAlignment.Top,
          fontRenderContext = frc
        )
        surface.drawRunPx(
          xPx = placement.xPx,
          yPx = placement.yPx,
          bgWidthPx = placement.widthPx,
          lineHeightPx = placement.lineHeightPx,
          ascentPx = placement.ascentPx,
          s = line
        )
      case None =>
        val x = math.max(0, (viewportSize.width - line.length) / 2)
        CharacterRenderer.renderString(surface, x, y, line)

  private def renderCursors(
    buffer: Buffer,
    rect: LayoutRect,
    theme: Theme,
    context: RenderContext,
    snapshot: TextLayoutSnapshot
  ): Unit =

    buffer.cursors.zipWithIndex.foreach { (cursor, cursorIndex) =>
      val isPrimaryCursor = cursorIndex == 0
      val shouldRenderCursor =
        context.cursorVisible || (buffer.cursors.size > 1 && !isPrimaryCursor)
      calculateCursorVisualPosition(cursor, snapshot) match
        case Some((visualLine, xPx)) if shouldRenderCursor =>
          val screenYCell = rect.y + visualLine
          if screenYCell >= rect.y && screenYCell < rect.bottom &&
              screenYCell >= 0 && screenYCell < context.surface.viewportHeight
          then
            val effectiveCursorColor = context.cursorColorOverride.getOrElse(theme.cursor)
            val caretWidthPx         = math.max(2, math.round(context.cellMetrics.charWidth * 0.12f))
            val screenXPx            = context.cellMetrics.toPixelX(rect.x) + math.round(xPx)
            val screenYPx            = context.cellMetrics.toPixelY(screenYCell)
            context.surface.fillPixelRect(
              screenXPx,
              screenYPx,
              caretWidthPx,
              if snapshot.usesMeasuredLayout then snapshot.lineHeightPx else context.cellMetrics.lineHeight,
              effectiveCursorColor
            )
        case _ => ()
    }

  private def calculateCursorVisualPosition(
    cursor: CursorPosition,
    snapshot: TextLayoutSnapshot
  ): Option[(Int, Float)] =
    snapshot.visualLines.zipWithIndex.collectFirst {
      case (line, visualIndex)
          if line.bufferLine == cursor.line && cursor.column >= line.startColumn && cursor.column <= line.endColumn =>
        val xPx = line.xForColumn(cursor.column).getOrElse(line.widthPx)
        (visualIndex, xPx)
    }

  private def renderFloatingPanels(state: AppState, context: RenderContext): Unit =
    context.surface.setFont(context.uiFont)
    val overlays = OverlayViewModel.fromState(state, context.layout)

    overlays.aboveCursor.foreach { overlay =>
      val blurRadius = SurfaceMaterials.effectiveBlurRadius(state.config)
      if blurRadius > 0f then
        context.surface.blurRegion(
          overlay.rect.x,
          overlay.rect.y,
          overlay.rect.width,
          overlay.rect.height,
          blurRadius
        )
      TextOverlayRenderer.render(
        context.surface,
        overlay,
        state.theme,
        state.config,
        context.cursorVisible,
        context.uiFont,
        context.uiMetrics
      )
    }
    val belowOverlays =
      if overlays.belowCursorStack.nonEmpty then overlays.belowCursorStack else overlays.belowCursor.toList
    belowOverlays.foreach { overlay =>
      val blurRadius = SurfaceMaterials.effectiveBlurRadius(state.config)
      if blurRadius > 0f then
        context.surface.blurRegion(
          overlay.rect.x,
          overlay.rect.y,
          overlay.rect.width,
          overlay.rect.height,
          blurRadius
        )
      TextOverlayRenderer.render(
        context.surface,
        overlay,
        state.theme,
        state.config,
        context.cursorVisible,
        context.uiFont,
        context.uiMetrics
      )
    }

    context.layout.floatingPanelRect.foreach(rect => renderFloatingPanelPlaceholder(rect, state.theme, context))

  private def renderPinnedPanels(state: AppState, context: RenderContext): Unit =
    context.surface.setFont(context.uiFont)
    state.uiSurfaces.foreach {
      case surface @ UiSurface(_, content, SurfacePresentation.Pinned(position, _), _) =>
        context.layout.pinnedPanelRects.get(position).foreach { rect =>
          val blurRadius = SurfaceMaterials.effectiveBlurRadius(state.config)
          if blurRadius > 0f then
            context.surface.blurRegion(
              rect.x,
              rect.y,
              rect.width,
              rect.height,
              blurRadius
            )
          content match
            case SurfaceContent.MarkdownPreview(bufferId, title) =>
              renderMarkdownPreviewPanel(bufferId, title, rect, state, context)
            case _ =>
              PinnedPanelRenderer.render(
                context.surface,
                PinnedPanelViewModel.resolve(surface, rect),
                state.theme,
                state.config
              )
        }
      case _ => ()
    }

  private def renderMarkdownPreviewPanel(
    bufferId: BufferId,
    title: String,
    rect: LayoutRect,
    state: AppState,
    context: RenderContext
  ): Unit =
    val shell = TextPanelView(rect, s"Preview: $title", Nil)
    PinnedPanelRenderer.render(context.surface, shell, state.theme, state.config)

    val contentWidthCells  = math.max(1, rect.width - 2)
    val contentHeightCells = math.max(1, rect.height - 2)
    val widthPx            = contentWidthCells * context.cellMetrics.charWidth
    val heightPx           = contentHeightCells * context.cellMetrics.lineHeight
    val buffer             = state.buffers.get(bufferId)
    val content            = buffer.map(_.content.collect()).getOrElse("")
    val baseUri =
      buffer.flatMap(_.filePath).flatMap(path => Option(path.toAbsolutePath.getParent).map(_.toUri))
    val image = MarkdownDocumentPreview.renderImage(
      source = content,
      title = title,
      widthPx = widthPx,
      heightPx = heightPx,
      theme = state.theme,
      font = context.textFont,
      baseUri = baseUri
    )
    context.surface.drawImage(image, rect.x + 1, rect.y + 1, contentWidthCells, contentHeightCells)

  private def renderFloatingPanelPlaceholder(rect: LayoutRect, theme: Theme, context: RenderContext): Unit =
    context.surface.setBackgroundColor(theme.panel.background)
    context.surface.setForegroundColor(theme.border)

    for y <- rect.y until rect.bottom; x <- rect.x until rect.right do
      if y < context.surface.viewportHeight && x < context.surface.viewportWidth then
        CharacterRenderer.renderChar(context.surface, x, y, '.')

  private def renderLineNumbers(state: AppState, context: RenderContext): Unit =
    if state.config.showLineNumbers then
      context.surface.setFont(context.uiFont)
      context.layout.lineNumberRect foreach { lineRect =>
        val surface = context.surface

        surface.setBackgroundColor(state.theme.panel.background)
        surface.setForegroundColor(state.theme.muted)

        surface.fillRect(lineRect.x, lineRect.y, lineRect.width, lineRect.height, ' ')
        state.layout.activeEditorPaneId
          .flatMap(state.layout.editorPanes.get)
          .flatMap(_.bufferId)
          .flatMap(state.buffers.get)
          .foreach { buffer =>
            val font         = context.fontForBuffer(buffer)
            val panelWidthPx = context.layout.editorPanelRect.width * context.cellMetrics.charWidth
            val snapshot = TextLayoutSnapshot.fromBuffer(
              buffer,
              panelWidthPx,
              font,
              surface.fontRenderContext.getOrElse(TextLayoutSnapshot.defaultFontRenderContext())
            )
            val firstVisualRows =
              snapshot.visualLines.zipWithIndex
                .groupMapReduce(_._1.bufferLine)(_._2)(math.min)

            snapshot.visualLines.zipWithIndex.foreach {
              case (visualLine, index) if index < lineRect.height =>
                val screenY = lineRect.y + index
                if firstVisualRows.get(visualLine.bufferLine).contains(index) then
                  val lineNumberText = (visualLine.bufferLine + 1).toString.padTo(lineRect.width - 1, ' ') + " "
                  surface.putString(lineRect.x, screenY, lineNumberText)
                  renderDiagnosticIndicator(surface, lineRect, screenY, visualLine.bufferLine, buffer, state)
              case _ => ()
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
      context.surface.setFont(context.uiFont)
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
            val language = buffer.language.fold("Plain Text")(_.displayName)

            val filePath = buffer.filePath match
              case Some(path) => s" | ${path.getFileName}"
              case None       => " | Not saved to file yet"

            s" $position | Language: $language$filePath "
          case None => " No active buffer "
      case _ => " No active editor pane "
