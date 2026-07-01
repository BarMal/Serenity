package com.serenity.ui.renderer

import java.awt.{Color, Font}

import com.serenity.animation.ThemeInterpolator
import com.serenity.config.{AppConfig, CursorInfoBarPlacement, MarkdownViewMode}
import com.serenity.lsp.config.LanguageId
import com.serenity.markdown.{MarkdownBlockLens, MarkdownDocumentPreview}
import com.serenity.spellcheck.SpellChecker
import com.serenity.state.models.*
import com.serenity.ui.layout.*
import com.serenity.ui.theme.{RichTextStyling, StyledText, Theme}

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

  def fontForRole(role: TypographyRole): java.awt.Font =
    role match
      case TypographyRole.Code            => codeFont
      case TypographyRole.Prose           => textFont
      case TypographyRole.MarkdownSource  => textFont
      case TypographyRole.MarkdownPreview => textFont
      case TypographyRole.Ui              => uiFont
      case TypographyRole.Mixed           => textFont

  def fontForBuffer(buffer: Buffer): java.awt.Font =
    fontForRole(buffer.typographyRole)

object Renderer:

  private case class EditorPaneRenderPlan(
      paneLayouts: Map[PaneId, EditorPaneLayout],
      snapshots: Map[PaneId, TextLayoutSnapshot]
  )

  private case class MarkdownLensFrame(
      lines: Vector[String],
      previewWindow: MarkdownDocumentPreview.PreviewWindow
  )

  private val MinMarkdownPreviewSourceLines = 32
  private val MarkdownPreviewOverscanFactor = 4

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
    cursorColor: Option[java.awt.Color]
  ): Unit =
    surface.hideCursor()
    surface.clearViewport(state.theme.background)

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
        val editorRenderPlan = prepareEditorPaneRenderPlan(state, context)
        renderSpacerColumns(context)
        renderLineNumbers(state, context, editorRenderPlan)
        renderGutter(state, context)
        renderPinnedPanels(state, context)
        renderEditorPanes(state, context, editorRenderPlan)
        renderFloatingPanels(state, context)

    surface.flush()

  private def renderSpacerColumns(context: RenderContext): Unit = ()

  private def prepareEditorPaneRenderPlan(state: AppState, context: RenderContext): EditorPaneRenderPlan =
    val paneLayouts = LayoutEngine.calculateEditorPaneLayouts(state, context.layout)
    val snapshots =
      state.layout.editorPanes.flatMap {
        case (paneId, pane) =>
          for
            paneLayout <- paneLayouts.get(paneId)
            bufferId   <- pane.bufferId
            buffer     <- state.buffers.get(bufferId)
          yield paneId -> snapshotForBuffer(buffer, paneLayout.contentRect, state, context)
      }

    EditorPaneRenderPlan(paneLayouts, snapshots)

  private def snapshotForBuffer(
    buffer: Buffer,
    contentRect: LayoutRect,
    state: AppState,
    context: RenderContext
  ): TextLayoutSnapshot =
    val bufferFont    = context.fontForBuffer(buffer)
    val panelWidthPx  = contentRect.width * context.cellMetrics.charWidth
    val panelHeightPx = contentRect.height * context.cellMetrics.lineHeight
    val bufferMetrics = CellMetrics.fromFont(bufferFont)
    val baseViewport  = LayoutEngine.updateBufferViewportDimensions(buffer, contentRect, state.config.wordWrapEnabled)
    val fontRenderContext =
      context.surface.fontRenderContext.getOrElse(TextLayoutSnapshot.defaultFontRenderContext())
    val visibleColumns =
      if bufferFont == context.codeFont then baseViewport.visibleColumns
      else visibleColumnsFor(bufferFont, fontRenderContext, panelWidthPx, baseViewport.visibleColumns)
    val visibleLines = math.max(1, panelHeightPx / math.max(1, bufferMetrics.lineHeight))
    val sizedViewport = baseViewport.copy(
      visibleColumns = visibleColumns,
      visibleLines = visibleLines,
      topVisualLine = baseViewport.topVisualLine.min(math.max(0, visibleLines - 1))
    )
    val scrollViewport = baseViewport.copy(
      visibleLines = visibleLines,
      topVisualLine = baseViewport.topVisualLine.min(math.max(0, visibleLines - 1))
    )
    val leftColumn =
      if visibleColumns == baseViewport.visibleColumns then baseViewport.leftColumn
      else renderedLeftColumn(buffer, scrollViewport, state.config.wordWrapEnabled)
    val renderedViewport = sizedViewport.copy(
      leftColumn = leftColumn
    )
    val renderBuffer = buffer.copy(
      viewport = renderedViewport
    )
    context.surface.setFont(bufferFont)
    TextLayoutSnapshot.fromBuffer(
      renderBuffer,
      panelWidthPx,
      bufferFont,
      fontRenderContext,
      wordWrapEnabled = state.config.wordWrapEnabled
    )

  private def visibleColumnsFor(
    font: Font,
    fontRenderContext: java.awt.font.FontRenderContext,
    panelWidthPx: Int,
    gridVisibleColumns: Int
  ): Int =
    val sample          = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"
    val measuredAdvance = TextLayoutSnapshot.caretXsForText(sample, font, fontRenderContext).lastOption.getOrElse(0.0f)
    val averageAdvance  = math.max(1.0f, measuredAdvance / sample.length.toFloat)
    val measuredColumns = math.ceil(panelWidthPx.toDouble / averageAdvance.toDouble).toInt + 32
    val gridOverscan    = gridVisibleColumns + 64

    math.max(gridOverscan, measuredColumns).max(1)

  private def renderedLeftColumn(buffer: Buffer, viewport: Viewport, wordWrapEnabled: Boolean): Int =
    if wordWrapEnabled then 0
    else
      val visibleColumns = math.max(1, viewport.visibleColumns)
      val cursor         = buffer.cursors.headOption.getOrElse(CursorPosition(viewport.topLine, 0))
      val cursorColumn   = cursor.column.max(0)
      val lineLength     = buffer.content.getLine(cursor.line).map(_.length).getOrElse(cursorColumn)
      val maxForCursor   = math.max(0, cursorColumn - visibleColumns + 1)
      val maxForLine     = math.max(0, lineLength - visibleColumns)

      viewport.leftColumn.max(0).min(maxForCursor).min(maxForLine)

  private def renderEditorPanes(state: AppState, context: RenderContext, renderPlan: EditorPaneRenderPlan): Unit =
    val activePaneId = state.layout.activeEditorPaneId
    val orderedPanes =
      state.layout.orderedPaneIds
        .flatMap(paneId => state.layout.editorPanes.get(paneId).map(paneId -> _))
        .sortBy((paneId, _) => if activePaneId.contains(paneId) then 1 else 0)

    orderedPanes.foreach { (paneId, pane) =>
      renderPlan.paneLayouts.get(paneId) match
        case Some(paneLayout) => renderEditorPane(pane, paneLayout, state, context, renderPlan.snapshots.get(paneId))
        case None             => ()
    }

  private def renderEditorPane(
    pane: EditorPane,
    paneLayout: EditorPaneLayout,
    state: AppState,
    context: RenderContext,
    preparedSnapshot: Option[TextLayoutSnapshot]
  ): Unit =
    val buffer = pane.bufferId.flatMap(state.buffers.get)

    renderBufferHeader(pane, buffer, paneLayout, state, context)

    val contentRect = paneLayout.contentRect

    val bufferSnapshot = preparedSnapshot.orElse(buffer.map(snapshotForBuffer(_, contentRect, state, context)))
    val markdownLensFrame =
      buffer.collect {
        case buf if isInlineMarkdownLens(buf, state) => markdownLensFrameFor(buf)
      }

    buffer match
      case Some(buf) if buf.content.weight == 0 && buf.isNewEmpty =>
        renderWelcomeText(contentRect, state.theme, context)
      case Some(buf) if buf.content.weight == 0 =>
        renderEmptyPane(contentRect, state.theme, context)
      case Some(buf) =>
        renderBufferContent(buf, contentRect, state, context, bufferSnapshot.get, markdownLensFrame)
      case None =>
        renderEmptyPane(contentRect, state.theme, context)

    val cursorContext =
      if state.hasCommandRunnerDomain then context.copy(cursorVisible = true)
      else context
    buffer.foreach { buf =>
      if isInlineMarkdownLens(buf, state) then
        renderMarkdownLensCursors(
          buf,
          contentRect,
          state.theme,
          state.config,
          cursorContext,
          bufferSnapshot.get,
          markdownLensFrame.getOrElse(markdownLensFrameFor(buf))
        )
      else renderCursors(buf, contentRect, state.theme, state.config, cursorContext, bufferSnapshot.get)
    }

  private def renderBufferHeader(
    pane: EditorPane,
    buffer: Option[Buffer],
    paneLayout: EditorPaneLayout,
    state: AppState,
    context: RenderContext
  ): Unit =
    context.surface.setFont(context.uiFont)
    val surface    = context.surface
    val isActive   = state.layout.activeEditorPaneId.contains(pane.id)
    val headerRect = paneLayout.headerRect
    val titleRect  = paneLayout.titleRect

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

    val maxTitleWidth = math.max(1, titleRect.width - 2)
    val displayTitle =
      if bufferTitle.length > maxTitleWidth then bufferTitle.take(maxTitleWidth - 3) + "..."
      else bufferTitle

    surface.putString(headerRect.x, headerRect.y, " " * headerRect.width)

    val titlePlacement = TextAlignment.placeLine(
      displayTitle,
      TextAreaPx(
        xPx = context.cellMetrics.toPixelX(titleRect.x).toFloat,
        yPx = context.cellMetrics.toPixelY(titleRect.y),
        widthPx = titleRect.width * context.cellMetrics.charWidth.toFloat,
        heightPx = context.cellMetrics.lineHeight
      ),
      context.uiFont,
      context.cellMetrics.lineHeight,
      context.cellMetrics.ascent,
      TextHorizontalAlignment.Center,
      TextVerticalAlignment.Top,
      surface.fontRenderContext.getOrElse(TextLayoutSnapshot.defaultFontRenderContext())
    )
    surface.drawRunPx(
      titlePlacement.xPx,
      titlePlacement.yPx,
      titlePlacement.widthPx,
      titlePlacement.lineHeightPx,
      titlePlacement.ascentPx,
      displayTitle
    )

    surface.setBackgroundColor(state.theme.background)
    surface.setForegroundColor(state.theme.foreground)

  private def renderBufferContent(
    buffer: Buffer,
    rect: LayoutRect,
    state: AppState,
    context: RenderContext,
    snapshot: TextLayoutSnapshot,
    markdownLensFrame: Option[MarkdownLensFrame]
  ): Unit =
    context.surface.setFont(context.fontForBuffer(buffer))
    if isInlineMarkdownLens(buffer, state) then
      val frame = markdownLensFrame.getOrElse(markdownLensFrameFor(buffer))
      renderInlineMarkdownPreview(buffer, rect, state, context, frame.previewWindow)
      renderMarkdownRawLenses(buffer, rect, state, context, snapshot, frame)
    else renderPlainBufferContent(buffer, rect, state, context, snapshot)

  private def renderPlainBufferContent(
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
        if visualLineFits(rect, screenLineIndex, context, snapshot) then
          val screenY   = rect.y + screenLineIndex
          val lineTopPx = visualLineTopPx(rect, screenLineIndex, context, snapshot)
          val screenX   = rect.x + visualLineCellOffset(visualLine, context)

          context.surface.setForegroundColor(state.theme.foreground)

          if visualLineVisible(rect, screenLineIndex, context, snapshot) &&
              screenY >= 0 &&
              screenX < context.surface.viewportWidth &&
              screenX >= 0 &&
              screenX < rect.right
          then
            val lineTheme      = state.theme
            val styledSegments = richTextStyledSegments(visualLine, lineTheme, snapshot)
            if snapshot.usesMeasuredLayout then
              CharacterRenderer.renderMeasuredLineWithAnimation(
                context.surface,
                xOriginPx,
                lineTopPx,
                snapshot.lineHeightPx,
                snapshot.ascentPx,
                visualLine,
                lineTheme,
                buffer.animations,
                state.syntaxHighlightingEnabled,
                buffer.language,
                styledSegments
              )
            else
              CharacterRenderer.renderStringWithAnimation(
                context.surface,
                screenX,
                screenY,
                visualLine.text,
                lineTheme,
                buffer.animations,
                state.syntaxHighlightingEnabled,
                buffer.language,
                bufferLine = visualLine.bufferLine,
                bufferStartColumn = visualLine.startColumn,
                styledSegments = styledSegments
              )

            renderDocumentCommentHighlights(
              context.surface,
              buffer,
              visualLine,
              rect,
              screenY,
              lineTopPx,
              state.theme,
              context,
              snapshot
            )

            renderSelectionHighlights(
              context.surface,
              buffer,
              visualLine,
              rect,
              screenY,
              lineTopPx,
              state.theme,
              context,
              snapshot
            )

            val stringEnd = visualLine.startColumn + visualLine.text.length
            val lineAnims = buffer.animations.getLineAnimations(visualLine.bufferLine)
            lineAnims
              .filter((col, cell) => col >= stringEnd && cell.currentBackground.isDefined)
              .foreach { (col, cell) =>
                val bgScreenX = rect.x + visualLineCellOffset(visualLine, context) + (col - visualLine.startColumn)
                if bgScreenX >= 0 && bgScreenX < rect.right then
                  context.surface.setForegroundColor(state.theme.foreground)
                  context.surface.setBackgroundColor(cell.currentBackground.get)
                  context.surface.putString(bgScreenX, screenY, " ")
              }
    }

  private def visualLineFits(
    rect: LayoutRect,
    screenLineIndex: Int,
    context: RenderContext,
    snapshot: TextLayoutSnapshot
  ): Boolean =
    if snapshot.usesMeasuredLayout then
      visualLineTopPx(rect, screenLineIndex, context, snapshot) < contentBottomPx(rect, context)
    else screenLineIndex < rect.height

  private def visualLineVisible(
    rect: LayoutRect,
    screenLineIndex: Int,
    context: RenderContext,
    snapshot: TextLayoutSnapshot
  ): Boolean =
    if snapshot.usesMeasuredLayout then
      val lineTopPx = visualLineTopPx(rect, screenLineIndex, context, snapshot)
      lineTopPx >= contentTopPx(rect, context) &&
      lineTopPx < contentBottomPx(rect, context) &&
      lineTopPx < surfaceBottomPx(context)
    else
      val screenY = rect.y + screenLineIndex
      screenY < context.surface.viewportHeight && screenY >= 0 && screenY < rect.bottom

  private def visualLineTopPx(
    rect: LayoutRect,
    screenLineIndex: Int,
    context: RenderContext,
    snapshot: TextLayoutSnapshot
  ): Int =
    if snapshot.usesMeasuredLayout then contentTopPx(rect, context) + screenLineIndex * snapshot.lineHeightPx
    else context.cellMetrics.toPixelY(rect.y + screenLineIndex)

  private def contentTopPx(rect: LayoutRect, context: RenderContext): Int =
    context.cellMetrics.toPixelY(rect.y)

  private def contentBottomPx(rect: LayoutRect, context: RenderContext): Int =
    context.cellMetrics.toPixelY(rect.bottom)

  private def surfaceBottomPx(context: RenderContext): Int =
    context.cellMetrics.toPixelY(context.surface.viewportHeight)

  private def visualLineCellOffset(visualLine: TextVisualLine, context: RenderContext): Int =
    if visualLine.xOffsetPx <= 0.0f then 0
    else math.round(visualLine.xOffsetPx / context.cellMetrics.charWidth.toFloat).max(0)

  private def richTextStyledSegments(
    visualLine: TextVisualLine,
    theme: Theme,
    snapshot: TextLayoutSnapshot
  ): Option[List[StyledText]] =
    snapshot.richTextDocument
      .map { document =>
        RichTextStyling.styledLine(
          document,
          visualLine.bufferLine,
          visualLine.startColumn,
          visualLine.endColumn,
          theme
        )
      }
      .filter(segments => segments.map(_.content).mkString == visualLine.text)

  private def renderInlineMarkdownPreview(
    buffer: Buffer,
    rect: LayoutRect,
    state: AppState,
    context: RenderContext,
    previewWindow: MarkdownDocumentPreview.PreviewWindow
  ): Unit =
    val widthPx  = math.max(1, rect.width * context.cellMetrics.charWidth)
    val heightPx = math.max(1, rect.height * context.cellMetrics.lineHeight)
    val baseUri  = buffer.filePath.flatMap(path => Option(path.toAbsolutePath.getParent).map(_.toUri))
    val title    = buffer.filePath.flatMap(path => Option(path.getFileName).map(_.toString)).getOrElse("Untitled")
    val image = MarkdownDocumentPreview.renderImage(
      source = previewWindow.source,
      title = title,
      widthPx = widthPx,
      heightPx = heightPx,
      theme = state.theme,
      font = context.textFont,
      baseUri = baseUri,
      panelChrome = false
    )
    context.surface.drawImage(image, rect.x, rect.y, rect.width, rect.height)

  private def renderSelectionHighlights(
    surface: RenderSurface,
    buffer: Buffer,
    visualLine: TextVisualLine,
    rect: LayoutRect,
    screenY: Int,
    lineTopPx: Int,
    theme: Theme,
    context: RenderContext,
    snapshot: TextLayoutSnapshot
  ): Unit =
    buffer.allSelections.foreach { selection =>
      columnsForRange(selection.start, selection.end, visualLine, markPoint = false).foreach {
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
                lineTopPx,
                endXPx - startXPx,
                snapshot.lineHeightPx,
                snapshot.ascentPx,
                selectedText
              )
          else
            (selectionStart until selectionEnd).foreach { bufferColumn =>
              val relativeColumn = bufferColumn - visualLine.startColumn
              val screenX        = rect.x + visualLineCellOffset(visualLine, context) + relativeColumn
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

  private def renderDocumentCommentHighlights(
    surface: RenderSurface,
    buffer: Buffer,
    visualLine: TextVisualLine,
    rect: LayoutRect,
    screenY: Int,
    lineTopPx: Int,
    theme: Theme,
    context: RenderContext,
    snapshot: TextLayoutSnapshot
  ): Unit =
    buffer.documentComments.foreach { comment =>
      columnsForRange(comment.start, comment.end, visualLine, markPoint = true).foreach {
        case (commentStart, commentEnd) =>
          val foreground = theme.foreground
          renderTextRangeBackground(
            surface,
            visualLine,
            rect,
            screenY,
            lineTopPx,
            foreground,
            commentHighlightBackground(theme),
            context,
            snapshot,
            commentStart,
            commentEnd
          )
      }
    }

  private[serenity] def commentHighlightBackground(theme: Theme): Color =
    blend(theme.warning.background, theme.background, warningWeight = 0.45)

  private def blend(foreground: Color, background: Color, warningWeight: Double): Color =
    val clampedWeight    = math.max(0.0, math.min(1.0, warningWeight))
    val backgroundWeight = 1.0 - clampedWeight
    def blendChannel(channel: Color => Int): Int =
      math.round(channel(foreground) * clampedWeight + channel(background) * backgroundWeight).toInt

    Color(blendChannel(_.getRed), blendChannel(_.getGreen), blendChannel(_.getBlue))

  private def renderTextRangeBackground(
    surface: RenderSurface,
    visualLine: TextVisualLine,
    rect: LayoutRect,
    screenY: Int,
    lineTopPx: Int,
    foreground: java.awt.Color,
    background: java.awt.Color,
    context: RenderContext,
    snapshot: TextLayoutSnapshot,
    rangeStart: Int,
    rangeEnd: Int
  ): Unit =
    if snapshot.usesMeasuredLayout then
      val localStart = rangeStart - visualLine.startColumn
      val localEnd   = rangeEnd - visualLine.startColumn
      if localStart >= 0 && localStart < localEnd then
        val rangeText =
          if localStart < visualLine.text.length then
            visualLine.text.substring(localStart, math.min(localEnd, visualLine.text.length))
          else " "
        val lineOriginPx = context.cellMetrics.toPixelX(rect.x).toFloat
        val startXPx     = lineOriginPx + visualLine.xForColumn(rangeStart).getOrElse(visualLine.widthPx)
        val endXPx =
          if rangeStart == rangeEnd - 1 && rangeStart >= visualLine.endColumn then
            startXPx + context.cellMetrics.charWidth
          else lineOriginPx + visualLine.xForColumn(rangeEnd).getOrElse(visualLine.widthPx)
        surface.setForegroundColor(foreground)
        surface.setBackgroundColor(background)
        surface.drawRunPx(
          startXPx,
          lineTopPx,
          math.max(context.cellMetrics.charWidth.toFloat, endXPx - startXPx),
          snapshot.lineHeightPx,
          snapshot.ascentPx,
          rangeText
        )
    else
      (rangeStart until rangeEnd).foreach { bufferColumn =>
        val relativeColumn = bufferColumn - visualLine.startColumn
        val screenX        = rect.x + visualLineCellOffset(visualLine, context) + relativeColumn
        if screenX >= rect.x && screenX < rect.right then
          val charIndex = bufferColumn - visualLine.startColumn
          val charToRender =
            if charIndex >= 0 && charIndex < visualLine.text.length then visualLine.text.charAt(charIndex)
            else ' '
          surface.setForegroundColor(foreground)
          surface.setBackgroundColor(background)
          CharacterRenderer.renderChar(surface, screenX, screenY, charToRender)
      }

  private def columnsForRange(
    start: CursorPosition,
    end: CursorPosition,
    visualLine: TextVisualLine,
    markPoint: Boolean
  ): Option[(Int, Int)] =
    if visualLine.bufferLine < start.line || visualLine.bufferLine > end.line then None
    else if markPoint && start == end && visualLine.bufferLine == start.line then
      Option.when(start.column >= visualLine.startColumn && start.column <= visualLine.endColumn)(
        start.column -> (start.column + 1)
      )
    else
      val rangeStart =
        if visualLine.bufferLine == start.line then start.column else visualLine.startColumn
      val rangeEnd =
        if visualLine.bufferLine == end.line then end.column else visualLine.endColumn
      val clippedStart = math.max(rangeStart, visualLine.startColumn)
      val clippedEnd   = math.min(rangeEnd, visualLine.endColumn)
      Option.when(clippedStart < clippedEnd)(clippedStart -> clippedEnd)

  private def isInlineMarkdownLens(buffer: Buffer, state: AppState): Boolean =
    buffer.language.contains(LanguageId.Markdown) && state.config.markdownViewMode == MarkdownViewMode.InlineLens

  private def markdownSourceLines(buffer: Buffer): Vector[String] =
    buffer.content.linesFrom(0, buffer.content.lineCount)

  private def markdownLensFrameFor(buffer: Buffer): MarkdownLensFrame =
    val lines = markdownSourceLines(buffer)
    MarkdownLensFrame(lines, markdownPreviewWindow(buffer, lines, buffer.viewport.visibleLines))

  private def markdownPreviewWindow(
    buffer: Buffer,
    lines: Vector[String],
    visibleRows: Int
  ): MarkdownDocumentPreview.PreviewWindow =
    MarkdownDocumentPreview.previewWindow(
      lines,
      activeLine = buffer.cursors.headOption.map(_.line),
      fallbackTopLine = buffer.viewport.topLine,
      maxSourceLines = markdownPreviewSourceLineLimit(visibleRows)
    )

  private def markdownPreviewSourceLineLimit(visibleRows: Int): Int =
    math.max(MinMarkdownPreviewSourceLines, visibleRows.max(1) * MarkdownPreviewOverscanFactor)

  private def activeMarkdownBlockRanges(lines: Vector[String], cursors: List[CursorPosition]): List[Range.Inclusive] =
    cursors
      .map(_.line)
      .filter(line => line >= 0 && line < lines.length)
      .map(line => MarkdownBlockLens.currentBlock(lines, line))
      .distinctBy(range => range.start -> range.end)

  private case class MarkdownLensPlacement(top: Int, height: Int)

  private def renderMarkdownRawLenses(
    buffer: Buffer,
    rect: LayoutRect,
    state: AppState,
    context: RenderContext,
    snapshot: TextLayoutSnapshot,
    frame: MarkdownLensFrame
  ): Unit =
    val lines         = frame.lines
    val previewWindow = frame.previewWindow
    activeMarkdownBlockRanges(lines, buffer.cursors).foreach { blockRange =>
      val blockVisualLines = snapshot.visualLines.filter(line => blockRange.contains(line.bufferLine))
      if blockVisualLines.nonEmpty then
        val placement =
          markdownLensPlacement(
            blockRange,
            blockVisualLines,
            cursorForBlock(buffer.cursors, blockRange),
            rect.height,
            snapshot,
            lines,
            previewWindow
          )
        val lensY = rect.y + placement.top
        context.surface.setBackgroundColor(state.theme.panel.background)
        context.surface.fillRect(rect.x, lensY, rect.width, placement.height, ' ')
        context.surface.strokeRoundRect(
          context.cellMetrics.toPixelX(rect.x),
          context.cellMetrics.toPixelY(lensY),
          rect.width * context.cellMetrics.charWidth,
          placement.height * context.cellMetrics.lineHeight,
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
    config: AppConfig,
    context: RenderContext,
    snapshot: TextLayoutSnapshot,
    frame: MarkdownLensFrame
  ): Unit =
    val lines         = frame.lines
    val previewWindow = frame.previewWindow
    activeMarkdownBlockRanges(lines, buffer.cursors).foreach { blockRange =>
      val blockVisualLines = snapshot.visualLines.filter(line => blockRange.contains(line.bufferLine))
      if blockVisualLines.nonEmpty then
        val placement =
          markdownLensPlacement(
            blockRange,
            blockVisualLines,
            cursorForBlock(buffer.cursors, blockRange),
            rect.height,
            snapshot,
            lines,
            previewWindow
          )
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
              val screenYCell = rect.y + placement.top + visualLine
              if screenYCell >= rect.y && screenYCell < rect.bottom &&
                  screenYCell >= 0 && screenYCell < context.surface.viewportHeight
              then
                val effectiveCursorColor = cursorColorFor(config, theme, context, isPrimaryCursor)
                val caretWidthPx         = math.max(2, math.round(context.cellMetrics.charWidth * 0.12f))
                val screenXPx            = context.cellMetrics.toPixelX(rect.x) + math.round(xPx)
                val screenYPx = cursorTopPx(
                  context.cellMetrics.toPixelY(screenYCell),
                  context.cellMetrics.toPixelY(rect.y + placement.top),
                  context.cellMetrics.lineHeight
                )
                context.surface.fillPixelRect(
                  screenXPx,
                  screenYPx,
                  caretWidthPx,
                  context.cellMetrics.lineHeight,
                  effectiveCursorColor
                )
            case _ => ()
        }
    }

  private def markdownLensPlacement(
    blockRange: Range.Inclusive,
    blockVisualLines: Vector[TextVisualLine],
    primaryCursor: Option[CursorPosition],
    visibleHeight: Int,
    snapshot: TextLayoutSnapshot,
    markdownLines: Vector[String],
    previewWindow: MarkdownDocumentPreview.PreviewWindow
  ): MarkdownLensPlacement =
    val previewRange = MarkdownDocumentPreview.previewRowsForSourceRange(markdownLines, blockRange)
    val lensHeight = math.max(
      blockVisualLines.length,
      previewRange.map(range => range.end - range.start + 1).getOrElse(0)
    )
    val cursorVisualRow = primaryCursor.flatMap { cursor =>
      MarkdownDocumentPreview
        .previewRowForSourceLine(markdownLines, cursor.line)
        .map(_ - previewWindow.firstPreviewRow)
        .orElse(calculateCursorVisualPosition(cursor, snapshot).map(_._1))
    }
    val desiredTop =
      previewRange
        .map(_.start - previewWindow.firstPreviewRow)
        .orElse(cursorVisualRow.map(row => row - lensHeight / 2))
        .getOrElse(blockVisualLines.headOption.fold(0)(_.bufferLine))
    val visibleLensHeight = lensHeight.max(1).min(visibleHeight.max(1))
    MarkdownLensPlacement(
      top = desiredTop.max(0).min(math.max(0, visibleHeight - visibleLensHeight)),
      height = visibleLensHeight
    )

  private def cursorForBlock(cursors: List[CursorPosition], blockRange: Range.Inclusive): Option[CursorPosition] =
    cursors.find(cursor => blockRange.contains(cursor.line)).orElse(cursors.headOption)

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
    config: AppConfig,
    context: RenderContext,
    snapshot: TextLayoutSnapshot
  ): Unit =

    buffer.cursors.zipWithIndex.foreach { (cursor, cursorIndex) =>
      val isPrimaryCursor = cursorIndex == 0
      val shouldRenderCursor =
        context.cursorVisible || (buffer.cursors.size > 1 && !isPrimaryCursor)
      calculateCursorVisualPosition(cursor, snapshot) match
        case Some((visualLine, xPx)) if shouldRenderCursor =>
          val lineTopPx = visualLineTopPx(rect, visualLine, context, snapshot)
          if visualLineVisible(rect, visualLine, context, snapshot)
          then
            val effectiveCursorColor = cursorColorFor(config, theme, context, isPrimaryCursor)
            val caretWidthPx         = math.max(2, math.round(context.cellMetrics.charWidth * 0.12f))
            val screenXPx            = context.cellMetrics.toPixelX(rect.x) + math.round(xPx)
            val screenYPx =
              if snapshot.usesMeasuredLayout then lineTopPx
              else
                cursorTopPx(
                  lineTopPx,
                  contentTopPx(rect, context),
                  snapshot.lineHeightPx
                )
            context.surface.fillPixelRect(
              screenXPx,
              screenYPx,
              caretWidthPx,
              snapshot.lineHeightPx,
              effectiveCursorColor
            )
        case _ => ()
    }

  private def cursorTopPx(rowTopPx: Int, contentTopPx: Int, lineHeightPx: Int): Int =
    math.max(contentTopPx, rowTopPx - cursorOpticalLiftPx(lineHeightPx))

  private def cursorOpticalLiftPx(lineHeightPx: Int): Int =
    math.max(2, math.round(lineHeightPx.toFloat * 0.125f))

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

  private def cursorColorFor(
    config: AppConfig,
    theme: Theme,
    context: RenderContext,
    isPrimaryCursor: Boolean
  ): java.awt.Color =
    val activeColor = context.cursorColorOverride.getOrElse(config.cursorColors.activeOr(theme.cursor))
    if isPrimaryCursor then activeColor
    else config.cursorColors.inactiveOr(activeColor)

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
        context.cellMetrics
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
        context.cellMetrics
      )
    }

    context.layout.floatingPanelRect.foreach(rect => renderFloatingPanelPlaceholder(rect, state.theme, context))

  private def renderPinnedPanels(state: AppState, context: RenderContext): Unit =
    context.surface.setFont(context.uiFont)
    (state.pinnedSurfaces ++ state.uiSurfaces.filter {
      _.presentation match
        case SurfacePresentation.Expanded(_, _) => true
        case _                                  => false
    }).foreach {
      case surface @ UiSurface(_, content, SurfacePresentation.Pinned(position, _), _) =>
        context.layout.pinnedSurfaceRects
          .get(surface.id)
          .orElse(context.layout.pinnedPanelRects.get(position))
          .foreach { rect =>
            val animationState =
              state.surfaceAnimations
                .get(surface.id)
                .map(_.animationState)
                .getOrElse(com.serenity.animation.AnimationState.empty)
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
                renderMarkdownPreviewPanel(bufferId, title, rect, state, context, animationState)
              case _ =>
                PinnedPanelRenderer.render(
                  context.surface,
                  PinnedPanelViewModel.resolve(surface, rect, state),
                  state.theme,
                  state.config,
                  animationState
                )
          }
      case surface @ UiSurface(_, content, SurfacePresentation.Expanded(_, _), _) =>
        context.layout.expandedPanelRect.foreach { rect =>
          val animationState =
            state.surfaceAnimations
              .get(surface.id)
              .map(_.animationState)
              .getOrElse(com.serenity.animation.AnimationState.empty)
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
              renderMarkdownPreviewPanel(bufferId, title, rect, state, context, animationState)
            case _ =>
              PinnedPanelRenderer.render(
                context.surface,
                PinnedPanelViewModel.resolve(surface, rect, state),
                state.theme,
                state.config,
                animationState
              )
        }
      case _ => ()
    }

  private def renderMarkdownPreviewPanel(
    bufferId: BufferId,
    title: String,
    rect: LayoutRect,
    state: AppState,
    context: RenderContext,
    animationState: com.serenity.animation.AnimationState
  ): Unit =
    val shell = TextPanelView(rect, s"Preview: $title", Nil)
    PinnedPanelRenderer.render(context.surface, shell, state.theme, state.config, animationState)

    val contentWidthCells  = math.max(1, rect.width - 2)
    val contentHeightCells = math.max(1, rect.height - 2)
    val widthPx            = contentWidthCells * context.cellMetrics.charWidth
    val heightPx           = contentHeightCells * context.cellMetrics.lineHeight
    val buffer             = state.buffers.get(bufferId)
    val content = buffer
      .map(buffer => markdownPreviewWindow(buffer, markdownSourceLines(buffer), contentHeightCells).source)
      .getOrElse("")
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

  private def renderLineNumbers(state: AppState, context: RenderContext, renderPlan: EditorPaneRenderPlan): Unit =
    if state.config.showLineNumbers then
      context.surface.setFont(context.uiFont)
      context.layout.lineNumberRect foreach { lineRect =>
        val surface = context.surface

        surface.setBackgroundColor(state.theme.panel.background)
        surface.setForegroundColor(state.theme.muted)

        surface.fillRect(lineRect.x, lineRect.y, lineRect.width, lineRect.height, ' ')
        state.layout.activeEditorPaneId
          .flatMap(state.layout.editorPanes.get)
          .foreach { pane =>
            val buffer = pane.bufferId.flatMap(state.buffers.get)
            val snapshot =
              state.layout.activeEditorPaneId
                .flatMap(renderPlan.snapshots.get)
                .orElse {
                  for
                    paneLayout <- state.layout.activeEditorPaneId.flatMap(renderPlan.paneLayouts.get)
                    buf        <- buffer
                  yield snapshotForBuffer(buf, paneLayout.contentRect, state, context)
                }
            snapshot.foreach { snapshot =>
              snapshot.visualLines.zipWithIndex.foreach {
                case (visualLine, index) if visualLineFits(lineRect, index, context, snapshot) =>
                  val screenY   = lineRect.y + index
                  val lineTopPx = visualLineTopPx(lineRect, index, context, snapshot)
                  if shouldRenderLineNumberForVisualLine(visualLine, state.config.wordWrapEnabled) then
                    val numberWidth = math.max(1, lineRect.width - 1)
                    val lineNumberText =
                      (visualLine.bufferLine + 1).toString.reverse.padTo(numberWidth, ' ').reverse + " "
                    val measuredLineNumberFont = buffer.filter(useMeasuredLineNumberFont(_, context))
                    if snapshot.usesMeasuredLayout && measuredLineNumberFont.nonEmpty then
                      measuredLineNumberFont.foreach(buf => surface.setFont(context.fontForBuffer(buf)))
                      surface.drawRunPx(
                        context.cellMetrics.toPixelX(lineRect.x).toFloat,
                        lineTopPx,
                        lineRect.width * context.cellMetrics.charWidth.toFloat,
                        snapshot.lineHeightPx,
                        snapshot.ascentPx,
                        lineNumberText
                      )
                      surface.setFont(context.uiFont)
                    else surface.putString(lineRect.x, screenY, lineNumberText)
                    buffer.foreach(
                      renderDiagnosticIndicator(surface, lineRect, screenY, visualLine.bufferLine, _, state)
                    )
                case _ => ()
              }
            }
          }
      }

  private def useMeasuredLineNumberFont(buffer: Buffer, context: RenderContext): Boolean =
    buffer.typographyRole != TypographyRole.Code && context.fontForBuffer(buffer) != context.codeFont

  private def shouldRenderLineNumberForVisualLine(visualLine: TextVisualLine, wordWrapEnabled: Boolean): Boolean =
    !wordWrapEnabled || visualLine.startColumn == 0

  private def renderDiagnosticIndicator(
    surface: RenderSurface,
    lineRect: LayoutRect,
    screenY: Int,
    bufferLineIndex: Int,
    buffer: Buffer,
    state: AppState
  ): Unit =
    val uri = SpellChecker.diagnosticsUri(buffer)
    {
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
    context.layout.gutterRect.foreach { gutterRect =>
      context.surface.setFont(context.uiFont)
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
    if state.config.cursorInfoBarPlacement == CursorInfoBarPlacement.PinnedBottom then
      state.cursorInfoBarText.map(text => s" $text ").getOrElse(legacyGutterContent(state))
    else legacyGutterContent(state)

  private def legacyGutterContent(state: AppState): String =
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
