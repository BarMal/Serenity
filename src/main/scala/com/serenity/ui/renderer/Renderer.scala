package com.serenity.ui.renderer

import java.awt.{Color, Font}

import com.serenity.animation.ThemeInterpolator
import com.serenity.config.{AppConfig, CursorInfoBarPlacement, MarkdownViewMode}
import com.serenity.lsp.config.LanguageId
import com.serenity.markdown.{MarkdownBlockLens, MarkdownDocumentPreview}
import com.serenity.spellcheck.SpellChecker
import com.serenity.state.models.*
import com.serenity.ui.layout.*
import com.serenity.ui.theme.*

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
      workspaceLayout: EditorWorkspaceLayout,
      layoutContract: EditorLayoutContract,
      snapshots: Map[PaneId, TextLayoutSnapshot]
  ):
    def paneLayouts: Map[PaneId, EditorPaneLayout] = workspaceLayout.paneLayouts

  private case class MarkdownLensFrame(
      lines: Vector[String],
      previewWindow: MarkdownDocumentPreview.PreviewWindow,
      activeSourceRanges: List[Range.Inclusive],
      previewRows: Vector[MarkdownDocumentPreview.InlinePreviewLine],
      placements: Map[Range.Inclusive, MarkdownLensPlacement]
  )

  private case class MarkdownLensPreviewWindow(
      window: MarkdownDocumentPreview.PreviewWindow,
      sourceLineCount: Int
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
    cursorColor: Option[java.awt.Color],
    repaintOnFlush: Boolean
  ): Unit =
    val state0       = withEffectiveTheme(state)
    val publishFrame = if repaintOnFlush then swingWin.onImageReady else swingWin.onBaseImageReady
    val surface      = Java2DRenderSurface.forFrame(swingWin.metrics, codeFont, swingWin.canvas, publishFrame)
    val viewportSize = swingWin.viewportSize
    val layout       = LayoutEngine.calculateLayout(state0, viewportSize)
    val _ = renderFrame(
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

  def renderCursorOnly(
    state: AppState,
    cursorVisible: Boolean,
    swingWin: com.serenity.ui.terminal.SwingWindow,
    codeFont: java.awt.Font,
    textFont: java.awt.Font,
    uiFont: java.awt.Font,
    uiMetrics: CellMetrics,
    cursorColor: Option[java.awt.Color]
  ): Boolean =
    val state0       = withEffectiveTheme(state)
    val viewportSize = swingWin.viewportSize
    val layout       = LayoutEngine.calculateLayout(state0, viewportSize)
    swingWin.onCursorOverlayReady { image =>
      val surface =
        Java2DRenderSurface.forImage(image, swingWin.metrics, codeFont, swingWin.canvas, _ => ())
      val context =
        RenderContext(
          surface,
          layout,
          cursorVisible,
          cursorColor,
          codeFont,
          textFont,
          uiFont,
          swingWin.metrics,
          uiMetrics
        )
      val renderPlan = prepareEditorPaneRenderPlan(state0, context)
      renderEditorCursors(state0, context, renderPlan)
      surface.flush()
    }

  /** Render a base frame and its cursor overlay without recalculating the editor layout. */
  def renderWithCursorOverlay(
    state: AppState,
    swingWin: com.serenity.ui.terminal.SwingWindow,
    codeFont: java.awt.Font,
    textFont: java.awt.Font,
    uiFont: java.awt.Font,
    uiMetrics: CellMetrics,
    cursorColor: Option[java.awt.Color]
  ): Boolean =
    val state0       = withEffectiveTheme(state)
    val viewportSize = swingWin.viewportSize
    val layout       = LayoutEngine.calculateLayout(state0, viewportSize)
    val surface = Java2DRenderSurface.forFrame(
      swingWin.metrics,
      codeFont,
      swingWin.canvas,
      swingWin.onBaseImageReady
    )
    renderFrame(
      state0,
      cursorVisible = false,
      surface,
      viewportSize,
      layout,
      codeFont,
      textFont,
      uiFont,
      swingWin.metrics,
      uiMetrics,
      cursorColor = None
    ).fold(false) { renderPlan =>
      swingWin.onCursorOverlayReady { image =>
        val cursorSurface =
          Java2DRenderSurface.forImage(image, swingWin.metrics, codeFont, swingWin.canvas, _ => ())
        val cursorContext = RenderContext(
          cursorSurface,
          layout,
          true,
          cursorColor,
          codeFont,
          textFont,
          uiFont,
          swingWin.metrics,
          uiMetrics
        )
        renderEditorCursors(state0, cursorContext, renderPlan)
        cursorSurface.flush()
      }
    }

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
    val _ = renderFrame(
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
  ): Option[EditorPaneRenderPlan] =
    surface.hideCursor()
    surface.clearViewport(state.theme.background)

    val editorRenderPlan = state.startPageSurface.flatMap {
      _.content match
        case SurfaceContent.StartPage(page) => Some(page)
        case _                              => None
    } match
      case Some(page) =>
        renderStartPage(page, surface, viewportSize, state.theme, uiFont, cellMetrics, uiMetrics)
        val floatContext =
          RenderContext(surface, layout, cursorVisible, cursorColor, codeFont, textFont, uiFont, cellMetrics, uiMetrics)
        renderFloatingPanels(state, floatContext)
        None
      case None =>
        val context =
          RenderContext(surface, layout, cursorVisible, cursorColor, codeFont, textFont, uiFont, cellMetrics, uiMetrics)
        val editorRenderPlan = prepareEditorPaneRenderPlan(state, context)
        renderSpacerColumns(state, context, editorRenderPlan.layoutContract)
        renderLineNumbers(state, context, editorRenderPlan)
        renderGutter(state, context, editorRenderPlan.layoutContract)
        renderPinnedPanels(state, context)
        renderEditorPanes(state, context, editorRenderPlan)
        renderFloatingPanels(state, context)
        Some(editorRenderPlan)

    surface.applyPostProcessing(state.config.postProcessingEffect)
    surface.flush()
    editorRenderPlan

  private def renderSpacerColumns(state: AppState, context: RenderContext, contract: EditorLayoutContract): Unit =
    val surface = context.surface
    surface.setBackgroundColor(state.theme.margin)
    List(contract.leftSpacerRect, contract.rightSpacerRect)
      .filter(rect => rect.width > 0 && rect.height > 0)
      .foreach(rect => surface.fillRect(rect.x, rect.y, rect.width, rect.height, ' '))

  private def prepareEditorPaneRenderPlan(state: AppState, context: RenderContext): EditorPaneRenderPlan =
    val layoutContract =
      EditorLayoutContract.from(
        state,
        ViewportSize(context.surface.viewportWidth, context.surface.viewportHeight),
        context.layout
      )
    val workspaceLayout = layoutContract.workspace
    val snapshots =
      state.layout.editorPanes.flatMap {
        case (paneId, pane) =>
          for
            paneLayout <- workspaceLayout.paneLayouts.get(paneId)
            bufferId   <- pane.bufferId
            buffer     <- state.buffers.get(bufferId)
          yield paneId -> snapshotForBuffer(buffer, paneLayout.contentRect, state, context)
      }

    EditorPaneRenderPlan(workspaceLayout, layoutContract, snapshots)

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
        case Some(paneLayout) =>
          renderEditorPane(
            pane,
            paneLayout,
            state,
            context,
            renderPlan.snapshots.get(paneId),
            renderPlan.layoutContract
          )
        case None => ()
    }

  private def renderEditorCursors(state: AppState, context: RenderContext, renderPlan: EditorPaneRenderPlan): Unit =
    val activePaneId = state.layout.activeEditorPaneId
    val orderedPanes =
      activePaneId.toList.flatMap(id => state.layout.editorPanes.get(id).map(id -> _)) ++
        state.layout.editorPanes.toList.filterNot((id, _) => activePaneId.contains(id)).sortBy(_._1.value)

    orderedPanes.foreach {
      case (paneId, pane) =>
        for
          paneLayout <- renderPlan.paneLayouts.get(paneId)
          bufferId   <- pane.bufferId
          buffer     <- state.buffers.get(bufferId)
          snapshot   <- renderPlan.snapshots.get(paneId)
        do renderCursors(buffer, paneLayout.contentRect, state.theme, state.config, context, snapshot)
    }

  private def renderEditorPane(
    pane: EditorPane,
    paneLayout: EditorPaneLayout,
    state: AppState,
    context: RenderContext,
    preparedSnapshot: Option[TextLayoutSnapshot],
    contract: EditorLayoutContract
  ): Unit =
    val buffer = pane.bufferId.flatMap(state.buffers.get)

    renderBufferHeader(pane, buffer, paneLayout, state, context, contract)
    renderEditorPaneVerticalSpacers(paneLayout, state, context)

    val contentRect = paneLayout.contentRect

    val bufferSnapshot = preparedSnapshot.orElse(buffer.map(snapshotForBuffer(_, contentRect, state, context)))
    val markdownLensFrame =
      buffer.collect {
        case buf if isInlineMarkdownLens(buf, state) => markdownLensFrameFor(buf, bufferSnapshot.get)
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
          markdownLensFrame.getOrElse(markdownLensFrameFor(buf, bufferSnapshot.get))
        )
      else renderCursors(buf, contentRect, state.theme, state.config, cursorContext, bufferSnapshot.get)
    }

  private def renderBufferHeader(
    pane: EditorPane,
    buffer: Option[Buffer],
    paneLayout: EditorPaneLayout,
    state: AppState,
    context: RenderContext,
    contract: EditorLayoutContract
  ): Unit =
    context.surface.setFont(context.uiFont)
    val surface    = context.surface
    val isActive   = state.layout.activeEditorPaneId.contains(pane.id)
    val headerRect = contract.paneHeaderRect(pane.id).getOrElse(paneLayout.headerRect)
    val titleRect  = contract.paneTitleRect(pane.id).getOrElse(paneLayout.titleRect)

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

  private def renderEditorPaneVerticalSpacers(
    paneLayout: EditorPaneLayout,
    state: AppState,
    context: RenderContext
  ): Unit =
    context.surface.setBackgroundColor(state.theme.margin)
    List(paneLayout.topSpacerRect, paneLayout.bottomSpacerRect)
      .filter(rect => rect.width > 0 && rect.height > 0)
      .foreach(rect => context.surface.fillRect(rect.x, rect.y, rect.width, rect.height, ' '))
    context.surface.setBackgroundColor(state.theme.background)
    context.surface.setForegroundColor(state.theme.foreground)

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
      val frame = markdownLensFrame.getOrElse(markdownLensFrameFor(buffer, snapshot))
      renderInlineMarkdownPreview(buffer, rect, state, context, frame)
      renderMarkdownRawLenses(buffer, rect, state, context, snapshot, frame)
    else renderPlainBufferContent(buffer, rect, state, context, snapshot)

  private def renderPlainBufferContent(
    buffer: Buffer,
    rect: LayoutRect,
    state: AppState,
    context: RenderContext,
    snapshot: TextLayoutSnapshot
  ): Unit =
    val visualLines     = snapshot.visualLines
    val xOriginPx       = context.cellMetrics.toPixelX(rect.x).toFloat
    val contentRightXPx = context.cellMetrics.toPixelX(rect.right).toFloat
    val activeBodyLines = focusedTextBodyLines(buffer, state)

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
            val styledSegments = visualLineStyledSegments(visualLine, lineTheme, snapshot, activeBodyLines)
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
                styledSegments,
                clipRightXPx = Some(contentRightXPx)
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
    textRowMetrics(rect, context, snapshot).lineFits(screenLineIndex)

  private def visualLineVisible(
    rect: LayoutRect,
    screenLineIndex: Int,
    context: RenderContext,
    snapshot: TextLayoutSnapshot
  ): Boolean =
    textRowMetrics(rect, context, snapshot).lineVisible(screenLineIndex, context.surface.viewportHeight)

  private def visualLineTopPx(
    rect: LayoutRect,
    screenLineIndex: Int,
    context: RenderContext,
    snapshot: TextLayoutSnapshot
  ): Int =
    textRowMetrics(rect, context, snapshot).lineTopPx(screenLineIndex)

  private def textRowMetrics(
    rect: LayoutRect,
    context: RenderContext,
    snapshot: TextLayoutSnapshot
  ): TextRowMetrics =
    TextRowMetrics(
      contentRect = rect,
      gridMetrics = context.cellMetrics,
      rowLineHeightPx = snapshot.lineHeightPx,
      usesMeasuredLayout = snapshot.usesMeasuredLayout
    )

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

  private def visualLineStyledSegments(
    visualLine: TextVisualLine,
    theme: Theme,
    snapshot: TextLayoutSnapshot,
    activeBodyLines: Set[Int]
  ): Option[List[StyledText]] =
    val richSegments = richTextStyledSegments(visualLine, theme, snapshot)
    if activeBodyLines.nonEmpty && !activeBodyLines.contains(visualLine.bufferLine) then
      val baseSegments =
        richSegments.getOrElse(List(StyledText(visualLine.text, TextStyle.normal, theme.foreground, theme.background)))
      Some(baseSegments.map(segment => segment.copy(foregroundColor = theme.muted, backgroundColor = theme.background)))
    else richSegments

  private def focusedTextBodyLines(buffer: Buffer, state: AppState): Set[Int] =
    if !state.config.focusedTextBodyEnabled then Set.empty
    else
      val lines      = buffer.content.linesFrom(0, buffer.content.lineCount)
      val activeLine = buffer.cursors.headOption.map(_.line)
      if buffer.language.contains(LanguageId.Markdown) then MarkdownBlockLens.activeBlockLineSet(lines, activeLine)
      else plainTextBodyLineSet(lines, activeLine)

  private def plainTextBodyLineSet(lines: Vector[String], activeLine: Option[Int]): Set[Int] =
    activeLine
      .filter(line => line >= 0 && line < lines.length)
      .map { line =>
        if lines(line).trim.isEmpty then Set(line)
        else
          val start = Iterator
            .iterate(line)(_ - 1)
            .takeWhile(index => index >= 0 && lines(index).trim.nonEmpty)
            .foldLeft(line)((_, index) => index)
          val end = Iterator
            .iterate(line)(_ + 1)
            .takeWhile(index => index < lines.length && lines(index).trim.nonEmpty)
            .foldLeft(line)((_, index) => index)
          (start to end).toSet
      }
      .getOrElse(Set.empty)

  private def renderInlineMarkdownPreview(
    buffer: Buffer,
    rect: LayoutRect,
    state: AppState,
    context: RenderContext,
    frame: MarkdownLensFrame
  ): Unit =
    val widthPx =
      scaledImagePixelDimension(rect.width * context.cellMetrics.charWidth, context.surface.devicePixelScaleX)
    val heightPx =
      scaledImagePixelDimension(rect.height * context.cellMetrics.lineHeight, context.surface.devicePixelScaleY)
    val previewFont = MarkdownDocumentPreview.inlineLensFont(
      context.textFont,
      context.cellMetrics.lineHeight,
      context.surface.devicePixelScaleY
    )
    val title = buffer.filePath.flatMap(path => Option(path.getFileName).map(_.toString)).getOrElse("Untitled")
    val image = MarkdownDocumentPreview.renderInlineRowsImage(
      rows = frame.previewRows,
      sourceLines = frame.lines,
      title = title,
      widthPx = widthPx,
      heightPx = heightPx,
      theme = state.theme,
      font = previewFont,
      inlineLineHeightPx = MarkdownDocumentPreview.lineHeightForDeviceScale(
        context.cellMetrics.lineHeight,
        context.surface.devicePixelScaleY
      )
    )
    context.surface.drawImage(image, rect.x, rect.y, rect.width, rect.height)

  private def scaledImagePixelDimension(logicalPx: Int, scale: Double): Int =
    math.ceil(logicalPx.max(1).toDouble * scale.max(1.0)).toInt.max(1)

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
              measuredRunWidthWithin(rect, context, startXPx, endXPx).foreach { widthPx =>
                surface.setForegroundColor(theme.highlighted.foreground)
                surface.setBackgroundColor(theme.highlighted.background)
                surface.drawRunPx(
                  startXPx,
                  lineTopPx,
                  widthPx,
                  snapshot.lineHeightPx,
                  snapshot.ascentPx,
                  selectedText,
                  clipGlyphToRun = true
                )
              }
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
        val desiredWidthPx = math.max(context.cellMetrics.charWidth.toFloat, endXPx - startXPx)
        measuredRunWidthWithin(rect, context, startXPx, startXPx + desiredWidthPx).foreach { widthPx =>
          surface.setForegroundColor(foreground)
          surface.setBackgroundColor(background)
          surface.drawRunPx(
            startXPx,
            lineTopPx,
            widthPx,
            snapshot.lineHeightPx,
            snapshot.ascentPx,
            rangeText,
            clipGlyphToRun = true
          )
        }
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

  private def markdownLensFrameFor(buffer: Buffer, snapshot: TextLayoutSnapshot): MarkdownLensFrame =
    val lines         = markdownSourceLines(buffer)
    val previewWindow = markdownPreviewWindow(buffer, lines, buffer.viewport.visibleLines)
    val activeRanges  = activeMarkdownBlockRanges(lines, buffer)
    val baseRows = MarkdownDocumentPreview.inlinePreviewRows(
      lines,
      previewWindow.window.firstSourceLine,
      previewWindow.sourceLineCount
    )
    val (previewRows, placements) = markdownLensRows(
      baseRows,
      lines,
      activeRanges,
      buffer.cursors.map(_.line).toSet,
      snapshot,
      previewWindow.window
    )
    MarkdownLensFrame(
      lines,
      previewWindow.window,
      activeRanges,
      previewRows,
      placements
    )

  private def markdownLensRows(
    baseRows: Vector[MarkdownDocumentPreview.InlinePreviewLine],
    lines: Vector[String],
    activeRanges: List[Range.Inclusive],
    activeLines: Set[Int],
    snapshot: TextLayoutSnapshot,
    previewWindow: MarkdownDocumentPreview.PreviewWindow
  ): (Vector[MarkdownDocumentPreview.InlinePreviewLine], Map[Range.Inclusive, MarkdownLensPlacement]) =
    val (rows, placements, _) = activeRanges.foldLeft(
      (baseRows, Map.empty[Range.Inclusive, MarkdownLensPlacement], 0)
    ) {
      case ((rows, placements, rowDelta), blockRange) =>
        val previewRange = MarkdownDocumentPreview.previewRowsForSourceRange(lines, blockRange).map { range =>
          (range.start - previewWindow.firstPreviewRow + rowDelta) to
            (range.end - previewWindow.firstPreviewRow + rowDelta)
        }
        val visibleActiveLine = snapshot.visualLines.exists(line => activeLines.contains(line.bufferLine))
        val rawHeight =
          if visibleActiveLine then snapshot.visualLines.count(line => blockRange.contains(line.bufferLine)) else 0
        previewRange match
          case Some(range) if rawHeight > 0 && range.end >= 0 && range.start < rows.length =>
            val start        = range.start.max(0).min(rows.length)
            val endExclusive = (range.end + 1).max(start).min(rows.length)
            val replacedRows = endExclusive - start
            val replacement  = Vector.fill(rawHeight)(MarkdownDocumentPreview.InlinePreviewLine(None, ""))
            val nextRows     = rows.take(start) ++ replacement ++ rows.drop(endExclusive)
            (
              nextRows,
              placements + (blockRange -> MarkdownLensPlacement(start, rawHeight)),
              rowDelta + rawHeight - replacedRows
            )
          case _ => (rows, placements, rowDelta)
    }
    rows -> placements

  private def markdownPreviewWindow(
    buffer: Buffer,
    lines: Vector[String],
    visibleRows: Int
  ): MarkdownLensPreviewWindow =
    if lines.isEmpty then MarkdownLensPreviewWindow(MarkdownDocumentPreview.PreviewWindow(0, 0, ""), 0)
    else
      val activeLine = buffer.cursors.headOption
        .map(_.line)
        .filter(line => line >= 0 && line < lines.length)
      val activeBlock     = activeLine.map(line => MarkdownBlockLens.currentBlock(lines, line))
      val viewportTopLine = buffer.viewport.topLine.max(0).min(lines.length - 1)
      val windowTopLine = activeLine
        .filter(line => line == viewportTopLine && line > 0 && lines(line).trim.isEmpty)
        .filter(line => lines(line - 1).trim.matches("^#{1,6}\\s+.*"))
        .map(_ - 1)
        .getOrElse(viewportTopLine)
      val firstSourceLine = activeBlock
        .filter(blockRange => blockRange.start < windowTopLine && blockRange.end >= windowTopLine)
        .map(_.start)
        .getOrElse(windowTopLine)
      val baseSourceLineLimit = markdownPreviewSourceLineLimit(visibleRows)
      val windowEndLine       = firstSourceLine + baseSourceLineLimit - 1
      val maxSourceLines = activeBlock
        .filter(_ => activeLine.exists(_ <= windowEndLine))
        .filter(blockRange => blockRange.end - blockRange.start + 1 <= baseSourceLineLimit)
        .map(blockRange => math.max(baseSourceLineLimit, blockRange.end - firstSourceLine + baseSourceLineLimit))
        .getOrElse(baseSourceLineLimit)
      MarkdownLensPreviewWindow(
        window = MarkdownDocumentPreview.PreviewWindow(
          firstSourceLine = firstSourceLine,
          firstPreviewRow =
            MarkdownDocumentPreview.previewRowForSourceLine(lines, firstSourceLine).getOrElse(firstSourceLine),
          source = ""
        ),
        sourceLineCount = math.min(maxSourceLines, lines.length - firstSourceLine)
      )

  private def markdownPreviewSourceLineLimit(visibleRows: Int): Int =
    math.max(MinMarkdownPreviewSourceLines, visibleRows.max(1) * MarkdownPreviewOverscanFactor)

  private def activeMarkdownBlockRanges(lines: Vector[String], buffer: Buffer): List[Range.Inclusive] =
    val cursorRanges = buffer.cursors
      .map(_.line)
      .filter(line => line >= 0 && line < lines.length)
      .map(line => MarkdownBlockLens.currentBlock(lines, line))
    val selectionRanges = buffer.allSelections.flatMap { selection =>
      if lines.isEmpty then Nil
      else
        val startLine = selection.start.line.max(0).min(lines.length - 1)
        val endLine   = selection.end.line.max(0).min(lines.length - 1)
        (startLine to endLine).map(line => MarkdownBlockLens.currentBlock(lines, line)).toList
    }
    mergeOverlappingMarkdownRanges(cursorRanges ++ selectionRanges)

  private def mergeOverlappingMarkdownRanges(ranges: List[Range.Inclusive]): List[Range.Inclusive] =
    ranges
      .sortBy(range => (range.start, range.end))
      .foldLeft(List.empty[Range.Inclusive]) {
        case (last :: rest, range) if range.start <= last.end =>
          (last.start to last.end.max(range.end)) :: rest
        case (merged, range) =>
          range :: merged
      }
      .reverse

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
    frame.activeSourceRanges.foreach { blockRange =>
      val blockVisualLines = snapshot.visualLines.filter(line => blockRange.contains(line.bufferLine))
      if blockVisualLines.nonEmpty then
        val placement = frame.placements.getOrElse(
          blockRange,
          markdownLensPlacement(blockRange, blockVisualLines, rect.height, lines, previewWindow)
        )
        val lensY = rect.y + placement.top
        context.surface.setBackgroundColor(state.theme.panel.background)
        context.surface.fillRect(rect.x, lensY, rect.width, placement.height, ' ')
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
                  language = None,
                  clipRightXPx = Some(context.cellMetrics.toPixelX(rect.right).toFloat)
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
              renderSelectionHighlights(
                context.surface,
                buffer,
                visualLine,
                rect,
                screenY,
                context.cellMetrics.toPixelY(screenY),
                state.theme,
                context,
                snapshot
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
    frame.activeSourceRanges.foreach { blockRange =>
      val blockVisualLines = snapshot.visualLines.filter(line => blockRange.contains(line.bufferLine))
      if blockVisualLines.nonEmpty then
        val placement = frame.placements.getOrElse(
          blockRange,
          markdownLensPlacement(blockRange, blockVisualLines, rect.height, lines, previewWindow)
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
                val screenYPx = TextRowMetrics(
                  contentRect = rect.copy(y = rect.y + placement.top, height = placement.height),
                  gridMetrics = context.cellMetrics,
                  rowLineHeightPx = context.cellMetrics.lineHeight,
                  usesMeasuredLayout = false
                ).cursorTopPx(visualLine)
                caretWithin(rect, context.cellMetrics, screenXPx, caretWidthPx).foreach { (caretXPx, widthPx) =>
                  context.surface.fillPixelRect(
                    caretXPx,
                    screenYPx,
                    widthPx,
                    context.cellMetrics.lineHeight,
                    effectiveCursorColor
                  )
                }
            case _ => ()
        }
    }

  private def markdownLensPlacement(
    blockRange: Range.Inclusive,
    blockVisualLines: Vector[TextVisualLine],
    visibleHeight: Int,
    markdownLines: Vector[String],
    previewWindow: MarkdownDocumentPreview.PreviewWindow
  ): MarkdownLensPlacement =
    val previewRange = MarkdownDocumentPreview.previewRowsForSourceRange(markdownLines, blockRange)
    val lensHeight = math.max(
      blockVisualLines.length,
      previewRange.map(range => range.end - range.start + 1).getOrElse(0)
    )
    val desiredTop = previewRange
      .map(_.start - previewWindow.firstPreviewRow)
      .getOrElse(blockRange.start - previewWindow.firstSourceLine)
    val visibleLensHeight = lensHeight.max(1).min(visibleHeight.max(1))
    MarkdownLensPlacement(
      top = desiredTop.max(0).min(math.max(0, visibleHeight - visibleLensHeight)),
      height = visibleLensHeight
    )

  private def renderEmptyPane(rect: LayoutRect, theme: Theme, context: RenderContext): Unit =
    val textMetrics  = CellMetrics.fromFont(context.textFont)
    val lineHeightPx = math.max(context.cellMetrics.lineHeight, textMetrics.lineHeight)
    val yPx          = centeredBlockTopPx(rect, context.cellMetrics, 1, lineHeightPx)
    context.surface.setFont(context.textFont)
    context.surface.setForegroundColor(theme.muted)
    context.surface.setBackgroundColor(theme.background)
    renderAlignedTextLine(
      surface = context.surface,
      line = "~ Empty ~",
      rect = rect,
      yPx = yPx,
      font = context.textFont,
      cellMetrics = context.cellMetrics,
      textMetrics = textMetrics
    )

  private def renderWelcomeText(rect: LayoutRect, theme: Theme, context: RenderContext): Unit =
    val lines = List(
      "Welcome to Serenity!",
      "",
      "Start typing to edit text.",
      "",
      "Press Ctrl+P for command palette"
    )

    val textMetrics  = CellMetrics.fromFont(context.textFont)
    val lineHeightPx = math.max(context.cellMetrics.lineHeight, textMetrics.lineHeight)
    val startYPx     = centeredBlockTopPx(rect, context.cellMetrics, lines.length, lineHeightPx)

    context.surface.setFont(context.textFont)
    context.surface.setForegroundColor(theme.placeholder)
    context.surface.setBackgroundColor(theme.background)

    lines.zipWithIndex.foreach {
      case (line, index) =>
        renderAlignedTextLine(
          surface = context.surface,
          line = line,
          rect = rect,
          yPx = startYPx + (index * lineHeightPx),
          font = context.textFont,
          cellMetrics = context.cellMetrics,
          textMetrics = textMetrics
        )
    }

  private def renderAlignedTextLine(
    surface: RenderSurface,
    line: String,
    rect: LayoutRect,
    yPx: Int,
    font: java.awt.Font,
    cellMetrics: CellMetrics,
    textMetrics: CellMetrics
  ): Unit =
    val lineHeightPx     = math.max(cellMetrics.lineHeight, textMetrics.lineHeight)
    val viewportHeightPx = surface.viewportHeight * cellMetrics.lineHeight
    if yPx + lineHeightPx > 0 && yPx < viewportHeightPx then
      surface.fontRenderContext match
        case Some(frc) =>
          val placement = TextAlignment.placeLine(
            text = line,
            area = TextAreaPx(
              xPx = cellMetrics.toPixelX(rect.x).toFloat,
              yPx = yPx,
              widthPx = rect.width * cellMetrics.charWidth,
              heightPx = lineHeightPx
            ),
            font = font,
            lineHeightPx = lineHeightPx,
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
          val y       = cellMetrics.toRow(yPx)
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
    val lines         = page.renderLines
    val lineHeightPx  = math.max(cellMetrics.lineHeight, uiMetrics.lineHeight)
    val totalHeightPx = lines.size * lineHeightPx
    val startYPx      = math.max(0, ((viewportSize.height * cellMetrics.lineHeight) - totalHeightPx) / 2)

    val titleLines       = 2
    val optionStartIndex = titleLines
    val optionEndIndex   = titleLines + page.options.size - 1

    lines.zipWithIndex.foreach {
      case (line, lineIndex) =>
        val yPx = startYPx + (lineIndex * lineHeightPx)

        if yPx + lineHeightPx > 0 && yPx < viewportSize.height * cellMetrics.lineHeight then
          val isOption    = lineIndex >= optionStartIndex && lineIndex <= optionEndIndex
          val optionIndex = lineIndex - optionStartIndex
          val isSelected  = isOption && optionIndex == page.selectedIndex

          if isSelected then
            surface.setForegroundColor(theme.highlighted.foreground)
            surface.setBackgroundColor(theme.highlighted.background)
            surface.fillPixelRect(
              xPx = 0,
              yPx = yPx,
              widthPx = viewportSize.width * cellMetrics.charWidth,
              heightPx = lineHeightPx,
              color = theme.highlighted.background
            )
            renderCenteredStartPageLine(surface, line, yPx, viewportSize, uiFont, cellMetrics, uiMetrics)
          else
            surface.setForegroundColor(theme.placeholder)
            surface.setBackgroundColor(theme.background)
            renderCenteredStartPageLine(surface, line, yPx, viewportSize, uiFont, cellMetrics, uiMetrics)
    }

  private def renderCenteredStartPageLine(
    surface: RenderSurface,
    line: String,
    yPx: Int,
    viewportSize: ViewportSize,
    uiFont: java.awt.Font,
    cellMetrics: CellMetrics,
    uiMetrics: CellMetrics
  ): Unit =
    surface.fontRenderContext match
      case Some(frc) =>
        val lineHeightPx = math.max(cellMetrics.lineHeight, uiMetrics.lineHeight)
        val placement = TextAlignment.placeLine(
          text = line,
          area = TextAreaPx(
            xPx = 0.0f,
            yPx = yPx,
            widthPx = viewportSize.width * cellMetrics.charWidth,
            heightPx = lineHeightPx
          ),
          font = uiFont,
          lineHeightPx = lineHeightPx,
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
        val y = cellMetrics.toRow(yPx)
        val x = math.max(0, (viewportSize.width - line.length) / 2)
        CharacterRenderer.renderString(surface, x, y, line)

  private def centeredBlockTopPx(
    rect: LayoutRect,
    cellMetrics: CellMetrics,
    lineCount: Int,
    lineHeightPx: Int
  ): Int =
    val contentTopPx    = cellMetrics.toPixelY(rect.y)
    val contentHeightPx = rect.height * cellMetrics.lineHeight
    contentTopPx + math.max(0, (contentHeightPx - (lineCount * lineHeightPx)) / 2)

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
          if visualLineVisible(rect, visualLine, context, snapshot)
          then
            val effectiveCursorColor = cursorColorFor(config, theme, context, isPrimaryCursor)
            val caretWidthPx         = math.max(2, math.round(context.cellMetrics.charWidth * 0.12f))
            val screenXPx            = context.cellMetrics.toPixelX(rect.x) + math.round(xPx)
            val screenYPx =
              textRowMetrics(rect, context, snapshot).cursorTopPx(visualLine)
            caretWithin(rect, context.cellMetrics, screenXPx, caretWidthPx).foreach { (caretXPx, widthPx) =>
              context.surface.fillPixelRect(
                caretXPx,
                screenYPx,
                widthPx,
                snapshot.lineHeightPx,
                effectiveCursorColor
              )
            }
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

  private def measuredRunWidthWithin(
    rect: LayoutRect,
    context: RenderContext,
    startXPx: Float,
    endXPx: Float
  ): Option[Float] =
    val rightXPx = context.cellMetrics.toPixelX(rect.right).toFloat
    Option.when(startXPx < rightXPx)(math.max(0.0f, math.min(endXPx, rightXPx) - startXPx)).filter(_ > 0.0f)

  private def caretWithin(
    rect: LayoutRect,
    cellMetrics: CellMetrics,
    desiredXPx: Int,
    desiredWidthPx: Int
  ): Option[(Int, Int)] =
    val leftXPx  = cellMetrics.toPixelX(rect.x)
    val rightXPx = cellMetrics.toPixelX(rect.right)
    val widthPx  = math.min(math.max(1, desiredWidthPx), rightXPx - leftXPx)
    Option.when(widthPx > 0)(desiredXPx.max(leftXPx).min(rightXPx - widthPx) -> widthPx)

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
      if blurRadius > 0f then renderFloatingBackdrop(overlay, blurRadius, state.config, context)
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
      if blurRadius > 0f then renderFloatingBackdrop(overlay, blurRadius, state.config, context)
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

  private def renderFloatingBackdrop(
    overlay: TextOverlayView,
    blurRadius: Float,
    config: AppConfig,
    context: RenderContext
  ): Unit =
    val offsetPx = FloatingSurfaceGeometry.signedRowOffsetPixels(overlay.verticalOffsetRows, context.cellMetrics)
    context.surface.withPixelTranslation(0.0, offsetPx) {
      context.surface.withRoundRectClip(
        overlay.rect.x,
        overlay.rect.y,
        overlay.rect.width,
        overlay.rect.height,
        config.uiCornerRadiusPx
      ) {
        context.surface.blurRegion(
          overlay.rect.x,
          overlay.rect.y,
          overlay.rect.width,
          overlay.rect.height,
          blurRadius
        )
      }
    }

  private def renderPinnedPanels(state: AppState, context: RenderContext): Unit =
    context.surface.setFont(context.uiFont)
    val contract =
      EditorLayoutContract.from(
        state,
        ViewportSize(context.surface.viewportWidth, context.surface.viewportHeight),
        context.layout
      )
    (state.pinnedSurfaces ++ state.uiSurfaces.filter {
      _.presentation match
        case SurfacePresentation.Expanded(_, _) => true
        case _                                  => false
    }).foreach {
      case surface @ UiSurface(_, content, SurfacePresentation.Pinned(position, _), _) =>
        contract.panelRect(surface.id).foreach { rect =>
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
              renderMarkdownPreviewPanel(
                bufferId,
                title,
                rect,
                contract
                  .panelContentRect(surface.id)
                  .getOrElse(SurfaceFrameLayout.forContent(rect, content).contentRect),
                state,
                context,
                animationState
              )
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
        contract.panelRect(surface.id).foreach { rect =>
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
              renderMarkdownPreviewPanel(
                bufferId,
                title,
                rect,
                contract
                  .panelContentRect(surface.id)
                  .getOrElse(SurfaceFrameLayout.forContent(rect, content).contentRect),
                state,
                context,
                animationState
              )
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
    contentRect: LayoutRect,
    state: AppState,
    context: RenderContext,
    animationState: com.serenity.animation.AnimationState
  ): Unit =
    val shell = TextPanelView(rect = rect, contentRect = Some(contentRect), title = s"Preview: $title", rows = Nil)
    PinnedPanelRenderer.render(context.surface, shell, state.theme, state.config, animationState)

    val imageRect          = markdownPreviewImageRect(rect, contentRect, context)
    val contentWidthCells  = math.max(1, imageRect.width)
    val contentHeightCells = math.max(1, imageRect.height)
    val widthPx =
      scaledImagePixelDimension(contentWidthCells * context.cellMetrics.charWidth, context.surface.devicePixelScaleX)
    val heightPx =
      scaledImagePixelDimension(contentHeightCells * context.cellMetrics.lineHeight, context.surface.devicePixelScaleY)
    val buffer = state.buffers.get(bufferId)
    val content = buffer
      .map(buffer => markdownSplitPreviewWindow(buffer, markdownSourceLines(buffer), contentHeightCells).source)
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
    context.surface.drawImage(image, imageRect.x, imageRect.y, contentWidthCells, contentHeightCells)

  private def markdownPreviewImageRect(
    rect: LayoutRect,
    contentRect: LayoutRect,
    context: RenderContext
  ): LayoutRect =
    val x =
      if rect.x <= 0 then rect.x
      else contentRect.x
    val right =
      if rect.right >= context.surface.viewportWidth then rect.right
      else contentRect.right

    LayoutRect(
      x = x,
      y = contentRect.y,
      width = math.max(1, right - x),
      height = math.max(1, contentRect.height)
    )

  private def markdownSplitPreviewWindow(
    buffer: Buffer,
    lines: Vector[String],
    visibleRows: Int
  ): MarkdownDocumentPreview.PreviewWindow =
    MarkdownDocumentPreview.splitPreviewWindow(
      lines,
      activeLine = buffer.cursors.headOption.map(_.line),
      fallbackTopLine = buffer.viewport.topLine,
      maxSourceLines = markdownPreviewSourceLineLimit(visibleRows)
    )

  private def renderLineNumbers(state: AppState, context: RenderContext, renderPlan: EditorPaneRenderPlan): Unit =
    if state.config.showLineNumbers then
      context.surface.setFont(context.uiFont)
      renderPlan.layoutContract.lineNumberRect foreach { lineRect =>
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
              renderPlan.layoutContract.lineNumberRowSlots(snapshot.visualLines.length).foreach {
                case SurfaceContentRowSlot(SurfaceContentRowKind.Item(index), rowY)
                    if visualLineFits(lineRect, index, context, snapshot) =>
                  snapshot.visualLines.lift(index).foreach { visualLine =>
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
                      else surface.putString(lineRect.x, rowY, lineNumberText)
                      buffer.foreach(
                        renderDiagnosticIndicator(surface, lineRect, rowY, visualLine.bufferLine, _, state)
                      )
                  }
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

  private def renderGutter(state: AppState, context: RenderContext, contract: EditorLayoutContract): Unit =
    contract.gutterRect.foreach { gutterRect =>
      context.surface.setFont(context.uiFont)
      val surface = context.surface

      surface.setBackgroundColor(state.theme.panel.background)
      surface.setForegroundColor(state.theme.panel.foreground)

      surface.fillRect(gutterRect.x, gutterRect.y, gutterRect.width, gutterRect.height, ' ')

      val gutterContent = buildGutterContent(state)
      val displayContent =
        if gutterContent.length > gutterRect.width then gutterContent.take(gutterRect.width - 3) + "..."
        else gutterContent

      drawUiTextInCellRect(surface, context, gutterRect, displayContent)
    }

  private def drawUiTextInCellRect(
    surface: RenderSurface,
    context: RenderContext,
    rect: LayoutRect,
    text: String
  ): Unit =
    val rowHeightPx  = math.max(1, rect.height * context.cellMetrics.lineHeight)
    val lineHeightPx = math.max(1, math.min(context.uiMetrics.lineHeight, rowHeightPx - 2))
    val ascentPx     = math.max(1, math.min(context.uiMetrics.ascent, lineHeightPx))
    val placement = TextAlignment.placeLine(
      text = text,
      area = TextAreaPx(
        xPx = context.cellMetrics.toPixelX(rect.x).toFloat,
        yPx = context.cellMetrics.toPixelY(rect.y),
        widthPx = rect.width * context.cellMetrics.charWidth.toFloat,
        heightPx = rowHeightPx
      ),
      font = context.uiFont,
      lineHeightPx = lineHeightPx,
      ascentPx = ascentPx,
      horizontal = TextHorizontalAlignment.Left,
      vertical = TextVerticalAlignment.Middle,
      fontRenderContext = surface.fontRenderContext.getOrElse(TextLayoutSnapshot.defaultFontRenderContext())
    )

    surface.drawRunPx(
      xPx = placement.xPx,
      yPx = placement.yPx,
      bgWidthPx = placement.widthPx,
      lineHeightPx = placement.lineHeightPx,
      ascentPx = placement.ascentPx,
      s = text
    )

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
