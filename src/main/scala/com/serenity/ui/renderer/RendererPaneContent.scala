package com.serenity.ui.renderer

import com.serenity.lsp.config.LanguageId
import com.serenity.markdown.MarkdownDocumentPreview
import com.serenity.state.manager.FocusedTextBody
import com.serenity.state.models.*
import com.serenity.ui.layout.*
import com.serenity.ui.theme.*

/** Paints each editor pane's own content: the header/spacer chrome and the visible text rows (plain or the inline
  * markdown-lens variant).
  *
  * It also owns the per-row geometry of a pane -- [[textRowMetrics]] and the [[visualLineFits]]/[[visualLineVisible]]/
  * [[visualLineTopPx]]/[[visualLineCellOffset]] questions derived from it. Those are public because everything that
  * paints *onto* a pane row has to place itself against the same rows this object drew: the gutter's line numbers, the
  * caret glyphs, the highlight backgrounds, and the reuse bands [[RendererPaneSetup.paneRecordsFor]] measures. A second
  * copy of this arithmetic anywhere is a renderer that draws half a row out of step with the text.
  */
object RendererPaneContent:

  def renderEditorPanes(
    state: AppState,
    context: RenderContext,
    renderPlan: EditorPaneRenderPlan,
    dirtyRowsByPane: Map[PaneId, Set[Int]]
  ): Unit =
    val activePaneId = state.persisted.layout.activeEditorPaneId
    val orderedPanes =
      state.persisted.layout.orderedPaneIds
        .flatMap(paneId => state.persisted.layout.editorPanes.get(paneId).map(paneId -> _))
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
            renderPlan.layoutContract,
            pane.bufferId.flatMap(renderPlan.annotations.get),
            dirtyRowsByPane.get(paneId)
          )
        case None => ()
    }

  /** Paint every pane's visible cursors and report the union of pixel rects painted across all of them. */
  def renderEditorCursors(
    state: AppState,
    context: RenderContext,
    renderPlan: EditorPaneRenderPlan
  ): List[PixelRect] =
    val activePaneId = state.persisted.layout.activeEditorPaneId
    val orderedPanes =
      activePaneId.toList.flatMap(id => state.persisted.layout.editorPanes.get(id).map(id -> _)) ++
        state.persisted.layout.editorPanes.toList.filterNot((id, _) => activePaneId.contains(id)).sortBy(_._1.value)

    orderedPanes.flatMap {
      case (paneId, pane) =>
        (for
          paneLayout <- renderPlan.paneLayouts.get(paneId)
          bufferId   <- pane.bufferId
          buffer     <- state.persisted.buffers.get(bufferId)
          snapshot   <- renderPlan.snapshots.get(paneId)
        yield RendererCursorGlyphs.renderCursors(
          buffer,
          paneLayout.contentRect,
          state.persisted.theme,
          state.persisted.config,
          context,
          snapshot
        ))
          .getOrElse(Nil)
    }

  private def renderEditorPane(
    pane: EditorPane,
    paneLayout: EditorPaneLayout,
    state: AppState,
    context: RenderContext,
    preparedSnapshot: Option[TextLayoutSnapshot],
    contract: EditorLayoutContract,
    annotations: Option[BufferRenderAnnotations],
    dirtyRows: Option[Set[Int]]
  ): Unit =
    val buffer = pane.bufferId.flatMap(state.persisted.buffers.get)

    renderBufferHeader(pane, buffer, paneLayout, state, context, contract)
    renderEditorPaneVerticalSpacers(paneLayout, state, context)

    val contentRect = paneLayout.contentRect

    val bufferSnapshot =
      preparedSnapshot.orElse(buffer.map(RendererPaneSetup.snapshotForBuffer(_, contentRect, state, context)))
    val markdownLensFrame =
      for
        buf  <- buffer
        snap <- bufferSnapshot
        if RendererMarkdownLens.isInlineMarkdownLens(buf, state)
      yield RendererMarkdownLens.markdownLensFrameFor(buf, snap)

    buffer match
      case Some(buf) if buf.document.content.weight == 0 && buf.document.isNewEmpty =>
        RendererStartPage.renderWelcomeText(contentRect, state.persisted.theme, context)
      case Some(buf) if buf.document.content.weight == 0 =>
        RendererStartPage.renderEmptyPane(contentRect, state.persisted.theme, context)
      case Some(buf) =>
        bufferSnapshot.fold(RendererStartPage.renderEmptyPane(contentRect, state.persisted.theme, context)) { snap =>
          renderBufferContent(
            buf,
            contentRect,
            state,
            context,
            snap,
            markdownLensFrame,
            annotations.getOrElse(BufferRenderAnnotations(Map.empty, Map.empty)),
            dirtyRows
          )
        }
      case None =>
        RendererStartPage.renderEmptyPane(contentRect, state.persisted.theme, context)

    val cursorContext =
      if state.hasCommandRunnerDomain then context.copy(cursorVisible = true)
      else context
    (buffer, bufferSnapshot) match
      case (Some(buf), Some(snap)) =>
        if RendererMarkdownLens.isInlineMarkdownLens(buf, state) then
          RendererMarkdownLens.renderMarkdownLensCursors(
            buf,
            contentRect,
            state.persisted.theme,
            state.persisted.config,
            cursorContext,
            snap,
            markdownLensFrame.getOrElse(RendererMarkdownLens.markdownLensFrameFor(buf, snap))
          )
        else
          val _ = RendererCursorGlyphs.renderCursors(
            buf,
            contentRect,
            state.persisted.theme,
            state.persisted.config,
            cursorContext,
            snap
          )
      case _ => ()

  private def renderBufferHeader(
    pane: EditorPane,
    buffer: Option[Buffer],
    paneLayout: EditorPaneLayout,
    state: AppState,
    context: RenderContext,
    contract: EditorLayoutContract
  ): Unit =
    val surface    = context.surface
    val headerRect = contract.paneHeaderRect(pane.id).getOrElse(paneLayout.headerRect)
    if headerRect.height > 0 then
      context.surface.text.setFont(context.uiFont)
      val isActive  = state.persisted.layout.activeEditorPaneId.contains(pane.id)
      val titleRect = contract.paneTitleRect(pane.id).getOrElse(paneLayout.titleRect)

      if isActive then
        surface.setBackgroundColor(state.persisted.theme.highlighted.background)
        surface.setForegroundColor(state.persisted.theme.highlighted.foreground)
      else
        surface.setBackgroundColor(state.persisted.theme.panel.background)
        surface.setForegroundColor(state.persisted.theme.panel.foreground)

      val bufferTitleBase = buffer match
        case Some(buf) =>
          buf.document.filePath match
            case Some(path) =>
              val filename = path.getFileName.toString
              if buf.document.isDirty then s"$filename - unsaved" else filename
            case None =>
              if buf.document.isDirty then s"Buffer ${buf.id.value} - unsaved" else s"Buffer ${buf.id.value}"
        case None =>
          "No Buffer"
      val bufferTitle =
        if isActive then RendererGutter.applyModeTabWidgetToTopCorner(state, bufferTitleBase) else bufferTitleBase

      val maxTitleWidth = math.max(1, titleRect.width - 2)
      val displayTitle =
        if bufferTitle.length > maxTitleWidth then bufferTitle.take(maxTitleWidth - 3) + "..."
        else bufferTitle

      surface.putString(headerRect.x, headerRect.y, " " * headerRect.width)

      surface.text.fontRenderContext match
        case Some(frc) =>
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
            frc
          )
          surface.text.drawRunPx(
            titlePlacement.xPx,
            titlePlacement.yPx,
            titlePlacement.widthPx,
            titlePlacement.lineHeightPx,
            titlePlacement.ascentPx,
            displayTitle
          )
        case None =>
          val centerX = titleRect.x + math.max(0, (titleRect.width - displayTitle.length) / 2)
          CharacterRenderer.renderString(surface, centerX, titleRect.y, displayTitle)

    surface.setBackgroundColor(state.persisted.theme.background)
    surface.setForegroundColor(state.persisted.theme.foreground)

  private def renderEditorPaneVerticalSpacers(
    paneLayout: EditorPaneLayout,
    state: AppState,
    context: RenderContext
  ): Unit =
    context.surface.setBackgroundColor(state.persisted.theme.margin)
    List(paneLayout.topSpacerRect, paneLayout.bottomSpacerRect)
      .filter(rect => rect.width > 0 && rect.height > 0)
      .foreach(rect => context.surface.fillRect(rect.x, rect.y, rect.width, rect.height, ' '))
    context.surface.setBackgroundColor(state.persisted.theme.background)
    context.surface.setForegroundColor(state.persisted.theme.foreground)

  private def renderBufferContent(
    buffer: Buffer,
    rect: LayoutRect,
    state: AppState,
    context: RenderContext,
    snapshot: TextLayoutSnapshot,
    markdownLensFrame: Option[MarkdownLensFrame],
    annotations: BufferRenderAnnotations,
    dirtyRows: Option[Set[Int]]
  ): Unit =
    context.surface.text.setFont(context.fontForBuffer(buffer))
    if RendererMarkdownLens.isInlineMarkdownLens(buffer, state) then
      val frame = markdownLensFrame.getOrElse(RendererMarkdownLens.markdownLensFrameFor(buffer, snapshot))
      renderInlineMarkdownPreview(buffer, rect, state, context, frame)
      RendererMarkdownLens.renderMarkdownRawLenses(buffer, rect, state, context, snapshot, frame)
    else renderPlainBufferContent(buffer, rect, state, context, snapshot, annotations, dirtyRows)

  /** Draw the pane's visible rows.
    *
    * `dirtyRows` is the dirty-region contract: `None` draws every row, `Some(rows)` draws only those rows because the
    * surface still holds correct pixels for the rest (see [[RendererFramePlanner.planFrame]]).
    */
  private def renderPlainBufferContent(
    buffer: Buffer,
    rect: LayoutRect,
    state: AppState,
    context: RenderContext,
    snapshot: TextLayoutSnapshot,
    annotations: BufferRenderAnnotations,
    dirtyRows: Option[Set[Int]]
  ): Unit =
    val visualLines     = snapshot.visualLines
    val xOriginPx       = context.cellMetrics.toPixelX(rect.x).toFloat
    val contentRightXPx = context.cellMetrics.toPixelX(rect.right).toFloat
    val activeBodyLines = focusedTextBodyLines(buffer, state)
    val lexStartStates  = bufferLexStartStates(buffer)

    visualLines.zipWithIndex.foreach {
      case (visualLine, screenLineIndex) =>
        if dirtyRows.forall(_.contains(screenLineIndex)) &&
            visualLineFits(rect, screenLineIndex, context, snapshot)
        then
          val screenY   = rect.y + screenLineIndex
          val lineTopPx = visualLineTopPx(rect, screenLineIndex, context, snapshot)
          val screenX   = rect.x + visualLineCellOffset(visualLine, context)

          context.surface.setForegroundColor(state.persisted.theme.foreground)

          if visualLineVisible(rect, screenLineIndex, context, snapshot) &&
              screenY >= 0 &&
              screenX < context.surface.viewportWidth &&
              screenX >= 0 &&
              screenX < rect.right
          then
            val lineTheme      = state.persisted.theme
            val styledSegments = visualLineStyledSegments(visualLine, lineTheme, snapshot, activeBodyLines)
            val lexStartState =
              lexStartStates.lift(visualLine.bufferLine).getOrElse(com.serenity.ui.theme.LexState.Default)
            if RendererPaneSetup.usesMeasuredDrawing(snapshot, context) then
              CharacterRenderer.renderMeasuredLineWithAnimation(
                context.surface,
                xOriginPx,
                lineTopPx,
                snapshot.lineHeightPx,
                snapshot.ascentPx,
                visualLine,
                lineTheme,
                context.bufferAnimations.getOrElse(buffer.id, com.serenity.animation.AnimationState.empty),
                state.syntaxHighlightingEnabled,
                buffer.document.language,
                styledSegments,
                clipRightXPx = Some(contentRightXPx),
                lexStartState = lexStartState
              )
            else
              CharacterRenderer.renderStringWithAnimation(
                context.surface,
                screenX,
                screenY,
                visualLine.text,
                lineTheme,
                context.bufferAnimations.getOrElse(buffer.id, com.serenity.animation.AnimationState.empty),
                state.syntaxHighlightingEnabled,
                buffer.document.language,
                bufferLine = visualLine.bufferLine,
                bufferStartColumn = visualLine.startColumn,
                styledSegments = styledSegments,
                lexStartState = lexStartState,
                maxColumn = Some(rect.right)
              )

            RendererHighlights.renderDocumentCommentHighlights(
              context.surface,
              annotations.commentsByLine.getOrElse(visualLine.bufferLine, Nil),
              visualLine,
              rect,
              screenY,
              lineTopPx,
              state.persisted.theme,
              context,
              snapshot,
              styledSegments
            )

            RendererHighlights.renderDiagnosticHighlights(
              context.surface,
              annotations.diagnosticsByLine.getOrElse(visualLine.bufferLine, Nil),
              visualLine,
              rect,
              screenY,
              lineTopPx,
              state.persisted.theme,
              context,
              snapshot,
              styledSegments
            )

            RendererHighlights.renderSelectionHighlights(
              context.surface,
              buffer,
              visualLine,
              rect,
              screenY,
              lineTopPx,
              state.persisted.theme,
              context,
              snapshot,
              styledSegments
            )

            val stringEnd = visualLine.startColumn + visualLine.text.length
            val lineAnims = context.bufferAnimations
              .getOrElse(buffer.id, com.serenity.animation.AnimationState.empty)
              .getLineAnimations(visualLine.bufferLine)
            lineAnims.foreach { (col, cell) =>
              cell.currentBackground.foreach { bg =>
                if col >= stringEnd then
                  val bgScreenX = rect.x + visualLineCellOffset(visualLine, context) + (col - visualLine.startColumn)
                  if bgScreenX >= 0 && bgScreenX < rect.right then
                    context.surface.setForegroundColor(state.persisted.theme.foreground)
                    context.surface.setBackgroundColor(bg)
                    context.surface.putString(bgScreenX, screenY, " ")
              }
            }
    }

  def visualLineFits(
    rect: LayoutRect,
    screenLineIndex: Int,
    context: RenderContext,
    snapshot: TextLayoutSnapshot
  ): Boolean =
    textRowMetrics(rect, context, snapshot).lineFits(screenLineIndex)

  def visualLineVisible(
    rect: LayoutRect,
    screenLineIndex: Int,
    context: RenderContext,
    snapshot: TextLayoutSnapshot
  ): Boolean =
    textRowMetrics(rect, context, snapshot).lineVisible(screenLineIndex, context.surface.viewportHeight)

  def visualLineTopPx(
    rect: LayoutRect,
    screenLineIndex: Int,
    context: RenderContext,
    snapshot: TextLayoutSnapshot
  ): Int =
    textRowMetrics(rect, context, snapshot).lineTopPx(screenLineIndex)

  def textRowMetrics(
    rect: LayoutRect,
    context: RenderContext,
    snapshot: TextLayoutSnapshot
  ): TextRowMetrics =
    TextRowMetrics(
      contentRect = rect,
      gridMetrics = context.cellMetrics,
      rowLineHeightPx = snapshot.lineHeightPx,
      usesMeasuredLayout = RendererPaneSetup.usesMeasuredDrawing(snapshot, context)
    )

  def visualLineCellOffset(visualLine: TextVisualLine, context: RenderContext): Int =
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

  /** Lexical state at the start of each buffer line, needed so an open block comment or triple-quoted string keeps
    * coloring correctly on the lines after it opened. Cheap for languages without token-aware highlighting (a single
    * `Default` per line, no document scan); for Scala it's recomputed incrementally by [[ThemeManager]], keyed by
    * buffer id, so an edit only re-scans from the changed line forward.
    */
  private def bufferLexStartStates(buffer: Buffer): Vector[com.serenity.ui.theme.LexState] =
    if !buffer.document.language.contains(LanguageId.Scala) then Vector.empty
    else
      val rope = buffer.document.content
      ThemeManager.lineStartStates(
        buffer.id.value.toString,
        rope.linesFrom(0, rope.lineCount),
        buffer.document.language
      )

  private def visualLineStyledSegments(
    visualLine: TextVisualLine,
    theme: Theme,
    snapshot: TextLayoutSnapshot,
    activeBodyLine: Int => Boolean
  ): Option[List[StyledText]] =
    val richSegments = richTextStyledSegments(visualLine, theme, snapshot)
    if activeBodyLine(visualLine.bufferLine) then richSegments
    else
      val baseSegments =
        richSegments.getOrElse(List(StyledText(visualLine.text, TextStyle.normal, theme.foreground, theme.background)))
      Some(baseSegments.map(segment => segment.copy(foregroundColor = theme.muted, backgroundColor = theme.background)))

  private def focusedTextBodyLines(buffer: Buffer, state: AppState): Int => Boolean =
    if !state.persisted.config.surfaceConfig.focusedTextBodyEnabled then _ => true
    else
      val activeLine = buffer.editing.cursors.headOption.map(_.line)
      FocusedTextBody
        .activeRange(buffer, activeLine)
        .map((range: Range.Inclusive) => (line: Int) => range.contains(line))
        .getOrElse((_: Int) => true)

  private def renderInlineMarkdownPreview(
    buffer: Buffer,
    rect: LayoutRect,
    state: AppState,
    context: RenderContext,
    frame: MarkdownLensFrame
  ): Unit =
    val widthPx = RendererMarkdownLens.scaledImagePixelDimension(
      rect.width * context.cellMetrics.charWidth,
      context.surface.devicePixelScaleX
    )
    val heightPx = RendererMarkdownLens.scaledImagePixelDimension(
      rect.height * context.cellMetrics.lineHeight,
      context.surface.devicePixelScaleY
    )
    val previewFont = MarkdownDocumentPreview.inlineLensFont(
      context.textFont,
      context.cellMetrics.lineHeight,
      context.surface.devicePixelScaleY
    )
    val title = buffer.document.filePath.flatMap(path => Option(path.getFileName).map(_.toString)).getOrElse("Untitled")
    val image = MarkdownDocumentPreview.renderInlineRowsImage(
      rows = frame.previewRows,
      sourceLines = frame.lines,
      title = title,
      widthPx = widthPx,
      heightPx = heightPx,
      theme = state.persisted.theme,
      font = previewFont,
      inlineLineHeightPx = MarkdownDocumentPreview.lineHeightForDeviceScale(
        context.cellMetrics.lineHeight,
        context.surface.devicePixelScaleY
      ),
      reuseLastRenderWhileEditing = buffer.markdownPreviewEditGeneration != buffer.markdownPreviewCommittedGeneration
    )
    context.surface.pixels.drawImage(image, rect.x, rect.y, rect.width, rect.height)
