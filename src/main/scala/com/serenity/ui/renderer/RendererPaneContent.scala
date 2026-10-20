package com.serenity.ui.renderer

import com.serenity.state.manager.FocusedTextBody
import com.serenity.state.models.*
import com.serenity.ui.layout.*

/** Paints each editor pane's own content: the visible text rows (plain or the inline markdown-lens variant), with the
  * header and spacers delegated to [[RendererPaneChrome]] when the frame repaints them.
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
    dirtyRowsByPane: Map[PaneId, Set[Int]],
    chrome: ChromeRepaint
  ): Unit =
    val activePaneId = state.persisted.layout.activeEditorPaneId
    val orderedPanes =
      state.persisted.layout.orderedPaneIds
        .flatMap(paneId => state.persisted.layout.editorPanes.get(paneId).map(paneId -> _))
        .sortBy((paneId, _) => if activePaneId.contains(paneId) then 1 else 0)

    orderedPanes.foreach { (paneId, pane) =>
      renderPlan.paneLayouts.get(paneId) match
        case Some(paneLayout) =>
          aboveBottomOf(paneLayout.contentRect, context) {
            renderEditorPane(
              pane,
              paneLayout,
              state,
              context,
              renderPlan.snapshots.get(paneId),
              renderPlan.columnSnapshotsFor(paneId),
              renderPlan.layoutContract,
              pane.bufferId.flatMap(renderPlan.annotations.get),
              dirtyRowsByPane.get(paneId),
              chrome
            )
          }
        case None => ()
    }

  /** A last row that starts inside the pane but ends past it is cut off at the pane's bottom edge. Left unclipped its
    * glyphs land on the status row, which a frame that repaints chrome clears and one that does not leaves covered, so
    * the same document drew two ways depending on the frame before. Only the bottom is bounded: glyph overscan to the
    * sides is deliberate.
    */
  private def aboveBottomOf(contentRect: LayoutRect, context: RenderContext)(paint: => Unit): Unit =
    context.surface.panelOutlines match
      case Some(clipping) if contentRect.bottom < context.surface.viewportHeight =>
        clipping.withRectClip(0, 0, context.surface.viewportWidth, contentRect.bottom)(paint)
      case _ => paint

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
        val columnPlacements = renderPlan.columnSnapshotsFor(paneId)
        (for
          paneLayout <- renderPlan.paneLayouts.get(paneId)
          bufferId   <- pane.bufferId
          buffer     <- state.persisted.buffers.get(bufferId)
        yield
          if columnPlacements.nonEmpty then
            // Multi-column e-reader layout (issue #1338, Phase 2 / slice 1): the caret paints against the one page
            // column that holds it, at that column's own x-shifted rect.
            columnPlacements.flatMap { placement =>
              RendererCursorGlyphs.renderCursors(
                buffer,
                columnRect(paneLayout.contentRect, placement),
                state.persisted.theme,
                state.persisted.config,
                context,
                placement.snapshot,
                paintsCursor = RendererCursorGlyphs.owningColumn(columnPlacements, _).exists(_ eq placement)
              )
            }.toList
          else
            renderPlan.snapshots
              .get(paneId)
              .map(snapshot =>
                RendererCursorGlyphs.renderCursors(
                  buffer,
                  paneLayout.contentRect,
                  state.persisted.theme,
                  state.persisted.config,
                  context,
                  snapshot
                )
              )
              .getOrElse(Nil)
        ).getOrElse(Nil)
    }

  private def renderEditorPane(
    pane: EditorPane,
    paneLayout: EditorPaneLayout,
    state: AppState,
    context: RenderContext,
    preparedSnapshot: Option[TextLayoutSnapshot],
    columnPlacements: Vector[ColumnSnapshotPlacement],
    contract: EditorLayoutContract,
    annotations: Option[BufferRenderAnnotations],
    dirtyRows: Option[Set[Int]],
    chrome: ChromeRepaint
  ): Unit =
    val buffer = pane.bufferId.flatMap(state.persisted.buffers.get)

    context.surface.text.setFont(FontSpec.fromAwt(context.uiFont))
    if chrome.headers.includes(pane.id) then
      RendererPaneChrome.renderHeader(pane, buffer, paneLayout, state, context, contract)
    if chrome.spacers then RendererPaneChrome.renderVerticalSpacers(paneLayout, state, context)
    context.surface.setBackgroundColor(state.persisted.theme.background)
    context.surface.setForegroundColor(state.persisted.theme.foreground)

    val contentRect = paneLayout.contentRect

    val bufferSnapshot =
      preparedSnapshot.orElse(buffer.map(RendererPaneSetup.snapshotForBuffer(_, contentRect, state, context)))
    // Multi-column e-reader layout (issue #1338, Phase 2 / slice 1): a column-mode pane paints each column's own
    // snapshot at its own x-origin (`contentRect.x + placement.xOffsetCells`), so the page reads as columns side by
    // side. `dirtyRows` numbers the rows across the columns ([[ColumnPaneRows]]); each column draws its share.
    val paintsColumns =
      columnPlacements.nonEmpty && buffer.exists(_.document.content.weight > 0)

    buffer match
      case Some(buf) if buf.document.content.weight == 0 && buf.document.isNewEmpty =>
        RendererStartPage.renderWelcomeText(contentRect, state.persisted.theme, context, state.persisted.config)
      case Some(buf) if buf.document.content.weight == 0 =>
        RendererStartPage.renderEmptyPane(contentRect, state.persisted.theme, context)
      case Some(buf) if paintsColumns =>
        val bufferAnnotations =
          annotations.getOrElse(BufferRenderAnnotations(Map.empty, Map.empty, SemanticTokensAvailability.Pending))
        columnPlacements.zip(ColumnPaneRows.localRows(columnPlacements, dirtyRows)).foreach {
          (placement, columnDirtyRows) =>
            renderBufferContent(
              buf,
              columnRect(contentRect, placement),
              state,
              context,
              placement.snapshot,
              bufferAnnotations,
              columnDirtyRows
            )
        }
      case Some(buf) =>
        bufferSnapshot.fold(RendererStartPage.renderEmptyPane(contentRect, state.persisted.theme, context)) { snap =>
          renderBufferContent(
            buf,
            contentRect,
            state,
            context,
            snap,
            annotations.getOrElse(BufferRenderAnnotations(Map.empty, Map.empty, SemanticTokensAvailability.Pending)),
            dirtyRows
          )
        }
      case None =>
        RendererStartPage.renderEmptyPane(contentRect, state.persisted.theme, context)

    val cursorContext =
      if state.hasCommandRunnerDomain then context.copy(cursorVisible = true)
      else context
    (buffer, bufferSnapshot) match
      case (Some(buf), _) if paintsColumns =>
        // Each column paints only the cursors whose buffer position falls on one of its own visual rows; the others
        // resolve to no visual row in that column's snapshot and paint nothing. So the caret lands in whichever page
        // column actually holds it.
        columnPlacements.foreach { placement =>
          val _ = RendererCursorGlyphs.renderCursors(
            buf,
            columnRect(contentRect, placement),
            state.persisted.theme,
            state.persisted.config,
            cursorContext,
            placement.snapshot
          )
        }
      case (Some(buf), Some(snap)) =>
        val _ = RendererCursorGlyphs.renderCursors(
          buf,
          contentRect,
          state.persisted.theme,
          state.persisted.config,
          cursorContext,
          snap
        )
      case _ => ()

  /** The x-shifted content rect a single page column's TEXT paints into: the pane's content rect moved right by the
    * column's own cell offset, past its line-number rail (`gutterWidthCells`, slice 2), and narrowed to the remaining
    * band -- so a column's text never paints over its own rail or past its neighbour.
    */
  def columnRect(contentRect: LayoutRect, placement: ColumnSnapshotPlacement): LayoutRect =
    val leftX     = contentRect.x + placement.xOffsetCells + placement.gutterWidthCells
    val bandWidth = math.max(0, placement.columnWidthCells - placement.gutterWidthCells)
    val width     = math.min(bandWidth, math.max(0, contentRect.right - leftX))
    contentRect.copy(x = leftX, width = math.max(0, width))

  private def renderBufferContent(
    buffer: Buffer,
    rect: LayoutRect,
    state: AppState,
    context: RenderContext,
    snapshot: TextLayoutSnapshot,
    annotations: BufferRenderAnnotations,
    dirtyRows: Option[Set[Int]]
  ): Unit =
    context.surface.text.setFont(FontSpec.fromAwt(context.fontForBuffer(buffer)))
    renderPlainBufferContent(buffer, rect, state, context, snapshot, annotations, dirtyRows)

  /** Paints `buffer` into `rect` as plain, annotation-free text: what a docked preview shows of the document. */
  private[renderer] def renderReadOnlyText(
    buffer: Buffer,
    rect: LayoutRect,
    state: AppState,
    context: RenderContext,
    snapshot: TextLayoutSnapshot
  ): Unit =
    context.surface.text.setFont(FontSpec.fromAwt(context.fontForBuffer(buffer)))
    val noAnnotations = BufferRenderAnnotations(Map.empty, Map.empty, SemanticTokensAvailability.Pending)
    renderPlainBufferContent(buffer, rect, state, context, snapshot, noAnnotations, dirtyRows = None)

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
    val visualLines       = snapshot.visualLines
    val xOriginPx         = context.cellMetrics.toPixelX(rect.x).toFloat
    val contentRightXPx   = context.cellMetrics.toPixelX(rect.right).toFloat
    val activeBodyLines   = focusedTextBodyLines(buffer, state)
    val rowMetrics        = textRowMetrics(rect, context, snapshot)
    val rowsBottomPx      = rowMetrics.lineTopPx(visualLines.size).min(rowMetrics.contentBottomPx)
    val findMatchesByLine = RendererHighlights.findMatchesOnDrawnRows(buffer, state, visualLines, dirtyRows)
    FrameCounting.rowsPainted(context.caches.frameCounters, visualLines.size, dirtyRows)

    // A terminal has no fonts to fall back between; the GUI cell grid draws what the buffer's font lacks in the chain.
    val cellFallback =
      Option.when(context.surface.text.fontRenderContext.isDefined)(
        CellFallback(
          context.fontForBuffer(buffer),
          state.persisted.config.editorConfig.fontConfig.fallback,
          context.caches.wrappedLines
        )
      )

    visualLines.zipWithIndex.foreach {
      case (visualLine, screenLineIndex) =>
        if dirtyRows.forall(_.contains(screenLineIndex)) && rowMetrics.lineFits(screenLineIndex) then
          val screenY   = rowMetrics.screenRow(screenLineIndex)
          val lineTopPx = rowMetrics.lineTopPx(screenLineIndex)
          val screenX   = rect.x + visualLineCellOffset(visualLine, context)

          context.surface.setForegroundColor(state.persisted.theme.foreground)

          if rowMetrics.lineVisible(screenLineIndex, context.surface.viewportHeight) &&
              screenY >= 0 &&
              screenX < context.surface.viewportWidth &&
              screenX >= 0 &&
              screenX < rect.right
          then
            val lineTheme = state.persisted.theme
            // `Pending` renders as `Some(Nil)`, not `None` -- a request still in flight (or one not yet sent) must
            // not flash the muted "unavailable" style a confirmed `Unavailable` gets; it renders exactly like a
            // connected document whose visible lines just don't have tokens yet.
            val lineSemanticTokens = annotations.semanticTokensAvailability match
              case SemanticTokensAvailability.Available(byLine) => Some(byLine.getOrElse(visualLine.bufferLine, Nil))
              case SemanticTokensAvailability.Pending           => Some(Nil)
              case SemanticTokensAvailability.Unavailable       => None
            val styledSegments = RendererLineSegments.forLine(
              visualLine,
              lineTheme,
              snapshot,
              activeBodyLines,
              buffer,
              state,
              context,
              lineSemanticTokens
            )
            if RendererPaneSetup.usesMeasuredDrawing(snapshot, context) then
              DropCapRenderer.renderMeasuredHomeAware(
                snapshot,
                state,
                visualLine,
                styledSegments,
                context,
                buffer,
                xOriginPx,
                lineTopPx,
                rowHeightPxFor(visualLine, snapshot),
                rowAscentPxFor(visualLine, snapshot),
                contentRightXPx,
                lineSemanticTokens,
                rowsBottomPx
              )
            else
              CharacterRenderer.renderStyledString(
                context.surface,
                screenX,
                screenY,
                visualLine.text,
                lineTheme,
                state.syntaxHighlightingEnabled,
                buffer.document.language,
                styledSegments = styledSegments,
                semanticTokens = lineSemanticTokens,
                maxColumn = Some(rect.right),
                highlightCache = context.caches.themeHighlightCache,
                tabWidth = buffer.effectiveIndentation(state.persisted.config.indentation).tabSize,
                fallback = cellFallback
              )
              DropCapRenderer.paintCellHomeLineIfNeeded(snapshot, state, visualLine, context.surface, screenX, screenY)
              RendererHeadingRules.paintCells(
                context.surface,
                snapshot,
                visualLine,
                lineTheme,
                rect,
                screenY,
                dimmed = !activeBodyLines(visualLine.bufferLine)
              )

            RendererMarkdownRules.paint(
              context.surface,
              snapshot.markdownInline,
              visualLine,
              lineTheme,
              xOriginPx,
              contentRightXPx,
              lineTopPx,
              rowHeightPxFor(visualLine, snapshot)
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
              styledSegments,
              fallback = cellFallback
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
              styledSegments,
              dimmed = !activeBodyLines(visualLine.bufferLine),
              blendWeight = state.persisted.config.surfaceConfig.diagnosticHighlightBlendWeight,
              clipBottomPx = rowsBottomPx,
              fallback = cellFallback
            )

            RendererHighlights.renderKeywordHighlights(
              context.surface,
              annotations.keywordsByLine.getOrElse(visualLine.bufferLine, Nil),
              visualLine,
              rect,
              screenY,
              lineTopPx,
              state.persisted.theme,
              context,
              snapshot,
              styledSegments,
              fallback = cellFallback
            )

            RendererHighlights.renderFindMatchHighlights(
              context.surface,
              findMatchesByLine.getOrElse(visualLine.bufferLine, Nil),
              visualLine,
              rect,
              screenY,
              lineTopPx,
              state.persisted.theme,
              context,
              snapshot,
              styledSegments,
              fallback = cellFallback
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
              styledSegments,
              fallback = cellFallback
            )
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
    TextRowMetrics.forSnapshot(
      rect,
      context.cellMetrics,
      snapshot,
      measured = RendererPaneSetup.usesMeasuredDrawing(snapshot, context)
    )

  def visualLineCellOffset(visualLine: TextVisualLine, context: RenderContext): Int =
    if visualLine.xOffsetPx <= 0.0f then 0
    else math.round(visualLine.xOffsetPx / context.cellMetrics.charWidth.toFloat).max(0)

  /** A visual line's own measured height (the tallest run on it), or the snapshot's uniform height when unset (cell
    * layout, or a non-rich buffer). Shared by content, gutter, cursor, and highlight rendering so they agree per row.
    */
  def rowHeightPxFor(visualLine: TextVisualLine, snapshot: TextLayoutSnapshot): Int =
    if visualLine.heightPx > 0 then visualLine.heightPx else snapshot.lineHeightPx

  def rowAscentPxFor(visualLine: TextVisualLine, snapshot: TextLayoutSnapshot): Int =
    if visualLine.ascentPx > 0 then visualLine.ascentPx else snapshot.ascentPx

  private def focusedTextBodyLines(buffer: Buffer, state: AppState): Int => Boolean =
    if !state.persisted.config.surfaceConfig.focusedTextBodyEnabled then _ => true
    else
      val activeLine = buffer.editing.cursorPositions.headOption.map(_.line)
      FocusedTextBody
        .activeRange(buffer, activeLine)
        .map((range: Range.Inclusive) => (line: Int) => range.contains(line))
        .getOrElse((_: Int) => true)
