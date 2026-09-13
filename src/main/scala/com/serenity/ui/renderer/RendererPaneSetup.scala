package com.serenity.ui.renderer

import java.awt.Font

import com.serenity.state.models.*
import com.serenity.ui.layout.*

/** Geometry for one editor pane's rendered content: which buffer line each visual row shows, plus per-buffer
  * comment/diagnostic annotations for the visible lines.
  */
final case class EditorPaneRenderPlan(
    workspaceLayout: EditorWorkspaceLayout,
    layoutContract: EditorLayoutContract,
    snapshots: Map[PaneId, TextLayoutSnapshot],
    annotations: Map[BufferId, BufferRenderAnnotations]
):
  def paneLayouts: Map[PaneId, EditorPaneLayout] = workspaceLayout.paneLayouts

/** Per-pane geometry for this frame: which buffer line each visual row shows, to translate `Damage`'s buffer-line facts
  * into row indices, and the pixel band each row owns, to know what a preserved row's pixels actually cover.
  */
final case class PaneFrameRecord(
    bufferId: BufferId,
    rowBufferLines: Vector[Int],
    rowRects: Vector[PixelRect],
    overflowingRows: Set[Int],
    snapshot: TextLayoutSnapshot
)

final case class BufferRenderAnnotations(
    commentsByLine: Map[Int, List[DocumentComment]],
    diagnosticsByLine: Map[Int, List[com.serenity.lsp.model.Diagnostic]],
    // See `SemanticTokensAvailability` for what each case means -- `Pending` (a request may still be in flight) is
    // deliberately distinct from `Unavailable` (confirmed no server/capability). AppState.semanticTokensIndexByBuffer
    // is where this is derived.
    semanticTokensAvailability: SemanticTokensAvailability
)

/** Answers "what does each pane's content look like this frame": the per-frame [[EditorPaneRenderPlan]] (buffer layout
  * snapshots + visible annotations), the buffer text layout snapshot each pane paints from, and the [[PaneFrameRecord]]
  * geometry [[RendererFramePlanner]] plans row reuse against. Upstream of the actual paint calls in
  * [[RendererPaneContent]].
  */
object RendererPaneSetup:

  def renderSpacerColumns(state: AppState, context: RenderContext, contract: EditorLayoutContract): Unit =
    val surface = context.surface
    surface.setBackgroundColor(state.persisted.theme.margin)
    List(contract.leftSpacerRect, contract.rightSpacerRect)
      .filter(rect => rect.width > 0 && rect.height > 0)
      .foreach(rect => surface.fillRect(rect.x, rect.y, rect.width, rect.height, ' '))

  def prepareEditorPaneRenderPlan(
    state: AppState,
    context: RenderContext,
    scene: UiSceneSnapshot
  ): EditorPaneRenderPlan =
    val layoutContract  = scene.editorContract
    val workspaceLayout = layoutContract.workspace
    val snapshots =
      state.persisted.layout.editorPanes.flatMap {
        case (paneId, pane) =>
          for
            paneLayout <- workspaceLayout.paneLayouts.get(paneId)
            bufferId   <- pane.bufferId
            buffer     <- state.persisted.buffers.get(bufferId)
          yield paneId -> scene
            .textSnapshot(paneId)
            .getOrElse(
              snapshotForBuffer(buffer, paneLayout.contentRect, state, context)
            )
      }

    val visibleLinesByBuffer = state.persisted.layout.editorPanes.toList
      .flatMap {
        case (paneId, pane) =>
          for
            bufferId <- pane.bufferId
            snapshot <- snapshots.get(paneId)
          yield bufferId -> snapshot.visualLines.map(_.bufferLine).toSet
      }
      .groupMap(_._1)(_._2)
      .view
      .mapValues(_.foldLeft(Set.empty[Int])(_ ++ _))
      .toMap

    val annotations = state.persisted.layout.editorPanes.values
      .flatMap(_.bufferId)
      .toList
      .distinct
      .flatMap { bufferId =>
        state.persisted.buffers.get(bufferId).map { _ =>
          val visibleLines = visibleLinesByBuffer.getOrElse(bufferId, Set.empty)
          val cached =
            state.annotationIndexByBuffer.get(bufferId).map(_()).getOrElse(AnnotationLineIndex(Vector.empty, Map.empty))
          val commentsByLine    = cached.commentsByLine(visibleLines)
          val diagnosticsByLine = visibleAnnotationLines(visibleLines, cached.diagnosticsByLine)
          val semanticTokensAvailability =
            state.semanticTokensIndexByBuffer.get(bufferId).map(_()).getOrElse(SemanticTokensAvailability.Pending)
          bufferId -> BufferRenderAnnotations(commentsByLine, diagnosticsByLine, semanticTokensAvailability)
        }
      }
      .toMap

    EditorPaneRenderPlan(workspaceLayout, layoutContract, snapshots, annotations)

  def visibleAnnotationLines[A](
    visibleLines: Set[Int],
    indexed: Map[Int, List[A]]
  ): Map[Int, List[A]] =
    visibleLines.iterator.flatMap(line => indexed.get(line).map(line -> _)).toMap

  /** Whether buffer text should actually be painted through the measured pixel-run path.
    *
    * `snapshot.usesMeasuredLayout` is a font-only judgement (proportional advances, ligatures, ...) computed once when
    * the pane's [[TextLayoutSnapshot]] is built -- including by [[com.serenity.state.manager.AuthoritativeUiScene]],
    * which has no surface to ask and always uses it as a scene-wide, surface-agnostic cache shared with mouse
    * targeting. `drawRunPx` is a no-op on a surface reporting no `FontRenderContext` (a terminal), so painting must
    * additionally require the surface's own capability -- #1105.
    */
  def usesMeasuredDrawing(snapshot: TextLayoutSnapshot, context: RenderContext): Boolean =
    snapshot.usesMeasuredLayout && context.surface.text.fontRenderContext.nonEmpty

  def snapshotForBuffer(
    buffer: Buffer,
    contentRect: LayoutRect,
    state: AppState,
    context: RenderContext
  ): TextLayoutSnapshot =
    // #1105: a surface reporting no FontRenderContext (a terminal) can't draw a measured pixel run at all -- drawRunPx
    // is a no-op there -- so a buffer that would otherwise pick a proportional text/ui font is forced onto the code
    // font instead, and the resulting snapshot is forced onto the cell path below, regardless of what the font itself
    // would have decided (ligatures, non-monospace advances, ...).
    val hasFontRenderContext = context.surface.text.fontRenderContext.nonEmpty
    val bufferFont           = if hasFontRenderContext then context.fontForBuffer(buffer) else context.codeFont
    val panelWidthPx         = contentRect.width * context.cellMetrics.charWidth
    val panelHeightPx        = contentRect.height * context.cellMetrics.lineHeight
    val bufferMetrics        = CellMetrics.fromFont(bufferFont)
    val baseViewport =
      LayoutEngine.updateBufferViewportDimensions(
        buffer,
        contentRect,
        state.persisted.config.surfaceConfig.wordWrapEnabled
      )
    val fontRenderContext =
      context.surface.text.fontRenderContext.getOrElse(TextLayoutSnapshot.defaultFontRenderContext())
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
      else renderedLeftColumn(buffer, scrollViewport, state.persisted.config.surfaceConfig.wordWrapEnabled)
    val renderedViewport = sizedViewport.copy(
      leftColumn = leftColumn
    )
    val renderBuffer = buffer.copy(
      viewport = renderedViewport
    )
    context.surface.text.setFont(bufferFont)
    // A surface with a real FontRenderContext keeps deriving cell-based advances from the buffer's own font, same as
    // always. A surface with none (a terminal, #1105) has no real font metrics to derive from at all -- its own
    // declared `context.cellMetrics` (TUI's CellMetricsOne, 1 pixel == 1 terminal cell) is the only legitimate scale,
    // so this is what must reach the cell-based layout path instead of `TextLayoutSnapshot` re-deriving
    // `CellMetrics.fromFont(bufferFont)` (a real AWT measurement of a font this surface never actually draws) (#1215).
    val snapshot = TextLayoutSnapshot.fromBuffer(
      renderBuffer,
      panelWidthPx,
      bufferFont,
      fontRenderContext,
      wordWrapEnabled = state.persisted.config.surfaceConfig.wordWrapEnabled,
      cellMetricsOverride = if hasFontRenderContext then Some(bufferMetrics) else Some(context.cellMetrics),
      // #1215: forces the cell path during construction (not just the flag below) -- otherwise a "monospaced" font
      // whose measured-vs-cell auto-detection trips on this environment's own font-rendering quirks would still bake
      // real (and here, meaningless -- #1105) measured advances into the snapshot, discarding `context.cellMetrics`.
      forceCellLayout = !hasFontRenderContext
    )
    if hasFontRenderContext then snapshot else snapshot.copy(usesMeasuredLayout = false)

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
      val cursor         = buffer.editing.cursors.headOption.getOrElse(CursorPosition(viewport.topLine, 0))
      val cursorColumn   = cursor.column.max(0)
      val lineLength     = buffer.document.content.getLine(cursor.line).map(_.length).getOrElse(cursorColumn)
      val maxForCursor   = math.max(0, cursorColumn - visibleColumns + 1)
      val maxForLine     = math.max(0, lineLength - visibleColumns + 1)

      viewport.leftColumn.max(0).min(maxForCursor).min(maxForLine)

  /** Geometry for the panes drawn by [[RendererPaneContent.renderPlainBufferContent]]: which buffer line each visual
    * row shows (to translate `Damage`'s buffer-line facts into row indices) and the pixel band each row owns (to know
    * what a preserved row's pixels actually cover).
    *
    * Panes on any other path — the welcome text, the empty-pane filler, the inline markdown lens, which paint over the
    * whole content rect from inputs this record does not track — are left out, so they always redraw in full.
    */
  def paneRecordsFor(
    state: AppState,
    context: RenderContext,
    renderPlan: EditorPaneRenderPlan
  ): Map[PaneId, PaneFrameRecord] =
    state.persisted.layout.editorPanes.flatMap {
      case (paneId, pane) =>
        for
          paneLayout <- renderPlan.paneLayouts.get(paneId)
          bufferId   <- pane.bufferId
          buffer     <- state.persisted.buffers.get(bufferId)
          snapshot   <- renderPlan.snapshots.get(paneId)
          if buffer.document.content.weight > 0 && !RendererMarkdownLens.isInlineMarkdownLens(buffer, state)
        yield paneId -> paneFrameRecord(bufferId, paneLayout.contentRect, context, snapshot)
    }.toMap

  private def paneFrameRecord(
    bufferId: BufferId,
    contentRect: LayoutRect,
    context: RenderContext,
    snapshot: TextLayoutSnapshot
  ): PaneFrameRecord =
    PaneFrameRecord(
      bufferId = bufferId,
      rowBufferLines = snapshot.visualLines.map(_.bufferLine),
      rowRects = paneRowRects(contentRect, context, snapshot),
      overflowingRows = overflowingRows(contentRect, context, snapshot),
      snapshot = snapshot
    )

  /** Rows whose run reaches past the pane's content rect.
    *
    * Unwrapped layout shapes a couple of columns of overscan and glyphs are clipped to the whole surface rather than to
    * the pane, so such a row paints pixels outside the band that could be preserved for it. Those rows are always
    * redrawn rather than reasoned about.
    */
  private def overflowingRows(
    contentRect: LayoutRect,
    context: RenderContext,
    snapshot: TextLayoutSnapshot
  ): Set[Int] =
    val contentWidthPx = (contentRect.right - contentRect.x) * context.cellMetrics.charWidth
    snapshot.visualLines.zipWithIndex.collect {
      case (visualLine, row) if visualLine.xOffsetPx + visualLine.widthPx > contentWidthPx.toFloat => row
    }.toSet

  /** The pixel band each visible row owns, clamped to the pane's content rect. */
  private def paneRowRects(
    contentRect: LayoutRect,
    context: RenderContext,
    snapshot: TextLayoutSnapshot
  ): Vector[PixelRect] =
    val rowMetrics = RendererPaneContent.textRowMetrics(contentRect, context, snapshot)
    val leftPx     = context.cellMetrics.toPixelX(contentRect.x)
    val widthPx    = context.cellMetrics.toPixelX(contentRect.right) - leftPx
    val heightPx =
      if usesMeasuredDrawing(snapshot, context) then snapshot.lineHeightPx else context.cellMetrics.lineHeight
    val topLimitPx    = context.cellMetrics.toPixelY(contentRect.y)
    val bottomLimitPx = rowMetrics.contentBottomPx
    snapshot.visualLines.indices.toVector.map { row =>
      val topPx    = rowMetrics.lineTopPx(row).max(topLimitPx)
      val bottomPx = (rowMetrics.lineTopPx(row) + heightPx).min(bottomLimitPx)
      PixelRect(leftPx, topPx, widthPx.max(0), (bottomPx - topPx).max(0))
    }
