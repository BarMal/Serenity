package com.serenity.ui.layout

import java.awt.Font
import java.awt.font.FontRenderContext

import com.serenity.state.models.{Buffer, TextVisualLine}
import com.serenity.ui.fonts.FontLoader

/** Column-based document layout (issue #1338, Phase 1): the two entry points column-mode rendering needs on top of
  * [[TextLayoutSnapshot.fromBuffer]]'s ordinary single-window slice. Split out of `TextLayoutSnapshot` itself once
  * adding these pushed that file past the architecture ratchet's file-length target (following `LayoutEngine`'s own
  * `export`-from-a-sibling-object precedent); [[TextLayoutSnapshot]] exports both so `TextLayoutSnapshot.foo` call
  * sites are unaffected by where the code actually lives.
  */
object TextLayoutSnapshotColumnMode:

  import TextLayoutSnapshot.{collectVisualLines, defaultFontRenderContext, shouldUseMeasuredLayout}

  /** The same wrapped visual-line stream `fromBuffer` slices into one screenful, partitioned instead into `columnCount`
    * groups of `buffer.viewport.visibleLines` rows each -- one per column -- starting from the viewport's own
    * `topLine`/`topVisualLine` (already snapped to a column boundary by `CursorViewport.adjustForCursorColumnMode`).
    * Reuses the same wrap logic (`collectVisualLines`) unchanged; a document that runs out of lines before filling
    * every column simply yields a shorter final chunk.
    */
  def columnChunksForBuffer(
    buffer: Buffer,
    columnWidthPx: Int,
    font: Font,
    fontRenderContext: FontRenderContext = defaultFontRenderContext(),
    cellMetricsOverride: Option[CellMetrics] = None,
    forceCellLayout: Boolean = false,
    proseScale: Float = 1.0f,
    columnCount: Int = 1
  ): Vector[Vector[TextVisualLine]] =
    val cellMetrics    = cellMetricsOverride.getOrElse(CellMetrics.fromFont(font))
    val measuredLayout = !forceCellLayout && shouldUseMeasuredLayout(font, fontRenderContext)
    val totalLines     = buffer.document.content.lineCount
    val richDocument =
      buffer.richText.richTextDocument.filter(_.matchesPlainTextShape(totalLines, buffer.document.content.weight))
    val visibleLines    = math.max(1, buffer.viewport.visibleLines)
    val topVisualLine   = buffer.viewport.topVisualLine
    val totalRowsNeeded = visibleLines * math.max(1, columnCount)
    val visualLineLimit = topVisualLine + totalRowsNeeded
    val allVisualLines = collectVisualLines(
      buffer,
      totalLines,
      math.max(1, columnWidthPx),
      font,
      fontRenderContext,
      measuredLayout,
      cellMetrics,
      visualLineLimit,
      richDocument,
      wordWrapEnabled = true,
      proseScale
    ).drop(topVisualLine).take(totalRowsNeeded)

    allVisualLines.grouped(visibleLines).toVector

  /** The snapshot for the ONE column currently showing -- `columnChunksForBuffer`'s first chunk (the viewport's own
    * `topLine`/`topVisualLine` is always already snapped to a column boundary by
    * `CursorViewport.adjustForCursorColumnMode`, so that first chunk is always the active column), wrapped as an
    * ordinary [[TextLayoutSnapshot]] so every downstream consumer (painting, cursor placement) needs no column-mode
    * branch of its own. `panelWidthPx` is the column's own (narrower) width, not the pane's full width -- the renderer
    * paints this one column's content starting at the pane's left edge, occupying only as much of the pane horizontally
    * as the column itself is wide (see `RendererPaneSetup.snapshotForBuffer`'s doc comment for why
    * centring/multi-column-at-once is out of scope for this phase).
    */
  def fromBufferColumn(
    buffer: Buffer,
    columnWidthPx: Int,
    font: Font,
    fontRenderContext: FontRenderContext = defaultFontRenderContext(),
    cellMetricsOverride: Option[CellMetrics] = None,
    forceCellLayout: Boolean = false,
    proseScale: Float = 1.0f
  ): TextLayoutSnapshot =
    val cellMetrics    = cellMetricsOverride.getOrElse(CellMetrics.fromFont(font))
    val measuredLayout = !forceCellLayout && shouldUseMeasuredLayout(font, fontRenderContext)
    val lineHeightPx =
      if measuredLayout then
        math.max(1, math.ceil(font.getLineMetrics("Mg", fontRenderContext).getHeight.toDouble).toInt)
      else math.max(1, cellMetrics.lineHeight)
    val ascentPx =
      if measuredLayout then
        math.max(1, math.ceil(font.getLineMetrics("Mg", fontRenderContext).getAscent.toDouble).toInt)
      else math.max(0, cellMetrics.ascent)
    val totalLines = buffer.document.content.lineCount
    val richDocument =
      buffer.richText.richTextDocument.filter(_.matchesPlainTextShape(totalLines, buffer.document.content.weight))
    val visualLines = columnChunksForBuffer(
      buffer,
      columnWidthPx,
      font,
      fontRenderContext,
      cellMetricsOverride,
      forceCellLayout,
      proseScale,
      columnCount = 1
    ).headOption.getOrElse(Vector.empty)

    TextLayoutSnapshot(
      visualLines = visualLines,
      panelWidthPx = math.max(1, columnWidthPx),
      lineHeightPx = lineHeightPx,
      ascentPx = ascentPx,
      isProportional = !FontLoader.isMonospacedFont(font),
      usesMeasuredLayout = measuredLayout,
      richTextDocument = richDocument,
      proseScale = proseScale
    )
