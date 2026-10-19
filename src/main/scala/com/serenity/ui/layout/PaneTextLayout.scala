package com.serenity.ui.layout

import java.awt.Font

import com.serenity.state.models.*

/** A pane's text as it is laid out for painting: the snapshot every non-column consumer reads, the per-column
  * placements of a multi-column page, and the viewport and metrics they were wrapped against.
  */
final case class PaneTextLayout(
    snapshot: TextLayoutSnapshot,
    columns: Vector[ColumnSnapshotPlacement],
    viewport: Viewport,
    widthPx: Int,
    font: Font,
    fontMetrics: CellMetrics,
    gridMetrics: CellMetrics,
    forceCellLayout: Boolean
)

object PaneTextLayout:

  /** The one place a pane's snapshot is built, shared by the scene that paints and hit-tests and by the layout that
    * anchors floating surfaces, so a surface and the caret it hangs off can never be laid out apart.
    *
    * Pane rects are measured in the screen grid's cells, and that grid is the code font's, whatever font a buffer draws
    * with. Sizing a document-font pane from its own cells makes the snapshot wrap at a width the pane does not have, so
    * its rows run off the right edge instead of wrapping.
    *
    * Column mode (issue #1338): the viewport narrows to one column's width and wraps at it whenever column mode and
    * word wrap are both on. Each column carries its own line-number rail on its left edge, so its TEXT wraps in the
    * band minus that rail while the band itself and the inter-column offset stay full-width.
    */
  def build(
    state: AppState,
    buffer: Buffer,
    contentRect: LayoutRect,
    codeFont: Font,
    textFont: Font,
    cellMetrics: Option[CellMetrics],
    wrappedLines: WrappedLineCache
  ): PaneTextLayout =
    val gridMetrics      = cellMetrics.getOrElse(CellMetrics.fromFont(codeFont))
    val surfaceConfig    = state.persisted.config.surfaceConfig
    val columnModeActive = surfaceConfig.columnModeEnabled && surfaceConfig.wordWrapEnabled
    val font             = if buffer.usesTextFont then textFont else codeFont
    val fontMetrics      = cellMetrics.getOrElse(CellMetrics.fromFont(font))
    val width            = contentRect.width * gridMetrics.charWidth
    val heightPx         = contentRect.height * gridMetrics.lineHeight
    val baseViewport = LayoutEngine
      .updateBufferViewportDimensions(
        buffer,
        contentRect,
        surfaceConfig.wordWrapEnabled,
        columnModeEnabled = surfaceConfig.columnModeEnabled,
        surfaceConfig
      )
    val visibleColumns =
      if surfaceConfig.wordWrapEnabled then baseViewport.visibleColumns
      else
        val averageAdvance = math.max(
          1.0f,
          TextLayoutSnapshot
            .caretXsForText(
              "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789",
              font,
              TextLayoutSnapshot.defaultFontRenderContext()
            )
            .lastOption
            .getOrElse(0.0f) / 62.0f
        )
        math
          .ceil(width.toDouble / averageAdvance.toDouble)
          .toInt
          .max(baseViewport.visibleColumns)
          .max(contentRect.width + 64)
    val cursorColumn =
      buffer.editing.cursorPositions.headOption.map(_.column).getOrElse(baseViewport.leftColumn)
    val leftColumn =
      if surfaceConfig.wordWrapEnabled then 0
      else baseViewport.leftColumn.max(0).max(cursorColumn - visibleColumns + 1)
    val viewport = baseViewport.copy(
      leftColumn = leftColumn,
      visibleColumns = visibleColumns,
      visibleLines = math.max(1, heightPx / math.max(1, fontMetrics.lineHeight))
    )
    val proseScale      = com.serenity.ui.theme.RichTextStyling.proseZoom(font.getSize2D)
    val forceCellLayout = cellMetrics.isDefined
    val config          = state.persisted.config
    if columnModeActive then
      val columnWidthCells     = math.max(1, viewport.visibleColumns)
      val gutterWidthCells     = LayoutEngine.perColumnGutterWidth(state)
      val columnTextWidthCells = math.max(1, columnWidthCells - gutterWidthCells)
      val columnTextWidthPx    = math.max(1, columnTextWidthCells * gridMetrics.charWidth)
      val columnCount          = LayoutEngine.resolvedColumnCount(contentRect.width, surfaceConfig)
      val columnSnapshotList = TextLayoutSnapshot.fromBufferColumns(
        buffer.copy(viewport = viewport),
        columnTextWidthPx,
        font,
        cellMetricsOverride = Some(fontMetrics),
        forceCellLayout = forceCellLayout,
        proseScale = proseScale,
        columnCount = columnCount,
        dropCapsEnabled = config.documentConfig.dropCapsEnabled,
        wrapCache = wrappedLines,
        tabSize = buffer.effectiveIndentation(config.indentation).tabSize,
        headings = state.effectiveHeadingSettings,
        openings = buffer.effectiveOpenings(config.openingsConfig),
        dropCapGapEm = config.documentConfig.dropCapGapEm
      )
      // Only a genuinely multi-column page carries per-column placements. A single fitted column is fully served by
      // the active snapshot alone (identical to the pre-multi-column render path, including its column-transition
      // animation overlay), so emitting a lone placement would only make the renderer take its multi-column branch for
      // a page that has nothing to lay out side by side.
      val placements =
        if columnCount <= 1 then Vector.empty[ColumnSnapshotPlacement]
        else
          columnSnapshotList.zipWithIndex.map {
            case (snapshot, columnIndex) =>
              ColumnSnapshotPlacement(
                columnIndex = columnIndex,
                xOffsetCells = columnIndex * (columnWidthCells + math.max(0, surfaceConfig.columnGap)),
                columnWidthCells = columnWidthCells,
                snapshot = snapshot,
                gutterWidthCells = gutterWidthCells
              )
          }
      // The single active-column snapshot every non-column consumer still reads is the page's first column (index 0)
      // -- the cursor's own column, since the viewport was snapped to the page holding it.
      val activeSnapshot = placements.headOption
        .map(_.snapshot)
        .getOrElse(
          TextLayoutSnapshot.fromBufferColumn(
            buffer.copy(viewport = viewport),
            columnTextWidthPx,
            font,
            cellMetricsOverride = Some(fontMetrics),
            forceCellLayout = forceCellLayout,
            proseScale = proseScale,
            dropCapsEnabled = config.documentConfig.dropCapsEnabled,
            wrapCache = wrappedLines,
            tabSize = buffer.effectiveIndentation(config.indentation).tabSize,
            headings = state.effectiveHeadingSettings,
            openings = buffer.effectiveOpenings(config.openingsConfig),
            dropCapGapEm = config.documentConfig.dropCapGapEm
          )
        )
      PaneTextLayout(activeSnapshot, placements, viewport, width, font, fontMetrics, gridMetrics, forceCellLayout)
    else
      val single = TextLayoutSnapshot.fromBuffer(
        buffer.copy(viewport = viewport),
        width,
        font,
        wordWrapEnabled = surfaceConfig.wordWrapEnabled,
        cellMetricsOverride = Some(fontMetrics),
        forceCellLayout = forceCellLayout,
        // Match the render path's prose zoom so hit-testing rows/advances line up with what was drawn.
        proseScale = proseScale,
        dropCapsEnabled = config.documentConfig.dropCapsEnabled,
        wrapCache = wrappedLines,
        markdownViewMode = config.inlineMarkdownViewMode,
        tabSize = buffer.effectiveIndentation(config.indentation).tabSize,
        headings = state.effectiveHeadingSettings,
        openings = buffer.effectiveOpenings(config.openingsConfig),
        dropCapGapEm = config.documentConfig.dropCapGapEm
      )
      PaneTextLayout(single, Vector.empty, viewport, width, font, fontMetrics, gridMetrics, forceCellLayout)
