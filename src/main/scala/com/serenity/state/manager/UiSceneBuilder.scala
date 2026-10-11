package com.serenity.state.manager

import java.awt.Font

import com.serenity.state.models.*
import com.serenity.ui.layout.*

/** Lays out a whole [[AppState]] into the [[UiSceneSnapshot]] that rendering and mouse targeting share. Pure apart from
  * the shared [[WrappedLineCache]], so [[AuthoritativeUiScene]] can run it without holding any lock.
  */
private[serenity] object UiSceneBuilder:

  type Build = (AppState, ViewportSize, Font, Font, Option[CellMetrics]) => UiSceneSnapshot

  def apply(wrappedLines: WrappedLineCache): Build =
    (state, viewportSize, codeFont, textFont, cellMetrics) =>
      build(wrappedLines, state, viewportSize, codeFont, textFont, cellMetrics)

  private def build(
    wrappedLines: WrappedLineCache,
    state: AppState,
    viewportSize: ViewportSize,
    codeFont: Font,
    textFont: Font,
    cellMetrics: Option[CellMetrics]
  ): UiSceneSnapshot =
    val layout = LayoutEngine.calculateLayoutWithUI(state, viewportSize)
    val base   = UiSceneSnapshot.from(state, layout, viewportSize)
    // Pane rects are measured in the screen grid's cells, and that grid is the code font's, whatever font a
    // buffer draws with. Sizing a document-font pane from its own cells makes the snapshot wrap at a width the
    // pane does not have, so its rows run off the right edge instead of wrapping.
    val gridMetrics   = cellMetrics.getOrElse(CellMetrics.fromFont(codeFont))
    val surfaceConfig = state.persisted.config.surfaceConfig
    // Column-based document layout (issue #1338): mirrors `RendererPaneSetup.snapshotForBuffer`'s column branch so
    // this shared scene -- used by both painting and mouse hit-testing -- narrows the viewport to one column's width
    // and wraps at it whenever column mode and word wrap are both on. Without this the scene stayed full-width and a
    // column-mode toggle had no visible render effect (Phase 1 regression).
    val columnModeActive = surfaceConfig.columnModeEnabled && surfaceConfig.wordWrapEnabled
    val perPane = base.paneLayouts.flatMap {
      case (paneId, paneLayout) =>
        for
          pane     <- state.persisted.layout.editorPanes.get(paneId)
          bufferId <- pane.bufferId
          buffer   <- state.persisted.buffers.get(bufferId)
        yield
          val font        = if buffer.usesTextFont then textFont else codeFont
          val fontMetrics = cellMetrics.getOrElse(CellMetrics.fromFont(font))
          val width       = paneLayout.contentRect.width * gridMetrics.charWidth
          val heightPx    = paneLayout.contentRect.height * gridMetrics.lineHeight
          val baseViewport = LayoutEngine
            .updateBufferViewportDimensions(
              buffer,
              paneLayout.contentRect,
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
                .max(paneLayout.contentRect.width + 64)
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
          val proseScale = com.serenity.ui.theme.RichTextStyling.proseZoom(font.getSize2D)
          if columnModeActive then
            // `viewport.visibleColumns` is the column's own full band in cells (from the column-aware `baseViewport`
            // above). Slice 2: each column carries its own line-number rail on its left edge, so its TEXT wraps in the
            // band minus that rail (`columnTextWidthCells`) while the band itself (placement `columnWidthCells`) and
            // the inter-column offset stay full-width -- matching `RendererPaneSetup.snapshotForBuffer`'s column
            // branch. Slice 4: `resolvedColumnCount` is count-driven -- Auto fits "as many columns as fit" the pane's
            // full content width, `Some(n)` pins n; each column is placed at `columnIndex * (columnWidth + gap)`
            // cells.
            val columnWidthCells     = math.max(1, viewport.visibleColumns)
            val gutterWidthCells     = LayoutEngine.perColumnGutterWidth(state)
            val columnTextWidthCells = math.max(1, columnWidthCells - gutterWidthCells)
            val columnTextWidthPx    = math.max(1, columnTextWidthCells * gridMetrics.charWidth)
            val columnCount =
              LayoutEngine.resolvedColumnCount(paneLayout.contentRect.width, surfaceConfig)
            val columnSnapshotList = TextLayoutSnapshot.fromBufferColumns(
              buffer.copy(viewport = viewport),
              columnTextWidthPx,
              font,
              cellMetricsOverride = Some(fontMetrics),
              forceCellLayout = cellMetrics.isDefined,
              proseScale = proseScale,
              columnCount = columnCount,
              dropCapsEnabled = state.persisted.config.documentConfig.dropCapsEnabled,
              wrapCache = wrappedLines
            )
            // Only a genuinely multi-column page carries per-column placements. A single fitted column is fully
            // served by `textSnapshots` alone (identical to the pre-multi-column render path, including its
            // column-transition animation overlay), so emitting a lone placement would only make the renderer take
            // its multi-column branch for a page that has nothing to lay out side by side.
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
            // The single active-column snapshot every non-column consumer still reads is the page's first column
            // (index 0) -- the cursor's own column, since the viewport was snapped to the page holding it.
            val activeSnapshot = placements.headOption
              .map(_.snapshot)
              .getOrElse(
                TextLayoutSnapshot.fromBufferColumn(
                  buffer.copy(viewport = viewport),
                  columnTextWidthPx,
                  font,
                  cellMetricsOverride = Some(fontMetrics),
                  forceCellLayout = cellMetrics.isDefined,
                  proseScale = proseScale,
                  dropCapsEnabled = state.persisted.config.documentConfig.dropCapsEnabled,
                  wrapCache = wrappedLines
                )
              )
            paneId -> (activeSnapshot, placements)
          else
            val single = TextLayoutSnapshot.fromBuffer(
              buffer.copy(viewport = viewport),
              width,
              font,
              wordWrapEnabled = surfaceConfig.wordWrapEnabled,
              cellMetricsOverride = Some(fontMetrics),
              forceCellLayout = cellMetrics.isDefined,
              // Match the render path's prose zoom so hit-testing rows/advances line up with what was drawn.
              proseScale = proseScale,
              dropCapsEnabled = state.persisted.config.documentConfig.dropCapsEnabled,
              wrapCache = wrappedLines,
              markdownViewMode = state.persisted.config.inlineMarkdownViewMode
            )
            paneId -> (single, Vector.empty[ColumnSnapshotPlacement])
    }
    val textSnapshots   = perPane.view.mapValues(_._1).toMap
    val columnSnapshots = perPane.view.mapValues(_._2).filter(_._2.nonEmpty).toMap
    base.withTextSnapshots(textSnapshots).withColumnSnapshots(columnSnapshots)
