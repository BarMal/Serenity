package com.serenity.state.manager

import com.serenity.state.models.*
import com.serenity.ui.fonts.FontLoader
import com.serenity.ui.layout.{CellMetrics, LayoutEngine, TextLayoutSnapshot, VisualRowCounts, WrappedLineCache}

/** Java2D/font measurement for cursor-visibility scrolling belongs at the effect boundary, not in a reducer -- a
  * reducer runs mid-edit against content the effect boundary has not seen yet. `adjustForCursor` is the shared
  * measurement (also used directly by mouse-click and vertical-navigation effect handlers); `ensureVisibleCursors` is
  * the boundary pass that re-applies it after a pure reduce, for every buffer whose primary cursor moved.
  */
object CursorViewport:

  def ensureVisibleCursors(
    before: AppState,
    after: AppState,
    wrapCache: WrappedLineCache = WrappedLineCache.Uncached
  ): AppState =
    after.persisted.buffers.foldLeft(after) {
      case (state, (bufferId, buffer)) =>
        val beforeBuffer = before.persisted.buffers.get(bufferId)
        val headMoved =
          beforeBuffer.exists(_.editing.cursorPositions.headOption != buffer.editing.cursorPositions.headOption)
        if !headMoved then state
        else
          buffer.editing.cursorPositions.headOption match
            case Some(cursor) =>
              val surfaceConfig    = state.persisted.config.surfaceConfig
              val columnModeActive = surfaceConfig.columnModeEnabled && surfaceConfig.wordWrapEnabled
              val placement =
                if columnModeActive then adjustForCursorColumnMode(buffer, state, cursor, wrapCache)
                else adjustForCursor(buffer, state, cursor, wrapCache)
              val updatedBuffer = buffer.copy(viewport = placement)
              state.copy(persisted =
                state.persisted.copy(buffers = state.persisted.buffers + (bufferId -> updatedBuffer))
              )
            case None => state
    }

  def adjustForCursor(
    buffer: Buffer,
    currentState: AppState,
    cursor: CursorPosition,
    wrapCache: WrappedLineCache = WrappedLineCache.Uncached
  ): Viewport =
    val wordWrapEnabled            = currentState.persisted.config.surfaceConfig.wordWrapEnabled
    val typewriterScrollingEnabled = currentState.persisted.config.surfaceConfig.typewriterScrollingEnabled
    val isTui                      = currentState.runtime.capabilities.isCellGrid
    val viewport                   = buffer.viewport
    val fontConfig                 = currentState.persisted.config.editorConfig.fontConfig
    val font                       = previewFontForBuffer(buffer, fontConfig)
    val gridWidthPx                = TextLayoutSnapshot.gridWrapWidthPx(viewport.visibleColumns, fontConfig)
    // `viewport.visibleLines` is a code-grid row count (`LayoutEngine.updateViewportDimensions` sets it from the
    // panel's grid rows, the same convention `gridWrapWidthPx` uses for columns) -- it does not vary with which font a
    // pane actually draws with. A document-font pane whose font has a taller line height than the code font fits fewer
    // of its own rows into that same pixel height than a code-font pane would, exactly what
    // `RendererPaneSetup.snapshotForBuffer`'s own `visibleLines = panelHeightPx / bufferMetrics.lineHeight` computes
    // for the pane actually painted. Using the code-grid count unadjusted let this centred/bottom-aligned scroll math
    // assume more rows fit than the pane's own font renders, scrolling the cursor below the pane's real bottom edge on
    // a long wrapped line (#1041).
    val effectiveVisibleLines =
      if isTui then viewport.visibleLines
      else
        val codeLineHeightPx =
          CellMetrics.fromFont(FontLoader.previewFontForRole(fontConfig, TypographyRole.Code)).lineHeight
        val panelHeightPx = viewport.visibleLines * codeLineHeightPx
        math.max(1, panelHeightPx / math.max(1, CellMetrics.fromFont(font).lineHeight))
    val halfVisibleLines = effectiveVisibleLines / 2
    // Count wrapped rows the way the terminal actually drew them: TUI wraps on a fixed 1px-per-cell grid
    // (`CellMetrics.cellUnit` + forceCellLayout), never a pixel measurement of the inert proportional prose font, which
    // folds each line across a different number of rows and so mis-places the centred viewport. Mirrors
    // `EditorGeometryProducer`.
    val cellMetricsOverride = if isTui then Some(CellMetrics.cellUnit) else None
    val forceCellLayout     = isTui
    val wrapWidthPx         = if isTui then viewport.visibleColumns * CellMetrics.cellUnit.charWidth else gridWidthPx
    val lineText            = buffer.document.content.getLine(cursor.line).getOrElse("")
    val cursorVisualLine =
      if !wordWrapEnabled then 0
      else
        TextLayoutSnapshot.visualLineIndexForCursor(
          lineText,
          cursor.column,
          wrapWidthPx,
          font,
          wordWrapEnabled = true,
          cellMetricsOverride = cellMetricsOverride,
          forceCellLayout = forceCellLayout,
          rowAffinity = cursor.rowAffinity,
          wrapCache = wrapCache
        )

    // Counted the same way `cursorVisualLine` above was measured, or the two disagree.
    val visualRows =
      if !wordWrapEnabled then VisualRowCounts.oneRowPerLine(buffer.document.content.lineCount)
      else VisualRowCounts.forBuffer(buffer, wrapWidthPx, font, cellMetricsOverride, forceCellLayout, wrapCache)

    // Desired top: halfVisibleLines rows of context above the cursor's own visual row, counted in visual rows (not
    // logical lines) and carrying the partial offset into whatever line that lands on so the cursor stays centred.
    // Forcing that offset to 0 whenever the top wasn't the cursor's own line let the cursor drift off-centre by up to a
    // full wrapped line's worth of rows.
    val scrollUpBudget = halfVisibleLines - cursorVisualLine
    val (rawTopLine, rawTopVisualLine) =
      if scrollUpBudget <= 0 then (cursor.line, math.max(0, cursorVisualLine - halfVisibleLines))
      else visualRows.rowAbove(cursor.line, scrollUpBudget)

    // Bottom clamp: the latest (line, visual-row) start that still fills the viewport with real content. Without this,
    // a cursor near the end of a short-ish document can leave blank rows below the last line. Typewriter scrolling
    // deliberately skips this clamp: its entire point is to hold the cursor's line at its centred row even while
    // typing at the very end of the document, which means padding with blank rows below rather than showing as much
    // real content as fits (#1204, #1293).
    val lineCount = buffer.document.content.lineCount
    // Every line is at least one row, so the bottom-aligned top is never above `lineCount - effectiveVisibleLines`; a
    // desired top above that cannot exceed it, and the last screen need not be measured.
    def exceedsBottomWindow: Option[(Int, Int)] =
      if lineCount <= 0 || rawTopLine < lineCount - effectiveVisibleLines then None
      else
        val (bottomLine, bottomVisualLine) = visualRows.rowAbove(lineCount, effectiveVisibleLines)
        val exceedsBottom =
          rawTopLine > bottomLine || (rawTopLine == bottomLine && rawTopVisualLine > bottomVisualLine)
        Option.when(exceedsBottom)((bottomLine, bottomVisualLine))
    val (clampedTopLine, topVisualLine) =
      if typewriterScrollingEnabled then (rawTopLine, rawTopVisualLine)
      else exceedsBottomWindow.getOrElse((rawTopLine, rawTopVisualLine))
    val clampedLeftColumn =
      if wordWrapEnabled then 0
      else
        // Same TUI-vs-font split as the wrap measurement above: horizontal scrolling has to be measured on the grid
        // the terminal actually draws on, or a line of wide glyphs scrolls to the wrong column.
        val measuredLeftColumn =
          TextLayoutSnapshot.leftColumnForCursorVisibility(
            lineText,
            cursor.column,
            if isTui then wrapWidthPx else gridWidthPx,
            font,
            cellMetricsOverride = cellMetricsOverride,
            forceCellLayout = forceCellLayout
          )
        val minimumVisibleColumn = math.max(0, cursor.column - viewport.visibleColumns + 1)
        math.max(minimumVisibleColumn, measuredLeftColumn)

    viewport.copy(
      topLine = clampedTopLine,
      leftColumn = clampedLeftColumn,
      topVisualLine = topVisualLine
    )

  /** Column-based document layout (issue #1338, Phase 1): fixed/discrete snapping only -- `topVisualLine` always lands
    * on an exact `activeColumnIndex * visibleLines` boundary (`activeColumnIndex` derived here as
    * `absoluteCursorRow / visibleLines`, never stored), so the column holding the cursor's visual row is always shown
    * from its own top row. A sibling to [[adjustForCursor]] rather than a branch inside it -- that function's centring/
    * typewriter/bottom-clamp logic does not apply here at all, so branching it in would only add complexity to both
    * paths for no shared benefit.
    *
    * Walks from the *previous* placement (`buffer.viewport`) rather than re-measuring every line from the document
    * start on every call. `buffer.viewport.topVisualLine` is always an exact multiple of the `visibleLines` it was
    * placed with (the invariant above), so the new target column boundary can be found by measuring only the lines
    * between the previous top and the cursor, and then walking that same distance from the previous top -- cost
    * proportional to how far the cursor moved since the last placement, not to its absolute position in the document.
    * Re-scanning from line 0 made moving the cursor progressively through a large wrapped document O(n) per move
    * (O(n^2) overall). The one case that invalidates the shortcut -- column mode just switched on (the inherited
    * viewport came from `adjustForCursor`'s centring, whose `topVisualLine` has no reason to be a multiple of
    * `visibleLines`) or a resize changed `visibleLines` since the last placement -- is detected by the same modulus
    * check and falls back to the original from-scratch scan.
    */
  def adjustForCursorColumnMode(
    buffer: Buffer,
    currentState: AppState,
    cursor: CursorPosition,
    wrapCache: WrappedLineCache = WrappedLineCache.Uncached
  ): Viewport =
    val isTui      = currentState.runtime.capabilities.isCellGrid
    val viewport   = buffer.viewport
    val fontConfig = currentState.persisted.config.editorConfig.fontConfig
    val font       = previewFontForBuffer(buffer, fontConfig)
    // Each column's text wraps inside its band minus the line-number rail, exactly as `RendererPaneSetup` and
    // `EditorGeometryProducer` wrap it; measuring the full band folds fewer rows than are drawn, and the page anchor
    // drifts away from what is on screen.
    val textColumns         = math.max(1, viewport.visibleColumns - LayoutEngine.perColumnGutterWidth(currentState))
    val gridWidthPx         = TextLayoutSnapshot.gridWrapWidthPx(textColumns, fontConfig)
    val cellMetricsOverride = if isTui then Some(CellMetrics.cellUnit) else None
    val forceCellLayout     = isTui
    val wrapWidthPx         = if isTui then textColumns * CellMetrics.cellUnit.charWidth else gridWidthPx
    val lineCount           = buffer.document.content.lineCount
    val visibleLines        = math.max(1, viewport.visibleLines)

    val visualRows =
      VisualRowCounts.forBuffer(buffer, wrapWidthPx, font, cellMetricsOverride, forceCellLayout, wrapCache)

    val lineText = buffer.document.content.getLine(cursor.line).getOrElse("")
    val cursorVisualRowInLine =
      TextLayoutSnapshot.visualLineIndexForCursor(
        lineText,
        cursor.column,
        wrapWidthPx,
        font,
        wordWrapEnabled = true,
        cellMetricsOverride = cellMetricsOverride,
        forceCellLayout = forceCellLayout,
        rowAffinity = cursor.rowAffinity,
        wrapCache = wrapCache
      )

    // Multi-column page anchoring (issue #1338, Phase 2 / slice 1): a "page" shows `columnCount` columns side by side,
    // so the viewport anchors to a whole-page boundary (`pageRows` rows) rather than a single column (`visibleLines`
    // rows). With one column -- the fallback whenever the pane's full width is unknown -- `pageRows == visibleLines`,
    // so this reduces exactly to the single-column anchoring from before multi-column pages existed.
    val columnCount = columnCountForBuffer(buffer, currentState)
    val pageRows    = columnCount * visibleLines

    def findTopForward(line: Int, targetRow: Int): (Int, Int) =
      visualRows.rowBelow(line, targetRow).getOrElse((math.max(0, lineCount - 1), 0))

    val previousTopValid = viewport.topVisualLine % pageRows == 0

    val (topLine, topVisualLine) =
      if lineCount <= 0 then (0, 0)
      else if previousTopValid then
        val previousTopLine = math.max(0, math.min(viewport.topLine, lineCount - 1))
        val relativeCursorRow =
          visualRows.rowsBetween(previousTopLine, cursor.line) + cursorVisualRowInLine - viewport.topVisualLine
        val targetOffsetFromPreviousTop      = Math.floorDiv(relativeCursorRow, pageRows) * pageRows
        val targetRowFromPreviousTopLineHead = viewport.topVisualLine + targetOffsetFromPreviousTop
        if targetRowFromPreviousTopLineHead >= 0 then findTopForward(previousTopLine, targetRowFromPreviousTopLineHead)
        else visualRows.rowAbove(previousTopLine, -targetRowFromPreviousTopLineHead)
      else
        val absoluteCursorRow = visualRows.rowsBetween(0, cursor.line) + cursorVisualRowInLine
        val targetVisualRow   = (absoluteCursorRow / pageRows) * pageRows
        findTopForward(0, targetVisualRow)

    viewport.copy(topLine = topLine, leftColumn = 0, topVisualLine = topVisualLine)

  /** How many e-reader columns a page shows for `buffer`'s pane -- `LayoutEngine.resolvedColumnCount` of the pane's own
    * full content width (in cells): Auto fits "as many as fit" at the configured target width/gap, an explicit
    * `columnCount` pins the count (issue #1338 slice 4). The buffer's own `viewport.visibleColumns` here is already the
    * NARROWED single-column width, so the full pane width has to be recovered from the pane layout. When
    * `runtime.viewportSize` is unset (no laid-out window yet -- e.g. a reducer test) the full width is unknown, so this
    * falls back to a single column, which makes [[adjustForCursorColumnMode]]'s page anchoring exactly the
    * active-column anchoring it did before multi-column pages existed.
    */
  private def columnCountForBuffer(buffer: Buffer, state: AppState): Int =
    val surfaceConfig = state.persisted.config.surfaceConfig
    state.runtime.viewportSize match
      case None => 1
      case Some(viewportSize) =>
        val calculated  = LayoutEngine.calculateLayoutWithUI(state, viewportSize)
        val paneLayouts = LayoutEngine.calculateEditorPaneLayouts(state, calculated)
        val paneContentWidth = state.persisted.layout.editorPanes.collectFirst {
          case (paneId, pane) if pane.bufferId.contains(buffer.id) =>
            paneLayouts.get(paneId).map(_.contentRect.width)
        }.flatten
        paneContentWidth match
          case None               => 1
          case Some(contentWidth) => LayoutEngine.resolvedColumnCount(contentWidth, surfaceConfig)

  private def previewFontForBuffer(
    buffer: Buffer,
    config: FontLoader.FontConfig
  ): java.awt.Font =
    FontLoader.previewFontForRole(config, buffer.typographyRole)
