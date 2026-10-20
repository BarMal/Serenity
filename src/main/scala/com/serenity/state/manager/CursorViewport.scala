package com.serenity.state.manager

import com.serenity.state.models.*
import com.serenity.ui.fonts.FontLoader
import com.serenity.ui.layout.{
  CellMetrics,
  ColumnPager,
  ColumnPaging,
  LayoutEngine,
  RichTextContext,
  TextLayoutSnapshot,
  TextLayoutSnapshotColumnMode,
  VisualRowCounts,
  WrappedLineCache
}
import com.serenity.ui.theme.RichTextStyling

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
        if headMoved then placeCursor(state, buffer, wrapCache) else state
    }

  /** Places every buffer an editor pane shows, for a change of pane geometry: a resize re-wraps the text, so the
    * viewport placed against the old wrap no longer holds the cursor on its centred row.
    */
  def replaceShownCursors(state: AppState, wrapCache: WrappedLineCache = WrappedLineCache.Uncached): AppState =
    val shown = state.persisted.layout.editorPanes.values.flatMap(_.bufferId).toSet
    state.persisted.buffers.foldLeft(state) {
      case (current, (bufferId, buffer)) =>
        if shown.contains(bufferId) then placeCursor(current, buffer, wrapCache) else current
    }

  private def placeCursor(state: AppState, buffer: Buffer, wrapCache: WrappedLineCache): AppState =
    buffer.editing.cursorPositions.headOption match
      case Some(cursor) =>
        val surfaceConfig    = state.persisted.config.surfaceConfig
        val columnModeActive = surfaceConfig.columnModeEnabled && surfaceConfig.wordWrapEnabled
        val placement =
          if columnModeActive then adjustForCursorColumnMode(buffer, state, cursor, wrapCache)
          else adjustForCursor(buffer, state, cursor, wrapCache)
        state.copy(persisted =
          state.persisted.copy(buffers = state.persisted.buffers + (buffer.id -> buffer.copy(viewport = placement)))
        )
      case None => state

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
    val effectiveVisibleLines      = rowsDrawnPerScreen(viewport, fontConfig, font, isTui)
    val halfVisibleLines           = effectiveVisibleLines / 2
    // Count wrapped rows the way the terminal actually drew them: TUI wraps on a fixed 1px-per-cell grid
    // (`CellMetrics.cellUnit` + forceCellLayout), never a pixel measurement of the inert proportional prose font, which
    // folds each line across a different number of rows and so mis-places the centred viewport. Mirrors
    // `EditorGeometryProducer`.
    val cellMetricsOverride = if isTui then Some(CellMetrics.cellUnit) else None
    val forceCellLayout     = isTui
    val wrapWidthPx         = if isTui then viewport.visibleColumns * CellMetrics.cellUnit.charWidth else gridWidthPx
    val lineText            = buffer.document.content.getLine(cursor.line).getOrElse("")
    val dropCapsEnabled     = currentState.persisted.config.documentConfig.dropCapsEnabled
    val markdownViewMode    = currentState.persisted.config.inlineMarkdownViewMode
    val tabSize             = buffer.effectiveIndentation(currentState.persisted.config.indentation).tabSize
    val headings            = currentState.effectiveHeadingSettings
    val openings            = buffer.effectiveOpenings(currentState.persisted.config.openingsConfig)
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
          wrapCache = wrapCache,
          bufferLine = cursor.line,
          richText = RichTextContext
            .forBuffer(
              buffer,
              font,
              dropCapsEnabled,
              markdownViewMode,
              isTui,
              wrapCache,
              tabSize = tabSize,
              headings = headings,
              openings = openings,
              dropCapGapEm = currentState.persisted.config.documentConfig.dropCapGapEm
            )
        )

    // Counted the same way `cursorVisualLine` above was measured, or the two disagree.
    val visualRows =
      if !wordWrapEnabled then VisualRowCounts.oneRowPerLine(buffer.document.content.lineCount)
      else
        VisualRowCounts.forBuffer(
          buffer,
          wrapWidthPx,
          font,
          cellMetricsOverride,
          forceCellLayout,
          wrapCache,
          dropCapsEnabled,
          markdownViewMode,
          tabSize,
          headings = headings,
          openings = openings,
          dropCapGapEm = currentState.persisted.config.documentConfig.dropCapGapEm
        )

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
            wrapCache.shapedText,
            cellMetricsOverride = cellMetricsOverride,
            forceCellLayout = forceCellLayout,
            tabSize = tabSize
          )
        val minimumVisibleColumn = math.max(0, cursor.column - viewport.visibleColumns + 1)
        math.max(minimumVisibleColumn, measuredLeftColumn)

    viewport.copy(
      topLine = clampedTopLine,
      leftColumn = clampedLeftColumn,
      topVisualLine = topVisualLine
    )

  /** `viewport.visibleLines` is a code-grid row count (`LayoutEngine.updateViewportDimensions` sets it from the panel's
    * grid rows, the same convention `gridWrapWidthPx` uses for columns) -- it does not vary with which font a pane
    * actually draws with. A document-font pane whose font has a taller line height than the code font fits fewer of its
    * own rows into that same pixel height than a code-font pane would, exactly what
    * `RendererPaneSetup.snapshotForBuffer`'s own `visibleLines = panelHeightPx / bufferMetrics.lineHeight` computes for
    * the pane actually painted. Using the code-grid count unadjusted let the centred/bottom-aligned scroll math assume
    * more rows fit than the pane's own font renders, scrolling the cursor below the pane's real bottom edge on a long
    * wrapped line (#1041), and let column mode's page anchor run past the last drawn column without turning.
    */
  private[manager] def rowsDrawnPerScreen(
    viewport: Viewport,
    fontConfig: FontLoader.FontConfig,
    font: java.awt.Font,
    isTui: Boolean
  ): Int =
    if isTui then viewport.visibleLines
    else
      val codeLineHeightPx =
        CellMetrics.fromFont(FontLoader.previewFontForRole(fontConfig, TypographyRole.Code)).lineHeight
      val panelHeightPx = viewport.visibleLines * codeLineHeightPx
      math.max(1, panelHeightPx / math.max(1, CellMetrics.fromFont(font).lineHeight))

  /** Column-based document layout (issue #1338, Phase 1): discrete snapping only -- the viewport always lands on the
    * first column of the page holding the cursor's column, so that column is shown from its own top row. Columns are
    * filled by pixel height (see [[ColumnPager]]), so where one starts is read off the document's own canonical column
    * starts from its beginning, not a multiple of a row count; the same cursor therefore gets the same page whichever
    * way it got there. A sibling to [[adjustForCursor]] rather than a branch inside it -- that function's centring/
    * typewriter/bottom-clamp logic does not apply here at all.
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
    val visibleLines        = math.max(1, rowsDrawnPerScreen(viewport, fontConfig, font, isTui))

    val pager = columnPagerFor(
      buffer,
      currentState,
      wrapWidthPx,
      font,
      visibleLines,
      cellMetricsOverride,
      forceCellLayout,
      wrapCache
    )

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
        wrapCache = wrapCache,
        bufferLine = cursor.line,
        richText = pager.richText
      )

    // A page is `columnCount` consecutive columns, and columns break where their rows stop fitting the column's pixel
    // height (`ColumnPager`), so the page is the one holding the cursor's column, from that page's first column.
    val columnCount = columnCountForBuffer(buffer, currentState)
    val pageStart =
      if lineCount <= 0 then ColumnPaging.ColumnStart.Origin
      else
        val starts = pager.startsThrough(ColumnPaging.ColumnStart(cursor.line, cursorVisualRowInLine))
        starts((starts.length - 1) / columnCount * columnCount)

    viewport.copy(topLine = pageStart.line, leftColumn = 0, topVisualLine = pageStart.row)

  /** The pager that decides where `buffer`'s columns break, built from the same inputs the painter uses
    * (`RendererPaneSetup`, `UiSceneBuilder`) so anchoring and painting agree on every column.
    */
  private[manager] def columnPagerFor(
    buffer: Buffer,
    state: AppState,
    wrapWidthPx: Int,
    font: java.awt.Font,
    visibleLines: Int,
    cellMetricsOverride: Option[CellMetrics],
    forceCellLayout: Boolean,
    wrapCache: WrappedLineCache
  ): ColumnPager =
    TextLayoutSnapshotColumnMode.pagerFor(
      buffer,
      wrapWidthPx,
      font,
      visibleLines,
      cellMetricsOverride = cellMetricsOverride,
      forceCellLayout = forceCellLayout,
      proseScale = RichTextStyling.proseZoom(font.getSize2D),
      dropCapsEnabled = state.persisted.config.documentConfig.dropCapsEnabled,
      wrapCache = wrapCache,
      tabSize = buffer.effectiveIndentation(state.persisted.config.indentation).tabSize,
      headings = state.effectiveHeadingSettings,
      openings = buffer.effectiveOpenings(state.persisted.config.openingsConfig),
      dropCapGapEm = state.persisted.config.documentConfig.dropCapGapEm
    )

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
