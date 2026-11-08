package com.serenity.state.manager

import com.serenity.state.models.*
import com.serenity.ui.fonts.FontLoader
import com.serenity.ui.layout.{
  CellMetrics,
  ColumnPager,
  ColumnPaging,
  LayoutEngine,
  PaneRowBands,
  RichTextContext,
  ScrollLimits,
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

  /** `keyedIn` is the buffer a caret or edit key just went to: a view the wheel scrolled away from its caret is placed
    * again there even when the key left the caret where it was.
    */
  def ensureVisibleCursors(
    before: AppState,
    after: AppState,
    wrapCache: WrappedLineCache = WrappedLineCache.Uncached,
    keyedIn: Option[BufferId] = None
  ): AppState =
    after.persisted.buffers.foldLeft(after) {
      case (state, (bufferId, buffer)) =>
        if ViewportResolution.needsPlacing(before, bufferId, buffer, keyedIn) then placeCursor(state, buffer, wrapCache)
        else state
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

  /** Places `bufferId`'s viewport against its primary cursor, as a caret move would. */
  def placed(state: AppState, bufferId: BufferId, wrapCache: WrappedLineCache = WrappedLineCache.Uncached): AppState =
    state.persisted.buffers.get(bufferId).fold(state)(placeCursor(state, _, wrapCache))

  /** The viewport that shows `cursor` in `buffer`'s pane: the page holding it in column mode, otherwise the rows
    * typewriter scrolling or the edge margins call for. The one placement every caret move and jump goes through.
    */
  def placementFor(
    buffer: Buffer,
    state: AppState,
    cursor: CursorPosition,
    wrapCache: WrappedLineCache = WrappedLineCache.Uncached
  ): Viewport =
    val surfaceConfig = state.persisted.config.surfaceConfig
    if surfaceConfig.columnModeEnabled && surfaceConfig.wordWrapEnabled then
      adjustForCursorColumnMode(buffer, state, cursor, wrapCache)
    else adjustForCursor(buffer, state, cursor, wrapCache)

  private def placeCursor(state: AppState, buffer: Buffer, wrapCache: WrappedLineCache): AppState =
    buffer.editing.cursorPositions.headOption match
      case Some(cursor) =>
        val placement = placementFor(buffer, state, cursor, wrapCache)
        val placed    = placement.copy(placement = ViewportPlacement.Placed)
        state.copy(persisted =
          state.persisted.copy(buffers = state.persisted.buffers + (buffer.id -> buffer.copy(viewport = placed)))
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
    val fontFallback        = currentState.persisted.config.editorConfig.fontConfig.fallback
    val headings            = currentState.effectiveHeadingSettings
    val openings            = buffer.effectiveOpenings(currentState.persisted.config.openingsConfig)
    val richText = RichTextContext
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
        dropCapGapEm = currentState.persisted.config.documentConfig.dropCapGapEm,
        fallback = fontFallback
      )
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
          richText = richText
        )

    // Placed by the rows' painted bands, not a count of rows: a heading, a drop cap or the prose font paints taller
    // than a row of the buffer font, and counting rows sank a "centred" caret towards the pane's bottom (#2570).
    val reach = viewport.visibleLines + MarginRows
    // A jump (the caret above the view, or further below it than the pane shows) walks about a pane of lines up from
    // the caret; they are read in one pass. A caret moving within the pane reads only the lines between it and the top.
    val jumped =
      typewriterScrollingEnabled || cursor.line < viewport.topLine || cursor.line - viewport.topLine > reach
    val readAhead =
      if jumped then PaneRowBands.ReadAhead(cursor.line - reach, reach + 1) else PaneRowBands.ReadAhead.None
    val bands =
      PaneRowBands.forBuffer(
        buffer,
        wrapWidthPx,
        font,
        cellMetricsOverride,
        forceCellLayout,
        wordWrapEnabled,
        richText,
        wrapCache,
        readAhead = readAhead
      )
    val paneHeightPx = paneHeightPxFor(viewport, fontConfig, isTui)
    val caret        = PaneRowBands.Row(cursor.line, cursorVisualLine)
    val caretBoxPx   = bands.textBoxPx(caret)
    val current      = PaneRowBands.Row(viewport.topLine, if wordWrapEnabled then viewport.topVisualLine else 0)

    // Typewriter scrolling holds the caret's row at the pane's middle, padding past the document's end (#1204, #1293).
    // Otherwise a caret already on screen moves the view only to stay `MarginRows` clear of the pane's edges -- less
    // where the document ends sooner, so its last line still rests on the pane's bottom -- and a caret that jumped off
    // it (a find, a go-to, a page) is brought to the middle, as #1204 kept jumps.
    val ordering  = summon[Ordering[PaneRowBands.Row]]
    val gridRowPx = paneHeightPx / math.max(1, viewport.visibleLines)
    val walk      = bands.tops(caret)
    def centred   = bands.topPainting(walk, caret, gridRowPx, viewport.visibleLines / 2)
    // How far below the current top the caret's text sits, when the pane shows it: read off the walk up from the caret,
    // which stops at the current top, so a caret moving within the pane costs only the rows between the two.
    val shownAtPx =
      if typewriterScrollingEnabled || ordering.gt(current, caret) then None
      else walk.takeWhile(_._2 <= paneHeightPx - caretBoxPx).find(_._1 == current).map(_._2)
    val placedTop = shownAtPx match
      case None => centred
      case Some(offsetPx) =>
        val margin = math.max(0, math.min(MarginRows * bands.lineHeightPx, (paneHeightPx - caretBoxPx) / 2))
        if offsetPx >= margin && offsetPx <= paneHeightPx - caretBoxPx - margin then current
        else
          // The rows below the caret are counted only when it nears the bottom, where the document's end can lift
          // the bottom margin.
          val roomLow  = paneHeightPx - caretBoxPx - math.min(margin, bands.belowPx(caret, margin))
          val earliest = bands.topWithin(walk, caret, roomLow)
          val latest   = bands.topWithin(walk, caret, margin)
          if ordering.lt(current, earliest) then earliest
          else if ordering.gt(current, latest) then latest
          else current

    // Bottom clamp: the latest start that still fills the pane with real content, so a view left past the end of a
    // shortened document comes back. Typewriter scrolling pads instead. Every line is at least one row, so a top above
    // `lineCount - effectiveVisibleLines` is never past the last screen and need not be measured.
    val lineCount = buffer.document.content.lineCount
    val (clampedTopLine, topVisualLine) =
      if typewriterScrollingEnabled || lineCount <= 0 || placedTop.line < lineCount - effectiveVisibleLines then
        (placedTop.line, placedTop.row)
      else
        val (line, row) = ScrollLimits
          .last(visualRowsFor(buffer, currentState, wrapCache), effectiveVisibleLines)
          .clamp(placedTop.line, placedTop.row)
        // Rows counted at the buffer font's height can call a top the last screen that leaves the caret below the
        // pane when its rows paint taller; the caret stays shown.
        val earliest = bands.topWithin(walk, caret, paneHeightPx - caretBoxPx)
        if ordering.lt(PaneRowBands.Row(line, row), earliest) then (earliest.line, earliest.row)
        else (line, row)
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
            tabSize = tabSize,
            wrapCache = wrapCache,
            fallback = fontFallback
          )
        val minimumVisibleColumn = math.max(0, cursor.column - viewport.visibleColumns + 1)
        math.max(minimumVisibleColumn, measuredLeftColumn)

    viewport.copy(
      topLine = clampedTopLine,
      leftColumn = clampedLeftColumn,
      topVisualLine = topVisualLine
    )

  /** The rows of the buffer font's height the view keeps between the caret and the pane's top and bottom edges when
    * typewriter scrolling is off (#2570).
    */
  private[manager] val MarginRows = 3

  /** The pane's height in the units its rows' bands are measured in: cells in the terminal, pixels of the code grid the
    * pane is laid out on in the window.
    */
  private def paneHeightPxFor(viewport: Viewport, fontConfig: FontLoader.FontConfig, isTui: Boolean): Int =
    if isTui then viewport.visibleLines
    else
      viewport.visibleLines *
        CellMetrics.fromFont(FontLoader.previewFontForRole(fontConfig, TypographyRole.Code)).lineHeight

  /** The visual rows of `buffer`'s lines as the pane wraps them, for the limits the wheel and the cursor both keep to.
    */
  private[manager] def visualRowsFor(
    buffer: Buffer,
    currentState: AppState,
    wrapCache: WrappedLineCache
  ): VisualRowCounts =
    val config     = currentState.persisted.config
    val isTui      = currentState.runtime.capabilities.isCellGrid
    val fontConfig = config.editorConfig.fontConfig
    val viewport   = buffer.viewport
    if !config.surfaceConfig.wordWrapEnabled then VisualRowCounts.oneRowPerLine(buffer.document.content.lineCount)
    else
      VisualRowCounts.forBuffer(
        buffer,
        if isTui then viewport.visibleColumns * CellMetrics.cellUnit.charWidth
        else TextLayoutSnapshot.gridWrapWidthPx(viewport.visibleColumns, fontConfig),
        previewFontForBuffer(buffer, fontConfig),
        if isTui then Some(CellMetrics.cellUnit) else None,
        isTui,
        wrapCache,
        config.documentConfig.dropCapsEnabled,
        config.inlineMarkdownViewMode,
        buffer.effectiveIndentation(config.indentation).tabSize,
        headings = currentState.effectiveHeadingSettings,
        openings = buffer.effectiveOpenings(config.openingsConfig),
        dropCapGapEm = config.documentConfig.dropCapGapEm,
        fallback = fontConfig.fallback
      )

  /** The rows of the pane's own font that fit `buffer`'s pane. */
  private[serenity] def rowsThatFit(buffer: Buffer, currentState: AppState): Int =
    val fontConfig = currentState.persisted.config.editorConfig.fontConfig
    rowsDrawnPerScreen(
      buffer.viewport,
      fontConfig,
      previewFontForBuffer(buffer, fontConfig),
      currentState.runtime.capabilities.isCellGrid
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
    else ScrollLimits.rowsThatFit(paneHeightPxFor(viewport, fontConfig, isTui), CellMetrics.fromFont(font).lineHeight)

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
    val paging   = ColumnPages.of(buffer, currentState, wrapCache)
    val lineText = buffer.document.content.getLine(cursor.line).getOrElse("")
    val cursorVisualRowInLine =
      TextLayoutSnapshot.visualLineIndexForCursor(
        lineText,
        cursor.column,
        paging.wrapWidthPx,
        paging.font,
        wordWrapEnabled = true,
        cellMetricsOverride = paging.cellMetricsOverride,
        forceCellLayout = paging.forceCellLayout,
        rowAffinity = cursor.rowAffinity,
        wrapCache = wrapCache,
        bufferLine = cursor.line,
        richText = paging.pager.richText
      )

    val pageStart = paging.pageHolding(ColumnPaging.ColumnStart(cursor.line, cursorVisualRowInLine))
    buffer.viewport.copy(topLine = pageStart.line, leftColumn = 0, topVisualLine = pageStart.row)

  /** The pages `buffer`'s pane shows in column mode: a page is `columnCount` consecutive columns, and columns break
    * where their rows stop fitting the column's pixel height ([[ColumnPager]]). The caret's placement and the wheel's
    * page turns both read pages from here, so each lands on a page the painter draws from its first column.
    */
  final private[manager] case class ColumnPages(
      pager: ColumnPager,
      columnCount: Int,
      lineCount: Int,
      font: java.awt.Font,
      wrapWidthPx: Int,
      cellMetricsOverride: Option[CellMetrics],
      forceCellLayout: Boolean
  ):

    def pageHolding(target: ColumnPaging.ColumnStart): ColumnPaging.ColumnStart =
      if lineCount <= 0 then ColumnPaging.ColumnStart.Origin
      else
        val starts = pager.startsThrough(target)
        starts((starts.length - 1) / columnCount * columnCount)

    /** The start of the page after the one starting at `pageStart`, or `pageStart` itself on the last page. */
    def nextPage(pageStart: ColumnPaging.ColumnStart): ColumnPaging.ColumnStart =
      val afterPage = (1 to columnCount).foldLeft(Option(pageStart))((start, _) => start.flatMap(pager.nextStart))
      afterPage.getOrElse(pageStart)

    /** The start of the page before the one holding `pageStart`, or the first page's start. */
    def previousPage(pageStart: ColumnPaging.ColumnStart): ColumnPaging.ColumnStart =
      if lineCount <= 0 then ColumnPaging.ColumnStart.Origin
      else
        val starts   = pager.startsThrough(pageStart)
        val thisPage = (starts.length - 1) / columnCount
        starts(math.max(0, thisPage - 1) * columnCount)

  private[manager] object ColumnPages:

    def of(buffer: Buffer, state: AppState, wrapCache: WrappedLineCache): ColumnPages =
      val isTui      = state.runtime.capabilities.isCellGrid
      val viewport   = buffer.viewport
      val fontConfig = state.persisted.config.editorConfig.fontConfig
      val font       = previewFontForBuffer(buffer, fontConfig)
      // Each column's text wraps inside its band minus the line-number rail, exactly as `RendererPaneSetup` and
      // `EditorGeometryProducer` wrap it; measuring the full band folds fewer rows than are drawn, and the page anchor
      // drifts away from what is on screen.
      val textColumns         = math.max(1, viewport.visibleColumns - LayoutEngine.perColumnGutterWidth(state))
      val cellMetricsOverride = if isTui then Some(CellMetrics.cellUnit) else None
      val wrapWidthPx =
        if isTui then textColumns * CellMetrics.cellUnit.charWidth
        else TextLayoutSnapshot.gridWrapWidthPx(textColumns, fontConfig)
      val visibleLines = math.max(1, rowsDrawnPerScreen(viewport, fontConfig, font, isTui))
      ColumnPages(
        columnPagerFor(buffer, state, wrapWidthPx, font, visibleLines, cellMetricsOverride, isTui, wrapCache),
        columnCountForBuffer(buffer, state),
        buffer.document.content.lineCount,
        font,
        wrapWidthPx,
        cellMetricsOverride,
        forceCellLayout = isTui
      )

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
      dropCapGapEm = state.persisted.config.documentConfig.dropCapGapEm,
      fallback = state.persisted.config.editorConfig.fontConfig.fallback
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
