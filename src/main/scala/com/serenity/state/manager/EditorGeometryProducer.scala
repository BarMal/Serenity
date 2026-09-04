package com.serenity.state.manager

import com.serenity.state.models.*
import com.serenity.ui.fonts.FontLoader
import com.serenity.ui.layout.{CellMetrics, LayoutEngine, TextLayoutSnapshot, ViewportSize}

/** Builds, at the effect boundary, the immutable navigation geometry a vertical cursor move needs. This is the font
  * lookup, cell-metric and text-layout-snapshot work the reducer used to do inline; keeping it here leaves the reducer
  * a pure function of state and geometry.
  */
object EditorGeometryProducer:

  /** Logical lines of context to keep above the cursor when building the navigation window (see [[forBuffer]]). A
    * single UP move only needs the line directly above, so a small margin is plenty; keeping it small also keeps the
    * accumulate-until-budget walk below from being pushed off the cursor's own line by a run of wrapped lines above it.
    */
  private val NavigationWindowMarginLines = 2

  /** Visual rows of context to keep above the cursor's own row when the window has to start part-way into a wrapped
    * line (see [[forBuffer]]). One row is all an UP move reads; the rest is slack so a rounding difference between this
    * measurement and the snapshot's own wrapping cannot strand the cursor on the window's first row.
    */
  private val NavigationWindowMarginRows = 4

  /** `rowsAbove` is how many visual rows of context above the cursor's own row the caller needs the window to cover.
    * The default is all a single Up step reads. A page move reads a whole screenful in the direction it travels, and
    * the window is anchored near the cursor, so it asks for one explicitly -- otherwise PageUp's target row falls
    * outside the geometry and page navigation drops to the logical-line fallback it is trying to replace.
    */
  def forPane(
    state: AppState,
    paneId: PaneId,
    rowsAbove: Int = NavigationWindowMarginRows
  ): Option[EditorGeometry] =
    state.persisted.layout.editorPanes
      .get(paneId)
      .flatMap(_.bufferId)
      .flatMap(state.persisted.buffers.get)
      .map(buffer => forBuffer(state, buffer, rowsAbove))

  private def forBuffer(state: AppState, buffer: Buffer, rowsAbove: Int = NavigationWindowMarginRows): EditorGeometry =
    val font    = FontLoader.previewFontForRole(state.persisted.config.editorConfig.fontConfig, buffer.typographyRole)
    val isTui   = state.runtime.isTuiMode
    val metrics = if isTui then CellMetrics.cellUnit else CellMetrics.fromFont(font)
    val panelWidthColumns = effectivePanelWidth(state)
    // TUI mode's terminal cell is 1px wide by definition, not `font`'s measured pixel width -- `font` is never
    // actually rendered with in TUI mode (`TuiRuntime.CellFont`'s doc comment), so wrapping geometry for it here must
    // agree with `TerminalAnsiDiff`'s cell grid, not a pixel measurement of an inert AWT font.
    val panelWidthPx =
      if isTui then panelWidthColumns * CellMetrics.cellUnit.charWidth
      else TextLayoutSnapshot.gridWrapWidthPx(panelWidthColumns, state.persisted.config.editorConfig.fontConfig)
    // Build the geometry over a window centred on the cursor rather than pinned to the viewport's top line. Pinning to
    // the top let a single heavily-wrapped line there fill the whole visual-row budget, so the cursor's own line fell
    // out of the geometry entirely -- `moveVertical`/`visualLineFor` then found nothing and vertical navigation dropped
    // to naive char-grid movement that mis-wraps prose (the "stuck"/jumpy cursor past the first screenful). Starting a
    // couple of lines above the cursor keeps an UP neighbour in range, and a budget of a few screenfuls of visual rows
    // covers the cursor and its DOWN neighbours while staying bounded on huge documents.
    //
    // The window is anchored on the cursor's own *visual* row, not merely its logical line: one prose paragraph wraps
    // into far more rows than that budget, so a window that always started at the top of a logical line left the
    // cursor's row past the end of the budget for any cursor deep inside a long paragraph -- `visualLineFor` and
    // `moveVertical` then found nothing and Up/Down/Home/End fell back to naive char-grid movement, which is the
    // "wrapped navigation moves by logical line" bug in a real prose document.
    val wordWrapEnabled = state.persisted.config.surfaceConfig.wordWrapEnabled
    val cursor          = buffer.editing.cursors.headOption.getOrElse(CursorPosition(0, 0))
    // Every logical line is at least one visual row, so walking back `rowsAbove` logical lines always reaches far
    // enough to offer `rowsAbove` rows of context; `windowTopVisualLine` below then trims the excess. The window's
    // budget has to hold that context as well as the rows below the cursor it already covered.
    val marginLines         = math.max(NavigationWindowMarginLines, rowsAbove)
    val windowTopLine       = math.max(0, cursor.line - marginLines)
    val windowBudget        = math.max(24, rowsAbove + buffer.viewport.visibleLines * 3)
    val cellMetricsOverride = if isTui then Some(CellMetrics.cellUnit) else None
    def visualRowsIn(line: Int): Int =
      TextLayoutSnapshot
        .boundedVisualLinesForText(
          buffer.document.content.getLine(line).getOrElse(""),
          line,
          panelWidthPx,
          font,
          cellMetricsOverride = cellMetricsOverride,
          forceCellLayout = isTui
        )
        .length
        .max(1)
    val cursorRowInWindow =
      if !wordWrapEnabled then cursor.line - windowTopLine
      else
        (windowTopLine until cursor.line).map(visualRowsIn).sum +
          TextLayoutSnapshot.visualLineIndexForCursor(
            buffer.document.content.getLine(cursor.line).getOrElse(""),
            cursor.column,
            panelWidthPx,
            font,
            wordWrapEnabled = true,
            cellMetricsOverride = cellMetricsOverride,
            forceCellLayout = isTui
          )
    val windowTopVisualLine = math.max(0, cursorRowInWindow - rowsAbove)
    val snapshot =
      TextLayoutSnapshot.fromBuffer(
        buffer.copy(viewport =
          buffer.viewport.copy(
            leftColumn = 0,
            topLine = windowTopLine,
            topVisualLine = windowTopVisualLine,
            visibleLines = windowBudget
          )
        ),
        panelWidthPx,
        font,
        wordWrapEnabled = wordWrapEnabled,
        cellMetricsOverride = cellMetricsOverride,
        forceCellLayout = isTui
      )
    EditorGeometry(snapshot.navigationGeometry, metrics.charWidth, panelWidthColumns)

  private def effectivePanelWidth(state: AppState): Int =
    val viewportSize = state.runtime.viewportSize.getOrElse(ViewportSize(80, 24))
    LayoutEngine.calculateLayout(state, viewportSize).editorPanelRect.width
