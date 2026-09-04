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

  def forPane(state: AppState, paneId: PaneId): Option[EditorGeometry] =
    state.persisted.layout.editorPanes
      .get(paneId)
      .flatMap(_.bufferId)
      .flatMap(state.persisted.buffers.get)
      .map(buffer => forBuffer(state, buffer))

  private def forBuffer(state: AppState, buffer: Buffer): EditorGeometry =
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
    val cursorLine    = buffer.editing.cursors.headOption.map(_.line).getOrElse(0)
    val windowTopLine = math.max(0, cursorLine - NavigationWindowMarginLines)
    val windowBudget  = math.max(24, buffer.viewport.visibleLines * 3)
    val snapshot =
      TextLayoutSnapshot.fromBuffer(
        buffer.copy(viewport =
          buffer.viewport.copy(leftColumn = 0, topLine = windowTopLine, topVisualLine = 0, visibleLines = windowBudget)
        ),
        panelWidthPx,
        font,
        wordWrapEnabled = state.persisted.config.surfaceConfig.wordWrapEnabled,
        cellMetricsOverride = if isTui then Some(CellMetrics.cellUnit) else None,
        forceCellLayout = isTui
      )
    EditorGeometry(snapshot.navigationGeometry, metrics.charWidth, panelWidthColumns)

  private def effectivePanelWidth(state: AppState): Int =
    val viewportSize = state.runtime.viewportSize.getOrElse(ViewportSize(80, 24))
    LayoutEngine.calculateLayout(state, viewportSize).editorPanelRect.width
