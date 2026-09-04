package com.serenity.state.manager

import com.serenity.state.models.*
import com.serenity.ui.fonts.FontLoader
import com.serenity.ui.layout.{CellMetrics, LayoutEngine, TextLayoutSnapshot, ViewportSize}

/** Builds, at the effect boundary, the immutable navigation geometry a vertical cursor move needs. This is the font
  * lookup, cell-metric and text-layout-snapshot work the reducer used to do inline; keeping it here leaves the reducer
  * a pure function of state and geometry.
  */
object EditorGeometryProducer:

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
    val snapshot =
      TextLayoutSnapshot.fromBuffer(
        buffer.copy(viewport = buffer.viewport.copy(leftColumn = 0, topVisualLine = 0)),
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
