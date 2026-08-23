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
    state.layout.editorPanes
      .get(paneId)
      .flatMap(_.bufferId)
      .flatMap(state.buffers.get)
      .map(buffer => forBuffer(state, buffer))

  private def forBuffer(state: AppState, buffer: Buffer): EditorGeometry =
    val font              = FontLoader.previewFontForRole(state.config.fontConfig, buffer.typographyRole)
    val metrics           = CellMetrics.fromFont(font)
    val panelWidthColumns = effectivePanelWidth(state)
    val snapshot =
      TextLayoutSnapshot.fromBuffer(
        buffer.copy(viewport = buffer.viewport.copy(leftColumn = 0, topVisualLine = 0)),
        panelWidthColumns * metrics.charWidth,
        font,
        wordWrapEnabled = state.config.wordWrapEnabled
      )
    EditorGeometry(snapshot.navigationGeometry, metrics.charWidth, panelWidthColumns)

  private def effectivePanelWidth(state: AppState): Int =
    val viewportSize = state.viewportSize.getOrElse(ViewportSize(80, 24))
    LayoutEngine.calculateLayout(state, viewportSize).editorPanelRect.width
