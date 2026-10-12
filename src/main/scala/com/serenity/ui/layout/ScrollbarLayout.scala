package com.serenity.ui.layout

import com.serenity.state.models.AppState

/** Where an editor pane's scrollbar lies: a thin strip over the right edge of the pane's content, drawn on top of the
  * text. It takes no room from the text, which wraps at the width it always did; the bar repaints on its own account
  * and the text beneath it is put back whenever the bar moves.
  */
object ScrollbarLayout:

  /** In logical pixels, so the bar is as many device pixels wide as the display scales it to. */
  val StripWidthPx: Int = 6

  /** Narrower than this a pane is too small to carry a bar over. */
  private val MinPaneColumns: Int = 12

  /** A terminal cannot draw anything thinner than a cell, and a column-mode page is scrolled a page at a time, so
    * neither shows the strip.
    */
  def shown(state: AppState, contentRect: LayoutRect): Boolean =
    val surfaceConfig = state.persisted.config.surfaceConfig
    val pagedColumns  = surfaceConfig.columnModeEnabled && surfaceConfig.wordWrapEnabled
    !state.runtime.capabilities.isCellGrid && !pagedColumns && contentRect.width >= MinPaneColumns

  /** The strip over the right edge of `contentRect`, for the whole of its height. */
  def strip(contentRect: LayoutRect, metrics: CellMetrics): PixelRect =
    val width = StripWidthPx.min(metrics.charWidth).max(1)
    val right = metrics.toPixelX(contentRect.right)
    val top   = metrics.toPixelY(contentRect.y)
    PixelRect(right - width, top, width, metrics.toPixelY(contentRect.bottom) - top)
