package com.serenity.ui.layout

import com.serenity.state.models.AppState

/** Where an editor pane's scrollbar lies. In the GUI it is a thin strip over the right edge of the pane's content,
  * drawn on top of the text: it takes no room from the text, which wraps at the width it always did, and the text
  * beneath it is put back whenever the bar moves. A terminal cannot draw anything thinner than a cell, so there the
  * pane's last column belongs to the bar and the text wraps one column short of it.
  */
object ScrollbarLayout:

  /** In logical pixels, so the bar is as many device pixels wide as the display scales it to. */
  val StripWidthPx: Int = 6

  /** Narrower than this a pane is too small to carry a bar over. */
  private val MinPaneColumns: Int = 12

  /** A column-mode page is scrolled a page at a time, so it has no bar; nor has any pane when the user turned the
    * scrollbar off (`editor.scrollbar`).
    */
  private def barless(state: AppState): Boolean =
    val surfaceConfig = state.persisted.config.surfaceConfig
    !state.chrome.scrollbar || (surfaceConfig.columnModeEnabled && surfaceConfig.wordWrapEnabled)

  /** Whether the GUI draws a strip over a pane whose content is `contentRect`. */
  def shown(state: AppState, contentRect: LayoutRect): Boolean =
    !state.runtime.capabilities.isCellGrid && !barless(state) && contentRect.width >= MinPaneColumns

  /** The columns a terminal pane `bandWidth` wide gives its scrollbar. */
  def terminalColumns(state: AppState, bandWidth: Int): Int =
    if state.runtime.capabilities.isCellGrid && !barless(state) && bandWidth >= MinPaneColumns then 1 else 0

  /** Whether `layout`'s pane has a scrollbar, in the GUI or a terminal. */
  def hasBar(state: AppState, layout: EditorPaneLayout): Boolean =
    if state.runtime.capabilities.isCellGrid then layout.scrollbarRect.width > 0 else shown(state, layout.contentRect)

  /** Where `layout`'s bar lies in pixels of `metrics`: the strip over the content, or the terminal's reserved column.
    */
  def stripOf(layout: EditorPaneLayout, metrics: CellMetrics): PixelRect =
    if layout.scrollbarRect.width > 0 then
      val rect = layout.scrollbarRect
      val left = metrics.toPixelX(rect.x)
      val top  = metrics.toPixelY(rect.y)
      PixelRect(left, top, metrics.toPixelX(rect.right) - left, metrics.toPixelY(rect.bottom) - top)
    else strip(layout.contentRect, metrics)

  /** The strip over the right edge of `contentRect`, for the whole of its height. */
  def strip(contentRect: LayoutRect, metrics: CellMetrics): PixelRect =
    val width = StripWidthPx.min(metrics.charWidth).max(1)
    val right = metrics.toPixelX(contentRect.right)
    val top   = metrics.toPixelY(contentRect.y)
    PixelRect(right - width, top, width, metrics.toPixelY(contentRect.bottom) - top)
