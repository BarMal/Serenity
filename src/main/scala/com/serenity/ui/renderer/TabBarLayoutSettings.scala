package com.serenity.ui.renderer

import java.awt.font.TextLayout
import java.util.concurrent.ConcurrentHashMap

import com.serenity.config.AppConfig
import com.serenity.state.models.AppState
import com.serenity.ui.fonts.FontLoader
import com.serenity.ui.layout.{CellMetrics, CloseReveal, TabBarSettings, TabTitleMeasure, TextLayoutSnapshot}

/** The tab strip's layout settings for a state, built the same way for the renderer, accessibility, and click and drag
  * hit testing, so all four lay the strip out identically.
  *
  * A GUI measures a title in the UI font the renderer draws with, which is `FontLoader.previewUiFont` of the config
  * (`loadUiFont` is that and nothing else), against the code font's cell width, and counts the text inset the title is
  * drawn with. A cell-grid frontend counts characters. Which close boxes show is the active tab's alone, plus the one
  * under the pointer in a GUI; the accessibility tree asks for every tab's.
  */
object TabBarLayoutSettings:

  def of(state: AppState, revealAll: Boolean = false): TabBarSettings =
    TabBarSettings(
      maxTabWidth = state.persisted.config.surfaceConfig.tabMaxWidth,
      measure = measureFor(state),
      reveal = if revealAll then CloseReveal.All else revealFor(state)
    )

  private def revealFor(state: AppState): CloseReveal =
    if state.runtime.capabilities.isCellGrid then CloseReveal.ActiveOnly
    else state.runtime.pointerGesture.hoveredTab.fold(CloseReveal.ActiveOnly)(CloseReveal.Hovered(_))

  private def measureFor(state: AppState): TabTitleMeasure =
    if state.runtime.capabilities.isCellGrid then TabTitleMeasure.CellGrid
    else proportional(state.persisted.config)

  /** Each call measures afresh and remembers a title's advance only for the layout passes that use this measure, so the
    * object holds no state between calls and a font change needs no invalidation.
    */
  private def proportional(config: AppConfig): TabTitleMeasure =
    val fontConfig = config.editorConfig.fontConfig
    val font       = FontLoader.previewUiFont(fontConfig)
    val cellWidth  = CellMetrics.fromFont(FontLoader.previewCodeFont(fontConfig)).charWidth.toDouble
    val context    = TextLayoutSnapshot.defaultFontRenderContext()
    val advances   = new ConcurrentHashMap[String, java.lang.Double]()
    def advanceOf(label: String): Double =
      if label.isEmpty then 0.0
      else
        advances.computeIfAbsent(
          label,
          text => java.lang.Double.valueOf(TextLayout(text, font, context).getAdvance.toDouble)
        )
    TabTitleMeasure.proportional(advanceOf, SurfaceTextInset.px(config), cellWidth)
