package com.serenity.ui.renderer

import java.awt.font.TextLayout
import java.util.concurrent.ConcurrentHashMap

import com.serenity.config.{AppConfig, DockedCornerStyle}
import com.serenity.state.models.AppState
import com.serenity.ui.fonts.FontLoader
import com.serenity.ui.layout.{CloseReveal, TabBarSettings, TabTitleMeasure, TextLayoutSnapshot}

/** The tab strip's layout settings for a state, built the same way for the renderer, accessibility, and click and drag
  * hit testing, so all four lay the strip out identically.
  *
  * A GUI measures a title in the UI font the renderer draws with, which is `FontLoader.previewUiFont` of the config
  * (`loadUiFont` is that and nothing else), against the code font's cell width, and counts the text inset the title is
  * drawn with. A cell-grid frontend counts characters. Which close boxes show is the active tab's alone, plus the one
  * under the pointer in a GUI; the accessibility tree asks for every tab's.
  */
object TabBarLayoutSettings:

  def of(state: AppState, revealAll: Boolean = false, measures: TabBarMeasures = new TabBarMeasures): TabBarSettings =
    val config = state.persisted.config
    val gui    = !state.runtime.capabilities.isCellGrid
    val cellWidth = if gui then measures.cellWidthPx(config) else 0.0
    TabBarSettings(
      maxTabWidth = config.surfaceConfig.tabMaxWidth,
      measure = if gui then proportional(config, cellWidth) else TabTitleMeasure.CellGrid,
      reveal = if revealAll then CloseReveal.All else revealFor(state),
      edgeInsetCells = if gui then edgeInsetCells(config, cellWidth) else 0
    )

  /** The cells at each end of a floating strip that its arcs cut into, which nothing square may be painted in. */
  private def edgeInsetCells(config: AppConfig, cellWidth: Double): Int =
    val radius = config.scaledUiCornerRadiusPx
    if radius <= 0 || config.dockedCornerStyle != DockedCornerStyle.Floating then 0
    else math.ceil(radius / cellWidth).toInt

  private def revealFor(state: AppState): CloseReveal =
    if state.runtime.capabilities.isCellGrid then CloseReveal.ActiveOnly
    else state.runtime.pointerGesture.hoveredTab.fold(CloseReveal.ActiveOnly)(CloseReveal.Hovered(_))

  /** Each call measures afresh and remembers a title's advance only for the layout passes that use this measure, so the
    * object holds no state between calls and a font change needs no invalidation.
    */
  private def proportional(config: AppConfig, cellWidth: Double): TabTitleMeasure =
    val fontConfig = config.editorConfig.fontConfig
    val font       = FontLoader.previewUiFont(fontConfig)
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
