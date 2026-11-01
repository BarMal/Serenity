package com.serenity.ui.renderer

import java.awt.font.TextLayout
import java.util.concurrent.ConcurrentHashMap
import java.util.concurrent.atomic.AtomicReference

import com.serenity.config.InterfaceDensity
import com.serenity.config.AppConfig
import com.serenity.state.models.AppState
import com.serenity.ui.fonts.FontLoader
import com.serenity.ui.fonts.FontLoader.FontConfig
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

  final private case class Key(fontConfig: FontConfig, density: InterfaceDensity)

  private val remembered = new AtomicReference[Option[(Key, TabTitleMeasure)]](None)

  private def measureFor(state: AppState): TabTitleMeasure =
    if state.runtime.capabilities.isCellGrid then TabTitleMeasure.CellGrid
    else
      val config = state.persisted.config
      val key    = Key(config.editorConfig.fontConfig, config.interfaceDensity)
      remembered.get() match
        case Some((rememberedKey, measure)) if rememberedKey == key => measure
        case _ =>
          val measure = proportional(config)
          remembered.set(Some((key, measure)))
          measure

  /** How many advances are kept before the table starts over: titles are typed and renamed, so it must not grow
    * without bound.
    */
  private val AdvanceCapacity = 512

  private def proportional(config: AppConfig): TabTitleMeasure =
    val fontConfig = config.editorConfig.fontConfig
    val font       = FontLoader.previewUiFont(fontConfig)
    val cellWidth  = CellMetrics.fromFont(FontLoader.previewCodeFont(fontConfig)).charWidth.toDouble
    val context    = TextLayoutSnapshot.defaultFontRenderContext()
    val advances   = new ConcurrentHashMap[String, java.lang.Double]()
    def advanceOf(label: String): Double =
      if label.isEmpty then 0.0
      else
        if advances.size() >= AdvanceCapacity then advances.clear()
        advances.computeIfAbsent(label, text => java.lang.Double.valueOf(TextLayout(text, font, context).getAdvance.toDouble))
    TabTitleMeasure.proportional(advanceOf, SurfaceTextInset.px(config), cellWidth)
