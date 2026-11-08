package com.serenity.ui.renderer

import java.awt.font.TextLayout
import java.util.concurrent.atomic.{AtomicInteger, AtomicLong, AtomicReference}

import com.serenity.config.AppConfig
import com.serenity.ui.fonts.FontLoader
import com.serenity.ui.fonts.FontLoader.FontConfig
import com.serenity.ui.layout.{BoundedRefCache, CellMetrics, TextLayoutSnapshot}

/** What the tab strip's layout measures, remembered across frames: the code font's cell width, once per font settings,
  * and each title's advance in the UI font, for as many distinct titles as [[TabBarMeasures.TitleCapacity]].
  *
  * The cell width builds an image and a graphics context and a title's advance builds a `TextLayout`, which a frame
  * that lays the strip out every time cannot afford. One instance belongs to each render owner (`RenderCaches`); a
  * caller with none gets a fresh one and measures every time, to the same widths.
  */
final class TabBarMeasures:

  private val remembered = new AtomicReference[Option[(FontConfig, Double)]](None)
  private val measured   = AtomicLong(0)

  private val titles =
    new BoundedRefCache[(String, FontConfig), Double](new AtomicInteger(TabBarMeasures.TitleCapacity))
  private val titlesMeasured = AtomicLong(0)

  /** How many title advances were measured, as opposed to remembered. */
  def titleMeasurements: Long = titlesMeasured.get()

  /** How far `label` advances in the UI font the renderer draws with, which `previewUiFont` of `fontConfig` is. */
  def titleAdvancePx(label: String, fontConfig: FontConfig): Double =
    val key = (label, fontConfig)
    titles.get(key).getOrElse {
      val _       = titlesMeasured.incrementAndGet()
      val font    = FontLoader.previewUiFont(fontConfig)
      val context = TextLayoutSnapshot.defaultFontRenderContext()
      val advance = TextLayout(label, font, context).getAdvance.toDouble
      titles.put(key, advance)
      advance
    }

  /** How many times the width was measured, as opposed to remembered. */
  def measurements: Long = measured.get()

  def cellWidthPx(config: AppConfig): Double =
    val fontConfig = config.editorConfig.fontConfig
    remembered.get() match
      case Some((rememberedConfig, width)) if rememberedConfig == fontConfig => width
      case _ =>
        val _     = measured.incrementAndGet()
        val width = CellMetrics.fromFont(FontLoader.previewCodeFont(fontConfig)).charWidth.toDouble
        remembered.set(Some(fontConfig -> width))
        width

object TabBarMeasures:

  /** More distinct titles than any strip shows, so a session's tabs stay remembered while a title edited keystroke by
    * keystroke cannot grow the cache without bound.
    */
  val TitleCapacity: Int = 256
