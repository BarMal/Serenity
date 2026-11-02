package com.serenity.ui.renderer

import java.util.concurrent.atomic.{AtomicLong, AtomicReference}

import com.serenity.config.AppConfig
import com.serenity.ui.fonts.FontLoader
import com.serenity.ui.fonts.FontLoader.FontConfig
import com.serenity.ui.layout.CellMetrics

/** The code font's cell width, which the tab strip's layout counts its titles against, measured once per font settings.
  *
  * Measuring it builds an image and a graphics context, which a frame that lays the strip out every time cannot afford.
  * One instance belongs to each render owner (`RenderCaches`); a caller with none gets a fresh one and measures every
  * time, to the same width.
  */
final class TabBarMeasures:

  private val remembered = new AtomicReference[Option[(FontConfig, Double)]](None)
  private val measured   = AtomicLong(0)

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
