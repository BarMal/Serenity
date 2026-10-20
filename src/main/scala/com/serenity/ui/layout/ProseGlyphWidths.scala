package com.serenity.ui.layout

import java.util.concurrent.ConcurrentHashMap
import java.util.concurrent.atomic.AtomicLong

import com.serenity.ui.fonts.FontLoader.FontConfig

/** The widths [[ProseColumn]] measures off a font configuration, kept for the state that asked: layout runs many times
  * a frame, measuring fonts is AWT work, and only a font change alters the answer.
  *
  * It has no parameters so that every one is equal to every other, as [[ChromeLayoutMemo]] is.
  */
final case class ProseGlyphWidths():

  private val byFont   = ConcurrentHashMap[FontConfig, ProseColumn.GlyphWidths]()
  private val measured = AtomicLong(0)

  /** How many font configurations were measured, as opposed to answered from what was kept. */
  def measurements: Long = measured.get

  private[layout] def of(config: FontConfig)(measure: => ProseColumn.GlyphWidths): ProseColumn.GlyphWidths =
    byFont.computeIfAbsent(
      config,
      _ =>
        val _ = measured.incrementAndGet()
        measure
    )
