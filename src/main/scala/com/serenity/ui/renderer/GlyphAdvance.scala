package com.serenity.ui.renderer

import java.awt.Font
import java.awt.font.FontRenderContext
import java.util.concurrent.ConcurrentHashMap
import java.util.concurrent.atomic.AtomicLong

/** A glyph's own advance, which `GlyphVector.getGlyphMetrics` also reports but only after measuring the glyph's
  * outline: a native call per glyph on macOS, which made measuring a page of text there cost minutes. A vector of the
  * one glyph ends where its advance does, and the answer is a function of font, render context and glyph alone, so it
  * is kept; `maxEntries` only stops a long session of ever-different keys from growing the map without limit.
  */
final class GlyphAdvance(maxEntries: Int = GlyphAdvance.DefaultMaxEntries):
  import GlyphAdvance.*

  private val advances  = new ConcurrentHashMap[Key, java.lang.Float]()
  private val measuredN = AtomicLong(0)

  /** How many glyphs were measured, as opposed to served from the map. */
  def measurements: Long = measuredN.get()

  def of(font: Font, frc: FontRenderContext, glyphCode: Int): Float =
    val key = Key(font, frc, glyphCode)
    if advances.size >= maxEntries && !advances.containsKey(key) then advances.clear()
    advances.computeIfAbsent(
      key,
      key =>
        val _ = measuredN.incrementAndGet(); measure(key)
    )

object GlyphAdvance:

  val DefaultMaxEntries: Int = 65536

  final private case class Key(font: Font, frc: FontRenderContext, glyphCode: Int)

  private def measure(key: Key): java.lang.Float =
    FontFirstUse.settle(key.font, key.frc)
    key.font.createGlyphVector(key.frc, Array(key.glyphCode)).getGlyphPosition(1).getX.toFloat
