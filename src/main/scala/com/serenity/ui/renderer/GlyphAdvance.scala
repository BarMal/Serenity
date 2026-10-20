package com.serenity.ui.renderer

import java.awt.Font
import java.awt.font.FontRenderContext
import java.util.concurrent.ConcurrentHashMap
import java.util.concurrent.atomic.{AtomicLong, AtomicLongArray}

/** A glyph's own advance, which `GlyphVector.getGlyphMetrics` also reports but only after measuring the glyph's
  * outline: a native call per glyph on macOS, which made measuring a page of text there cost minutes. A vector of the
  * one glyph ends where its advance does, and the answer is a function of font, render context and glyph alone, so it
  * is kept. A run or a stretch asks for its font's [[GlyphAdvance.Table]] once and reads every glyph from that; the
  * hash lookup on font and render context is paid per run, not per glyph. `maxTables` only stops a long session of
  * ever-different fonts from growing the map without limit.
  */
final class GlyphAdvance(maxTables: Int = GlyphAdvance.DefaultMaxTables):
  import GlyphAdvance.*

  private val tables    = new ConcurrentHashMap[TableKey, Table]()
  private val measuredN = AtomicLong(0)

  /** How many glyphs were measured, as opposed to served from a table. */
  def measurements: Long = measuredN.get()

  def table(font: Font, frc: FontRenderContext): Table =
    val key = TableKey(font, frc)
    if tables.size >= maxTables && !tables.containsKey(key) then tables.clear()
    tables.computeIfAbsent(key, key => Table(key.font, key.frc, measuredN))

  def of(font: Font, frc: FontRenderContext, glyphCode: Int): Float = table(font, frc).of(glyphCode)

object GlyphAdvance:

  val DefaultMaxTables: Int = 256

  private val SlotBits = 8
  private val Slots    = 1 << SlotBits
  private val Probes   = 8

  final private case class TableKey(font: Font, frc: FontRenderContext)

  /** The advances of one font's glyphs at one render context, in a fixed array of slots. A glyph lives in one of the
    * [[Probes]] slots from its hash on, and a slot holds the glyph code and its advance in one `Long`, so a reader sees
    * both or neither. A glyph with no room in its probes takes the place of the first, and is measured again when it is
    * next asked for: that costs time and never correctness.
    */
  final class Table private[GlyphAdvance] (font: Font, frc: FontRenderContext, measuredN: AtomicLong):
    private val slots = new AtomicLongArray(Slots)

    def of(glyphCode: Int): Float =
      val home = (glyphCode * 0x9e3779b1) >>> (32 - SlotBits)

      @annotation.tailrec
      def probe(step: Int): Float =
        val slot = (home + step) & (Slots - 1)
        val held = slots.get(slot)
        if held != 0L && (held >>> 32).toInt == glyphCode then java.lang.Float.intBitsToFloat(held.toInt)
        else if held == 0L || step == Probes - 1 then remember(if held == 0L then slot else home, glyphCode)
        else probe(step + 1)

      probe(0)

    private def remember(slot: Int, glyphCode: Int): Float =
      val advance = measure(glyphCode)
      slots.set(slot, (glyphCode.toLong << 32) | (java.lang.Float.floatToRawIntBits(advance) & 0xffffffffL))
      advance

    private def measure(glyphCode: Int): Float =
      val _ = measuredN.incrementAndGet()
      FontFirstUse.settle(font, frc)
      font.createGlyphVector(frc, Array(glyphCode)).getGlyphPosition(1).getX.toFloat
