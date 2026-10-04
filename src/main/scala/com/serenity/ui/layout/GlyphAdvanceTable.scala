package com.serenity.ui.layout

import java.awt.Font
import java.awt.font.FontRenderContext
import java.util.concurrent.ConcurrentHashMap
import java.util.concurrent.atomic.AtomicReference

/** One font's nominal glyph and advance for every context-free BMP character, measured on first use and then read as
  * primitives. A character is held here only when its advance is the same whatever surrounds it (see
  * [[GlyphAdvances]]), so an entry is a pure function of (font, render context, character): the glyph code and advance
  * a glyph vector of the font gives it in any run. Pages of 256 characters are allocated on demand; the whole table is
  * at most 512 KiB.
  *
  * Threads share it without ordering. Every cell is one `Int`, written whole, whose zero value means "not measured", so
  * a reader that races a writer (or a page's publication) sees either an unmeasured cell and measures the same value
  * itself, or the finished one. A cell holds the advance's float bits plus one, which no advance can make zero, and a
  * glyph code plus one.
  */
final private[layout] class GlyphAdvanceTable(font: Font, frc: FontRenderContext):
  import GlyphAdvanceTable.*

  private val pages = Array.fill(PageCount)(NoPage)

  /** The advance cell for `char` -- read it with [[advanceOf]] unless it [[isUnsuited]] -- measuring it if need be. */
  def advanceCell(char: Char): Int =
    val slots  = page(char >>> PageShift)
    val cached = slots(char & PageMask)
    if cached != Unmeasured then cached
    else
      measure(char, slots)
      slots(char & PageMask)

  /** The nominal glyph code of a character whose [[advanceCell]] is not unsuited. */
  def glyphCode(char: Char): Int =
    val slots  = page(char >>> PageShift)
    val cached = slots(PageSize + (char & PageMask))
    if cached != Unmeasured then cached - 1
    else
      measure(char, slots)
      slots(PageSize + (char & PageMask)) - 1

  private def page(index: Int): Array[Int] =
    val existing = pages(index)
    if existing ne NoPage then existing
    else
      val fresh = new Array[Int](2 * PageSize)
      pages(index) = fresh
      fresh

  private def measure(char: Char, slots: Array[Int]): Unit =
    val single  = Array(char)
    val glyphs  = font.createGlyphVector(frc, single)
    val advance = if glyphs.getNumGlyphs == 1 then glyphs.getGlyphMetrics(0).getAdvance else Float.NaN
    val code    = if glyphs.getNumGlyphs == 1 then glyphs.getGlyphCode(0) else NoGlyphCode
    val slot    = char & PageMask
    if code == NoGlyphCode || advance.isNaN || !GlyphAdvances.isContextFreeAdvance(char, single, advance) then
      slots(PageSize + slot) = 1
      slots(slot) = Unsuited
    else
      slots(PageSize + slot) = code + 1
      slots(slot) = java.lang.Float.floatToRawIntBits(advance) + 1

private[layout] object GlyphAdvanceTable:

  val Unmeasured: Int = 0

  /** The cell of a character needing the run's own measurement: the encoding of the float bits 1, a denormal no
    * context-free advance is.
    */
  val Unsuited: Int = 2

  private val NoGlyphCode = -1
  private val PageShift   = 8
  private val PageSize    = 1 << PageShift
  private val PageMask    = PageSize - 1
  private val PageCount   = 0x10000 >>> PageShift
  private val NoPage      = new Array[Int](0)

  /** True when the character must be measured with the rest of its run. */
  def isUnsuited(cell: Int): Boolean = cell == Unsuited

  def advanceOf(cell: Int): Float = java.lang.Float.intBitsToFloat(cell - 1)

  /** At most this many (font, render context) tables are kept by the shared cache; see [[GlyphAdvanceTableCache]]. */
  val MaxTables = 32

  val shared: GlyphAdvanceTableCache = GlyphAdvanceTableCache(MaxTables)

/** The tables of the (font, render context) pairs seen so far. When `maxTables` is reached, all are dropped and rebuilt
  * on demand, so a session cycling through many zoom levels never holds more than `maxTables * 512 KiB`. Thread-safe:
  * the dispatcher and render threads share it.
  */
final private[layout] class GlyphAdvanceTableCache(maxTables: Int):
  import GlyphAdvanceTableCache.*

  private val tables = new ConcurrentHashMap[Key, GlyphAdvanceTable]()
  private val recent = new AtomicReference[Option[Recent]](None)

  /** Fonts are built afresh per call by some callers, so equal fonts are different references; the one-entry front
    * cache answers the steady state (one font and render context per paragraph) without hashing either.
    */
  def forFont(font: Font, frc: FontRenderContext): GlyphAdvanceTable =
    recent.get match
      case Some(last) if (last.font eq font) && (last.frc eq frc) => last.table
      case _ =>
        val table = lookup(font, frc)
        recent.set(Some(Recent(font, frc, table)))
        table

  def size: Int = tables.size

  private def lookup(font: Font, frc: FontRenderContext): GlyphAdvanceTable =
    val key = Key(font, frc)
    Option(tables.get(key)).getOrElse {
      if tables.size >= maxTables then tables.clear()
      tables.computeIfAbsent(key, _ => GlyphAdvanceTable(font, frc))
    }

private object GlyphAdvanceTableCache:
  final private case class Key(font: Font, frc: FontRenderContext)
  final private class Recent(val font: Font, val frc: FontRenderContext, val table: GlyphAdvanceTable)
