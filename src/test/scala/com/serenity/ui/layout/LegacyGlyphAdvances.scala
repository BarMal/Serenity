package com.serenity.ui.layout

import java.awt.Font
import java.awt.font.{FontRenderContext, GlyphVector}

import com.serenity.ui.layout.TextCaretMeasurement.LineFontResolver

/** [[GlyphAdvances.measure]] as it was before advances were read from a per-font table: one glyph vector per font run
  * of every paragraph. Kept verbatim as the oracle the table must reproduce bit for bit.
  */
object LegacyGlyphAdvances:

  final case class Measured(advances: Array[Float], contextFree: Array[Boolean])

  private val ShapingTolerancePx      = 0.01f
  private val SmallestDistinctAdvance = 0.05f

  private def isContextFreeCharacter(char: Char): Boolean =
    Character.getType(char) != Character.FORMAT && !Character.isSurrogate(char) &&
      (char == '\t' || !Character.isISOControl(char))

  def measure(
    text: String,
    from: Int,
    until: Int,
    absoluteStartColumn: Int,
    resolver: LineFontResolver,
    frc: FontRenderContext
  ): Measured =
    val chars       = text.substring(from, until).toCharArray
    val advances    = new Array[Float](chars.length)
    val contextFree = new Array[Boolean](chars.length)
    val origin      = absoluteStartColumn + from
    resolver.fontRuns(origin, origin + chars.length).foreach { (runStart, runEnd, font) =>
      measureRun(chars, runStart - origin, runEnd - origin, font, frc, advances, contextFree)
    }
    Measured(advances, contextFree)

  private def measureRun(
    chars: Array[Char],
    start: Int,
    end: Int,
    font: Font,
    frc: FontRenderContext,
    advances: Array[Float],
    contextFree: Array[Boolean]
  ): Unit =
    val runChars = java.util.Arrays.copyOfRange(chars, start, end)
    val nominal  = font.createGlyphVector(frc, runChars)
    if nominal.getNumGlyphs == runChars.length then
      val codes = nominal.getGlyphCodes(0, runChars.length, new Array[Int](runChars.length))
      val memo  = GlyphAdvanceMemo(nominal)
      runChars.indices.foreach { index =>
        val advance = memo.advance(codes(index), index)
        advances(start + index) = advance
        contextFree(start + index) = isContextFreeCharacter(runChars(index)) &&
          !Font.textRequiresLayout(runChars, index, index + 1) &&
          (advance == 0.0f || advance >= SmallestDistinctAdvance)
      }
      if font.hasLayoutAttributes && (start until end).exists(contextFree(_)) then
        clearShapedCharacters(runChars, start, font, frc, codes, advances, contextFree)

  private def clearShapedCharacters(
    runChars: Array[Char],
    start: Int,
    font: Font,
    frc: FontRenderContext,
    nominalCodes: Array[Int],
    advances: Array[Float],
    contextFree: Array[Boolean]
  ): Unit =
    val count        = runChars.length
    val shaped       = font.layoutGlyphVector(frc, runChars, 0, count, Font.LAYOUT_LEFT_TO_RIGHT)
    val glyphs       = shaped.getNumGlyphs
    val codes        = shaped.getGlyphCodes(0, glyphs, new Array[Int](glyphs))
    val charIndices  = shaped.getGlyphCharIndices(0, glyphs, new Array[Int](glyphs))
    val positions    = shaped.getGlyphPositions(0, glyphs + 1, new Array[Float](2 * (glyphs + 1)))
    val glyphsByChar = new Array[Int](count)
    charIndices.foreach(char => if char >= 0 && char < count then glyphsByChar(char) += 1)
    val faithful = new Array[Boolean](count)
    (0 until glyphs).foreach { glyph =>
      val char    = charIndices(glyph)
      val inOrder = if glyph + 1 < glyphs then charIndices(glyph + 1) == char + 1 else char == count - 1
      if char >= 0 && char < count && glyphsByChar(char) == 1 && inOrder && codes(glyph) == nominalCodes(char) then
        val shapedAdvance = positions(2 * glyph + 2) - positions(2 * glyph)
        faithful(char) = positions(2 * glyph + 1) == 0.0f &&
          math.abs(shapedAdvance - advances(start + char)) <= ShapingTolerancePx
    }
    faithful.indices.foreach(char => if !faithful(char) then contextFree(start + char) = false)

  final private class GlyphAdvanceMemo(glyphs: GlyphVector):
    private val capacity = Integer.highestOneBit(math.max(16, math.min(glyphs.getNumGlyphs, 1024)) * 2)
    private val keys     = new Array[Int](capacity)
    private val values   = new Array[Float](capacity)
    private val filled   = new Array[Boolean](capacity)

    def advance(code: Int, glyphIndex: Int): Float =
      @annotation.tailrec
      def probe(slot: Int, remaining: Int): Float =
        if remaining == 0 then glyphs.getGlyphMetrics(glyphIndex).getAdvance
        else if !filled(slot) then
          val measured = glyphs.getGlyphMetrics(glyphIndex).getAdvance
          filled(slot) = true
          keys(slot) = code
          values(slot) = measured
          measured
        else if keys(slot) == code then values(slot)
        else probe((slot + 1) & (capacity - 1), remaining - 1)
      probe((code * 0x9e3779b9) >>> (32 - Integer.numberOfTrailingZeros(capacity)), capacity)
