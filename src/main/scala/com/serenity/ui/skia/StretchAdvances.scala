package com.serenity.ui.skia

import com.serenity.ui.layout.GlyphAdvances
import org.jetbrains.skia.Font

/** What one stretch's shaping says about each of its characters: its nominal advance, and whether shaping leaves it
  * that advance in a glyph of its own. The stretch has no surrogate pairs, which the measurer leaves unmeasured.
  */
private[skia] object StretchAdvances:

  private val MaxMeasurableRun = 4

  /** The glyph a font gives a character it has none for. A character set in it is laid out in the font the draw path
    * falls back to, which only the drawn stretch knows, so its advance is not the nominal one.
    */
  private val MissingGlyph: Short = 0

  /** Whether `char` has the glyph it is measured by: a control or a space draws nothing, so having none is not a
    * missing glyph.
    */
  private def hasGlyphOrNeedsNone(char: Char, glyph: Short): Boolean =
    glyph != MissingGlyph || Character.isISOControl(char) || Character.isWhitespace(char) ||
      Character.isSpaceChar(char)

  def measure(
    chars: String,
    font: Font,
    offset: Int,
    advances: Array[Float],
    kernAfter: Array[Float],
    measurable: Array[Boolean],
    shapeOf: (String, Font) => ShapedGlyphs,
    pairKerning: (Font, Char, Char) => Float
  ): Unit =
    if chars.nonEmpty then
      val length = chars.length
      // Surrogate-free, so each char is its own code point; Skiko's string overload builds them through a Kotlin sequence.
      val nominalGlyphs = font.getUTF32Glyphs(chars.toCharArray.map(_.toInt))
      val nominal       = font.getWidths(nominalGlyphs)
      val shapedStretch = shapeOf(chars, font)
      val clusters      = shapedStretch.clusters
      val glyphCount    = new Array[Int](length)
      val glyphOf       = Array.fill(length)(-1)
      val firstGlyphOf  = Array.fill(length)(-1)
      def xAt(glyph: Int): Float =
        if glyph < shapedStretch.xs.length then shapedStretch.xs(glyph) else shapedStretch.width
      @annotation.tailrec
      def index(glyph: Int): Unit =
        if glyph < clusters.length then
          val char = clusters(glyph)
          if char >= 0 && char < length then
            glyphCount(char) += 1
            glyphOf(char) = glyph
            if firstGlyphOf(char) < 0 then firstGlyphOf(char) = glyph
          index(glyph + 1)
      index(0)

      /** The glyph of a character that has one to itself, or -1. */
      def ownGlyph(character: Int): Int =
        if glyphCount(character) != 1 then -1
        else
          val glyph = glyphOf(character)
          val after = if glyph + 1 < clusters.length then clusters(glyph + 1) else length
          if after == character + 1 then glyph else -1
      val array     = chars.toCharArray
      val anyLayout = GlyphAdvances.requiresLayout(array, 0, length)
      def plainAlone(character: Int): Boolean =
        hasGlyphOrNeedsNone(array(character), nominalGlyphs(character)) &&
          GlyphAdvances.isContextFreeCharacter(array(character)) &&
          !(anyLayout && GlyphAdvances.requiresLayout(array, character, character + 1))
      @annotation.tailrec
      def measureFrom(character: Int): Unit =
        if character < length then
          advances(offset + character) = nominal(character)
          val glyph = ownGlyph(character)
          if glyph >= 0 &&
              plainAlone(character) &&
              (nominal(character) == 0.0f || nominal(character) >= GlyphAdvances.SmallestDistinctAdvancePx)
          then
            val shift = xAt(glyph + 1) - xAt(glyph) - nominal(character)
            if math.abs(shift) <= GlyphAdvances.ShapingTolerancePx then measurable(offset + character) = true
            else if character + 1 < length && ownGlyph(character + 1) >= 0 then
              val pair = pairKerning(font, array(character), array(character + 1))
              if pair != 0.0f && math.abs(shift - pair) <= GlyphAdvances.ShapingTolerancePx then
                measurable(offset + character) = true
                kernAfter(offset + character) = pair
          measureFrom(character + 1)
      measureFrom(0)
      measurableRuns(
        array,
        nominal,
        (character: Int) => plainAlone(character) && nominal(character) >= GlyphAdvances.SmallestDistinctAdvancePx,
        (glyph: Int) => xAt(glyph),
        firstGlyphOf,
        shapedStretch.width,
        offset,
        measurable
      )

  /** Short runs of characters with equal advances that shaping sets exactly where their advances put them end to end,
    * though not each at its own: a ligature such as `=>` in a monospaced code font, or glyphs a contextual alternate
    * widens and narrows by the same amount. Spread evenly across the run, which is how carets inside a ligature are
    * placed, they are the characters' own advances, so a row can be measured from them as if shaping had not touched
    * the run.
    */
  private def measurableRuns(
    chars: Array[Char],
    nominal: Array[Float],
    plain: Int => Boolean,
    xAt: Int => Float,
    firstGlyphOf: Array[Int],
    totalWidth: Float,
    offset: Int,
    measurable: Array[Boolean]
  ): Unit =
    val length = chars.length
    def originOf(character: Int): Float =
      if character == length then totalWidth
      else if firstGlyphOf(character) >= 0 then xAt(firstGlyphOf(character))
      else Float.NaN

    /** Whether `first until end` is a run of plain characters of one advance that shaping spreads evenly. */
    @annotation.tailrec
    def evenRun(first: Int, end: Int, at: Int, sum: Float): Boolean =
      if at == end then
        val from = originOf(first)
        val to   = originOf(end)
        !from.isNaN && !to.isNaN && math.abs(to - from - sum) <= GlyphAdvances.ShapingTolerancePx * (end - first)
      else
        plain(at) && math.abs(nominal(at) - nominal(first)) <= GlyphAdvances.ShapingTolerancePx &&
        evenRun(first, end, at + 1, sum + nominal(at))
    @annotation.tailrec
    def runEnd(first: Int, candidate: Int): Int =
      if candidate > math.min(first + MaxMeasurableRun, length) then -1
      else if evenRun(first, candidate, first, 0.0f) then candidate
      else runEnd(first, candidate + 1)
    @annotation.tailrec
    def scan(first: Int): Unit =
      if first < length then
        if !measurable(offset + first) && plain(first) then
          val end = runEnd(first, first + 2)
          if end >= 0 then markMeasurable(first, end)
        scan(first + 1)
    @annotation.tailrec
    def markMeasurable(index: Int, until: Int): Unit =
      if index < until then
        measurable(offset + index) = true
        markMeasurable(index + 1, until)
    scan(0)
