package com.serenity.ui.layout

import java.awt.Font
import java.awt.font.{FontRenderContext, GlyphVector, TextAttribute, TextLayout}
import java.text.AttributedString
import java.util.concurrent.ConcurrentHashMap

import com.serenity.ui.layout.TextCaretMeasurement.LineFontResolver
import com.serenity.ui.renderer.ShapedTextCache

/** Each character's glyph advance over a span of text, read from one glyph vector per font run instead of a
  * `TextLayout` caret query per character. A character is context-free when its advance is the same however the text
  * around it is cut into rows: no kerning pair, ligature, contextual substitution, complex script, surrogate, format or
  * control character (except tab) touches it; in a font whose layout advance is not its summed advances (see
  * [[GlyphAdvances.layoutAdvanceMatchesSum]]) no character is. In a font whose every glyph has the same advance (see
  * [[GlyphAdvances.advancesAreUniform]]) a contextual substitution or ligature whose glyphs add up to the advances of
  * the characters they stand for does not count: Monaspace's "texture healing" alternates and its `//` and `=>`
  * ligatures change shapes, never widths. Carets over a context-free span are plain sums of advances taken from the
  * span's start, so a row cut out of a measured paragraph has exactly the carets the row measures alone.
  */
final private[layout] class GlyphAdvances private (
    private val advances: Array[Float],
    private val contextFreeBefore: Array[Int]
):

  def length: Int = advances.length

  /** These advances with `[keep, tailFrom)` replaced by `middle`: the text before `keep` and from `tailFrom` on is kept
    * as measured, so both must be cuts that shaping does not read across (see [[ShapingBarriers]]).
    */
  def spliced(keep: Int, middle: GlyphAdvances, tailFrom: Int): GlyphAdvances =
    val tailLength = advances.length - tailFrom
    val tailAt     = keep + middle.length
    val merged     = new Array[Float](tailAt + tailLength)
    System.arraycopy(advances, 0, merged, 0, keep)
    System.arraycopy(middle.advances, 0, merged, keep, middle.length)
    System.arraycopy(advances, tailFrom, merged, tailAt, tailLength)
    val keptFree   = contextFreeBefore(keep)
    val middleFree = keptFree + middle.contextFreeBefore(middle.length)
    val counts     = new Array[Int](merged.length + 1)
    System.arraycopy(contextFreeBefore, 0, counts, 0, keep + 1)
    (1 to middle.length).foreach(index => counts(keep + index) = keptFree + middle.contextFreeBefore(index))
    val tailShift = middleFree - contextFreeBefore(tailFrom)
    (1 to tailLength).foreach(index => counts(tailAt + index) = tailShift + contextFreeBefore(tailFrom + index))
    new GlyphAdvances(merged, counts)

  def isContextFree(from: Int, until: Int): Boolean =
    contextFreeBefore(until) - contextFreeBefore(from) == until - from

  /** Each character's leading caret in `[from, from + count)`, plus the trailing edge, with `from` at 0. */
  def caretsFrom(from: Int, count: Int): IArray[Float] =
    val carets = new Array[Float](count + 1)
    @annotation.tailrec
    def sum(index: Int, xPx: Float): Unit =
      carets(index) = xPx
      if index < count then sum(index + 1, xPx + advances(from + index))
    sum(0, 0.0f)
    IArray.unsafeFromArray(carets)

private[layout] object GlyphAdvances:

  /** Shaped and nominal advances closer than this are the same advance read through different arithmetic. */
  private val ShapingTolerancePx = 0.01f

  /** Below this a non-zero advance could be mistaken for a collapsed caret by `normalizeCollapsedCarets`. */
  private val SmallestDistinctAdvancePx = 0.05f

  final private case class Probe(font: Font, frc: FontRenderContext)

  private val layoutAgreement = ConcurrentHashMap[Probe, java.lang.Boolean]()

  private val AgreementProbeText = "Hoxi "

  /** Whether `TextLayout` ends a row where summed glyph advances do: a slanted face (a transformed font, or the italic
    * AWT synthesises when a family has none) makes `TextLayout.getAdvance` include the slant overhang, so every row's
    * trailing edge, and with it where rows wrap, would differ from the sum. Read off the font once, since the font
    * alone does not say whether its italic is synthesised.
    */
  def layoutAdvanceMatchesSum(font: Font, frc: FontRenderContext): Boolean =
    layoutAgreement.computeIfAbsent(
      Probe(font, frc),
      probe => probeLayoutAdvance(probe.font, probe.frc): java.lang.Boolean
    )

  private def probeLayoutAdvance(font: Font, frc: FontRenderContext): Boolean =
    val chars  = AgreementProbeText.toCharArray
    val glyphs = font.createGlyphVector(frc, chars)
    val summed = (0 until glyphs.getNumGlyphs).map(glyphs.getGlyphMetrics(_).getAdvance).sum
    val text   = AttributedString(AgreementProbeText)
    text.addAttribute(TextAttribute.FONT, font)
    math.abs(TextLayout(text.getIterator, frc).getAdvance - summed) <= ShapingTolerancePx

  private val uniformAdvances = ConcurrentHashMap[Probe, java.lang.Boolean]()

  private val UniformProbeText = "iIm.W0 "

  /** Whether every glyph of the font advances alike, as a monospaced face's do. Substituting one of its glyphs for
    * another cannot then change where the text after it starts, whichever way the text is cut into rows.
    */
  def advancesAreUniform(font: Font, frc: FontRenderContext): Boolean =
    uniformAdvances.computeIfAbsent(
      Probe(font, frc),
      probe => probeUniformAdvances(probe.font, probe.frc): java.lang.Boolean
    )

  private def probeUniformAdvances(font: Font, frc: FontRenderContext): Boolean =
    val glyphs   = font.createGlyphVector(frc, UniformProbeText.toCharArray)
    val advances = (0 until glyphs.getNumGlyphs).map(glyphs.getGlyphMetrics(_).getAdvance)
    advances.maxOption.zip(advances.minOption).forall((widest, narrowest) => widest - narrowest <= ShapingTolerancePx)

  def hasContextFreeCharacters(text: String): Boolean =
    !Font.textRequiresLayout(text.toCharArray, 0, text.length) && text.forall(isContextFreeCharacter)

  /** AWT measures a format character (zero-width space, joiner) differently at the end of a run than inside it. */
  private def isContextFreeCharacter(char: Char): Boolean =
    Character.getType(char) != Character.FORMAT && !Character.isSurrogate(char) &&
      (char == '\t' || !Character.isISOControl(char))

  /** Advances of `text[from, until)`, indexed from `from`; `absoluteStartColumn` is the resolver column of `text(0)`. A
    * hidden column has no advance and a column with an extra advance has that much more, which is the same however the
    * text around it is cut into rows.
    */
  def measure(
    text: String,
    from: Int,
    until: Int,
    absoluteStartColumn: Int,
    resolver: LineFontResolver,
    frc: FontRenderContext,
    shapedText: ShapedTextCache = ShapedTextCache.shared
  ): GlyphAdvances =
    val chars       = text.substring(from, until).toCharArray
    val advances    = new Array[Float](chars.length)
    val contextFree = new Array[Boolean](chars.length)
    val origin      = absoluteStartColumn + from
    resolver.fontRuns(origin, origin + chars.length).foreach { span =>
      if span.hidden then java.util.Arrays.fill(contextFree, span.startColumn - origin, span.endColumn - origin, true)
      else
        measureRun(
          chars,
          span.startColumn - origin,
          span.endColumn - origin,
          span.font,
          frc,
          shapedText,
          advances,
          contextFree
        )
    }
    resolver.extraAdvances.foreach {
      case (column, extraPx) =>
        if column >= origin && column < origin + chars.length then advances(column - origin) += extraPx
    }
    val contextFreeBefore = contextFree.scanLeft(0)((count, free) => if free then count + 1 else count)
    new GlyphAdvances(advances, contextFreeBefore)

  private def measureRun(
    chars: Array[Char],
    start: Int,
    end: Int,
    font: Font,
    frc: FontRenderContext,
    shapedText: ShapedTextCache,
    advances: Array[Float],
    contextFree: Array[Boolean]
  ): Unit =
    val runChars = java.util.Arrays.copyOfRange(chars, start, end)
    val nominal  = font.createGlyphVector(frc, runChars)
    if nominal.getNumGlyphs == runChars.length && layoutAdvanceMatchesSum(font, frc) then
      val codes = nominal.getGlyphCodes(0, runChars.length, new Array[Int](runChars.length))
      val memo  = GlyphAdvanceMemo(nominal)
      runChars.indices.foreach { index =>
        val advance = memo.advance(codes(index), index)
        advances(start + index) = advance
        contextFree(start + index) = isContextFreeCharacter(runChars(index)) &&
          !Font.textRequiresLayout(runChars, index, index + 1) &&
          (advance == 0.0f || advance >= SmallestDistinctAdvancePx)
      }
      if font.hasLayoutAttributes && (start until end).exists(contextFree(_)) then
        clearShapedCharacters(runChars, start, font, frc, shapedText, codes, advances, contextFree)

  /** With kerning or ligatures requested, AWT shapes the run; any character whose shaped glyph is not its own nominal
    * glyph at its nominal advance (a ligature, a kerning pair, a contextual substitution) needs per-row measurement. In
    * a uniform-advance font a substitution is not a reason, nor is a ligature that spans several characters: the glyphs
    * of a cluster of characters need only add up to those characters' own advances, and any row cut inside the cluster
    * then measures the characters alone at the same advances. The run is shaped through `shapedText`, so drawing the
    * same text finds it shaped.
    */
  private def clearShapedCharacters(
    runChars: Array[Char],
    start: Int,
    font: Font,
    frc: FontRenderContext,
    shapedText: ShapedTextCache,
    nominalCodes: Array[Int],
    advances: Array[Float],
    contextFree: Array[Boolean]
  ): Unit =
    val count                    = runChars.length
    val shaped                   = shapedText.stretchOf(String(runChars), font, frc)
    val substitutionKeepsAdvance = advancesAreUniform(font, frc)
    val faithful                 = new Array[Boolean](count)

    @annotation.tailrec
    def lastGlyphOfCluster(glyph: Int): Int =
      if glyph + 1 < shaped.glyphCount && shaped.charIndexAt(glyph + 1) == shaped.charIndexAt(glyph) then
        lastGlyphOfCluster(glyph + 1)
      else glyph

    def onBaseline(from: Int, last: Int): Boolean =
      (from to last).forall(shaped.yAt(_) == 0.0f)

    @annotation.tailrec
    def judgeClusters(glyph: Int): Unit =
      if glyph < shaped.glyphCount then
        val first = shaped.charIndexAt(glyph)
        val last  = lastGlyphOfCluster(glyph)
        val until = if last + 1 < shaped.glyphCount then shaped.charIndexAt(last + 1) else count
        if first >= 0 && until > first && until <= count then
          val nominalWidth = (first until until).foldLeft(0.0f)((sum, char) => sum + advances(start + char))
          val shapedWidth  = shaped.rightEdgeAt(last) - shaped.xAt(glyph)
          val ownGlyph     = last == glyph && until == first + 1
          val sameWidth    = math.abs(shapedWidth - nominalWidth) <= ShapingTolerancePx * (until - first)
          val keepsGlyph   = ownGlyph && shaped.glyphCodeAt(glyph) == nominalCodes(first)
          val unchanged    = if ownGlyph then keepsGlyph || substitutionKeepsAdvance else substitutionKeepsAdvance
          if unchanged && sameWidth && onBaseline(glyph, last) then (first until until).foreach(faithful(_) = true)
        judgeClusters(last + 1)

    judgeClusters(0)
    faithful.indices.foreach(char => if !faithful(char) then contextFree(start + char) = false)

  /** A glyph's advance depends only on the glyph, so each distinct glyph code in a run is read once. */
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
