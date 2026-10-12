package com.serenity.ui.layout

import java.awt.Font
import java.awt.font.{FontRenderContext, GlyphVector, TextAttribute, TextLayout}
import java.text.AttributedString
import java.util.concurrent.ConcurrentHashMap

import com.serenity.ui.layout.TextCaretMeasurement.LineFontResolver
import com.serenity.ui.renderer.{FontFirstUse, ShapedTextCache}

/** Each character's glyph advance over a span of text, read from one glyph vector per font run instead of a
  * `TextLayout` caret query per character. A character is context-free when its advance is the same however the text
  * around it is cut into rows: no kerning pair, ligature, contextual substitution, complex script, surrogate, format or
  * control character (except tab) touches it; in a font whose layout advance is not its summed advances (see
  * [[GlyphAdvances.layoutAdvanceMatchesSum]]) no character is. In a font whose every glyph has the same advance (see
  * [[GlyphAdvances.advancesAreUniform]]) a contextual substitution or ligature whose glyphs add up to the advances of
  * the characters they stand for does not count: Monaspace's "texture healing" alternates and its `//` and `=>`
  * ligatures change shapes, never widths. Carets over a context-free span are plain sums of advances taken from the
  * span's start, so a row cut out of a measured paragraph has exactly the carets the row measures alone. A character
  * whose only context is a kerning pair with its successor is not context-free but is measurable: the pair's kerning is
  * kept beside its advance, which is all a row cut out of the paragraph needs to find its carets.
  */
final private[layout] class GlyphAdvances private (
    private val advances: Array[Float],
    private val contextFreeBefore: Array[Int],
    private val kernAfter: Array[Float],
    private val measurableBefore: Array[Int]
):

  def length: Int = advances.length

  /** These advances with `[keep, tailFrom)` replaced by `middle`: the text before `keep` and from `tailFrom` on is kept
    * as measured, so both must be cuts that shaping does not read across (see [[ShapingBarriers]]).
    */
  def spliced(keep: Int, middle: GlyphAdvances, tailFrom: Int): GlyphAdvances =
    val tailLength = advances.length - tailFrom
    val tailAt     = keep + middle.length
    def join[A](whole: Array[A], inserted: Array[A], make: Int => Array[A]): Array[A] =
      val merged = make(tailAt + tailLength)
      System.arraycopy(whole, 0, merged, 0, keep)
      System.arraycopy(inserted, 0, merged, keep, middle.length)
      System.arraycopy(whole, tailFrom, merged, tailAt, tailLength)
      merged
    def joinCounts(counts: Array[Int], inserted: Array[Int]): Array[Int] =
      val merged    = new Array[Int](tailAt + tailLength + 1)
      val kept      = counts(keep)
      val middleEnd = kept + inserted(middle.length)
      System.arraycopy(counts, 0, merged, 0, keep + 1)
      (1 to middle.length).foreach(index => merged(keep + index) = kept + inserted(index))
      val tailShift = middleEnd - counts(tailFrom)
      (1 to tailLength).foreach(index => merged(tailAt + index) = tailShift + counts(tailFrom + index))
      merged
    new GlyphAdvances(
      join(advances, middle.advances, new Array[Float](_)),
      joinCounts(contextFreeBefore, middle.contextFreeBefore),
      join(kernAfter, middle.kernAfter, new Array[Float](_)),
      joinCounts(measurableBefore, middle.measurableBefore)
    )

  def isContextFree(from: Int, until: Int): Boolean =
    contextFreeBefore(until) - contextFreeBefore(from) == until - from

  /** Whether a row cut out of `[from, until)` has the carets of these advances: every character is context-free, or
    * only kerned (see [[GlyphAdvances.measure]]). Carets of kerned characters are read with [[caretsFrom]].
    */
  def isMeasurable(from: Int, until: Int): Boolean =
    measurableBefore(until) - measurableBefore(from) == until - from

  /** How far each of `count` characters from `from` pulls the next one closer (negative) or pushes it away. */
  def kerningsFrom(from: Int, count: Int): IArray[Float] =
    IArray.unsafeFromArray(java.util.Arrays.copyOfRange(kernAfter, from, from + count))

  /** Each character's leading caret in `[from, from + count)`, plus the trailing edge, with `from` at 0. `TextLayout`
    * puts a kerned character's leading caret half way between where the kerning moved it and where it would have been
    * without, so the carets between characters read half the kerning of the one before; the trailing edge is the sum of
    * the advances, a last character feeling no kerning from a successor the row does not hold.
    */
  def caretsFrom(from: Int, count: Int): IArray[Float] =
    val carets = new Array[Float](count + 1)
    @annotation.tailrec
    def sum(index: Int, xPx: Float): Unit =
      carets(index) = if index > 0 && index < count then xPx - kernAfter(from + index - 1) / 2.0f else xPx
      if index < count then
        val kerning = if index + 1 < count then kernAfter(from + index) else 0.0f
        sum(index + 1, xPx + advances(from + index) + kerning)
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

  /** Whether `TextLayout` ends a row where the shaped glyphs' advances do, kerning included: a slanted face (a
    * transformed font, or the italic AWT synthesises when a family has none) makes `TextLayout.getAdvance` include the
    * slant overhang, so every row's trailing edge, and with it where rows wrap, would differ from the sum. Read off the
    * font once, since the font alone does not say whether its italic is synthesised.
    */
  def layoutAdvanceMatchesSum(font: Font, frc: FontRenderContext): Boolean =
    layoutAgreement.computeIfAbsent(
      Probe(font, frc),
      probe => SettledProbe(probeLayoutAdvance(probe.font, probe.frc)): java.lang.Boolean
    )

  private def probeLayoutAdvance(font: Font, frc: FontRenderContext): Boolean =
    FontFirstUse.settle(font, frc)
    val chars  = AgreementProbeText.toCharArray
    val glyphs = font.layoutGlyphVector(frc, chars, 0, chars.length, Font.LAYOUT_LEFT_TO_RIGHT)
    val summed = glyphs.getGlyphPosition(glyphs.getNumGlyphs).getX
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
      probe => SettledProbe(probeUniformAdvances(probe.font, probe.frc)): java.lang.Boolean
    )

  private def probeUniformAdvances(font: Font, frc: FontRenderContext): Boolean =
    FontFirstUse.settle(font, frc)
    val glyphs   = font.createGlyphVector(frc, UniformProbeText.toCharArray)
    val advances = (0 until glyphs.getNumGlyphs).map(glyphs.getGlyphMetrics(_).getAdvance)
    advances.maxOption.zip(advances.minOption).forall((widest, narrowest) => widest - narrowest <= ShapingTolerancePx)

  final private case class PairProbe(font: Font, frc: FontRenderContext, first: Char, second: Char)

  private val pairKernings = ConcurrentHashMap[PairProbe, java.lang.Float]()

  /** How far `first` pulls `second` closer (negative) when the two are shaped side by side. Read from the pair alone,
    * since a width taken from glyph positions along a long run carries rounding that differs from run to run.
    */
  private def pairKerning(font: Font, frc: FontRenderContext, first: Char, second: Char): Float =
    pairKernings.computeIfAbsent(
      PairProbe(font, frc, first, second),
      probe => SettledProbe(probePairKerning(probe)): java.lang.Float
    )

  private def probePairKerning(probe: PairProbe): Float =
    FontFirstUse.settle(probe.font, probe.frc)
    val chars   = Array(probe.first, probe.second)
    val nominal = probe.font.createGlyphVector(probe.frc, chars)
    val shaped  = probe.font.layoutGlyphVector(probe.frc, chars, 0, chars.length, Font.LAYOUT_LEFT_TO_RIGHT)
    if nominal.getNumGlyphs == chars.length && shaped.getNumGlyphs == chars.length then
      shaped.getGlyphPosition(1).getX.toFloat - nominal.getGlyphMetrics(0).getAdvance
    else 0.0f

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
    val kernAfter   = new Array[Float](chars.length)
    val measurable  = new Array[Boolean](chars.length)
    val contextFree = new Array[Boolean](chars.length)
    val origin      = absoluteStartColumn + from
    resolver.fontRuns(origin, origin + chars.length).foreach { span =>
      if span.hidden then
        java.util.Arrays.fill(contextFree, span.startColumn - origin, span.endColumn - origin, true)
        java.util.Arrays.fill(measurable, span.startColumn - origin, span.endColumn - origin, true)
      else
        measureRun(
          chars,
          span.startColumn - origin,
          span.endColumn - origin,
          span.font,
          frc,
          shapedText,
          advances,
          kernAfter,
          measurable,
          contextFree
        )
    }
    resolver.extraAdvances.foreach {
      case (column, extraPx) =>
        if column >= origin && column < origin + chars.length then advances(column - origin) += extraPx
    }
    def countBefore(flags: Array[Boolean]): Array[Int] =
      flags.scanLeft(0)((count, flag) => if flag then count + 1 else count)
    new GlyphAdvances(advances, countBefore(contextFree), kernAfter, countBefore(measurable))

  private def measureRun(
    chars: Array[Char],
    start: Int,
    end: Int,
    font: Font,
    frc: FontRenderContext,
    shapedText: ShapedTextCache,
    advances: Array[Float],
    kernAfter: Array[Float],
    measurable: Array[Boolean],
    contextFree: Array[Boolean]
  ): Unit =
    FontFirstUse.settle(font, frc)
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
        clearShapedCharacters(runChars, start, font, frc, shapedText, codes, advances, kernAfter, contextFree)
      java.lang.System.arraycopy(contextFree, start, measurable, start, end - start)
      (start until end).foreach(char => if kernAfter(char) != 0.0f then contextFree(char) = false)

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
    kernAfter: Array[Float],
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
          val onLine       = onBaseline(glyph, last)
          if unchanged && sameWidth && onLine then (first until until).foreach(faithful(_) = true)
          else if keepsGlyph && onLine && last + 1 < shaped.glyphCount && until < count then
            val pair = pairKerning(font, frc, runChars(first), runChars(until))
            if pair != 0.0f && math.abs(shapedWidth - nominalWidth - pair) <= ShapingTolerancePx then
              faithful(first) = true
              kernAfter(start + first) = pair
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
