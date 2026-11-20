package com.serenity.ui.layout

import java.awt.Font

import com.serenity.ui.layout.TextCaretMeasurement.LineFontResolver
import com.serenity.ui.layout.TextMeasurer.LineFonts

/** Each character's glyph advance over a span of text, read from one shaping per font run instead of a shaping per row.
  * A character is context-free when its advance is the same however the text around it is cut into rows: the measurer
  * found it in its own glyph at its own advance, and no kerning pair, ligature, contextual substitution, complex
  * script, surrogate, format or control character (except tab) touches it. Carets over a context-free span are plain
  * sums of advances taken from the span's start, so a row cut out of a measured paragraph has exactly the carets the
  * row measures alone. A character whose only context is a kerning pair with its successor is not context-free but is
  * measurable when the measurer keeps the pair's kerning beside its advance, which is all a row cut out of the
  * paragraph needs to find its carets.
  */
final private[ui] class GlyphAdvances private (
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

  /** Each character's caret in `[from, from + count)`, plus the trailing edge, with `from` at 0. A caret sits where the
    * glyph of its character is drawn, so a kerned pair's kerning is already in the caret of the second character; the
    * trailing edge is the sum of the advances, a last character feeling no kerning from a successor the row does not
    * hold.
    */
  def caretsFrom(from: Int, count: Int): IArray[Float] =
    val carets = new Array[Float](count + 1)
    @annotation.tailrec
    def sum(index: Int, xPx: Float): Unit =
      carets(index) = xPx
      if index < count then
        val kerning = if index + 1 < count then kernAfter(from + index) else 0.0f
        sum(index + 1, xPx + advances(from + index) + kerning)
    sum(0, 0.0f)
    IArray.unsafeFromArray(carets)

  /** Where each of `count` characters from `from` ends were the row to stop there: the carets without the kerning each
    * character's successor pulls, which a row that ends at the character does not feel.
    */
  def trailingEdgesFrom(from: Int, count: Int): IArray[Float] =
    val carets = caretsFrom(from, count)
    IArray.tabulate(count + 1)(index =>
      if index > 0 && index < count then carets(index) - kernAfter(from + index - 1) else carets(index)
    )

private[ui] object GlyphAdvances:

  /** Shaped and nominal advances closer than this are the same advance read through different arithmetic. */
  private[ui] val ShapingTolerancePx = 0.01f

  /** Below this a non-zero advance could be mistaken for a collapsed caret by `normalizeCollapsedCarets`. */
  private[ui] val SmallestDistinctAdvancePx = 0.05f

  /** Advances a backend measured: `advances` and `kernAfter` per character, and per character whether it is
    * context-free and whether it is measurable (see [[measure]]).
    */
  def of(
    advances: Array[Float],
    contextFree: Array[Boolean],
    kernAfter: Array[Float],
    measurable: Array[Boolean]
  ): GlyphAdvances =
    def countBefore(flags: Array[Boolean]): Array[Int] =
      val counts = new Array[Int](flags.length + 1)
      @annotation.tailrec
      def fill(index: Int): Unit =
        if index < flags.length then
          counts(index + 1) = if flags(index) then counts(index) + 1 else counts(index)
          fill(index + 1)
      fill(0)
      counts
    new GlyphAdvances(advances, countBefore(contextFree), kernAfter, countBefore(measurable))

  /** Whether the characters `[from, until)` of `chars` need more than one glyph per character to be laid out. */
  private[ui] def requiresLayout(chars: Array[Char], from: Int, until: Int): Boolean =
    Font.textRequiresLayout(chars, from, until)

  def hasContextFreeCharacters(text: String): Boolean =
    !Font.textRequiresLayout(text.toCharArray, 0, text.length) && text.forall(isContextFreeCharacter)

  /** A format character (zero-width space, joiner) is shaped with its neighbours, not on its own. */
  private[ui] def isContextFreeCharacter(char: Char): Boolean =
    Character.getType(char) != Character.FORMAT && !Character.isSurrogate(char) &&
      (char == '\t' || !Character.isISOControl(char))

  /** Advances of `text[from, until)`, indexed from `from`; `absoluteStartColumn` is the resolver column of `text(0)`.
    * Each of the resolver's runs is measured on its own, as it is shaped on its own when drawn. A hidden column has no
    * advance and a column with an extra advance has that much more, which is the same however the text around it is cut
    * into rows.
    */
  def measure(
    text: String,
    from: Int,
    until: Int,
    absoluteStartColumn: Int,
    resolver: LineFontResolver,
    measurer: TextMeasurer
  ): GlyphAdvances =
    val chars       = text.substring(from, until).toCharArray
    val advances    = new Array[Float](chars.length)
    val kernAfter   = new Array[Float](chars.length)
    val measurable  = new Array[Boolean](chars.length)
    val contextFree = new Array[Boolean](chars.length)
    val origin      = absoluteStartColumn + from
    resolver.fontRuns(origin, origin + chars.length).foreach { span =>
      val start = span.startColumn - origin
      val end   = span.endColumn - origin
      if span.hidden then
        java.util.Arrays.fill(contextFree, start, end, true)
        java.util.Arrays.fill(measurable, start, end, true)
      else
        val run = measurer.advances(text, from + start, from + end, 0, LineFonts(span.font, Vector.empty))
        System.arraycopy(run.advances, 0, advances, start, end - start)
        System.arraycopy(run.kernAfter, 0, kernAfter, start, end - start)
        (0 until end - start).foreach { index =>
          contextFree(start + index) = run.isContextFree(index, index + 1)
          measurable(start + index) = run.isMeasurable(index, index + 1)
        }
    }
    WideCells.snapAdvances(chars, advances, resolver.wideCellPx, index => resolver.isFallbackAt(origin + index))
    resolver.extraAdvances.foreach {
      case (column, extraPx) =>
        if column >= origin && column < origin + chars.length then advances(column - origin) += extraPx
    }
    of(advances, contextFree, kernAfter, measurable)
