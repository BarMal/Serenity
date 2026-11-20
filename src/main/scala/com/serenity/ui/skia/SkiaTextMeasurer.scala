package com.serenity.ui.skia

import scala.jdk.CollectionConverters.*

import com.serenity.ui.layout.TextMeasurer.{FontRun, InkBounds, LineFonts, LineMetrics, MetricsKey, ShapedRun}
import com.serenity.ui.layout.{CellMetrics, CharWidth, GlyphAdvances, TextMeasurer}
import com.serenity.ui.renderer.{BackendCacheKey, BackendCaches, FontSpec}
import org.jetbrains.skia.shaper.{BidiRun, LanguageRun, ScriptRun, Shaper}
import org.jetbrains.skia.{Font, ManagedString, Paint}

/** [[TextMeasurer]] over Skia: HarfBuzz shaping and Skia font metrics, for the day layout measures with what draws
  * (#2660 item 2). Nothing measures with it yet; `TextMeasurerParitySpec` reports how it differs from AWT.
  *
  * Faces come from [[SkiaFontCache]], which picks a typeface from the spec's family, style and weight.
  *
  * Shaping takes one lock because a `Shaper` holds state between calls and the font manager is not documented as safe
  * for two threads' first lookups; the faces are built under [[SkiaFontCache]]'s own lock.
  */
final class SkiaTextMeasurer(
    faces: SkiaFontCache,
    stretches: SkiaShapedTextCache,
    bidiFallbacks: FallbackFonts = FallbackFonts.systemByFamily,
    memos: ShapingMemos = new ShapingMemos()
) extends TextMeasurer:

  def this(faces: SkiaFontCache) = this(faces, new SkiaShapedTextCache(ligaturesOf = faces.ligaturesOf))

  import SkiaTextMeasurer.*
  import ShapingMemos.{AdvancesKey, CutKey, PairKey, ShapedKey}

  private val shaperLock = new Object
  private val shapingN   = new java.util.concurrent.atomic.AtomicLong(0)
  private val inkPaint   = new Paint()
  private val shaper     = Shaper.Companion.make()

  /** How many times the shaper has run for this measurer. */
  private[ui] def shaperRuns: Long = shapingN.get

  def metricsKey: MetricsKey = MetricsKey("skia", Vector.empty)

  private def skia(font: FontSpec): Font = faces.faceFor(font).font

  def cellMetrics(font: FontSpec): CellMetrics = faces.cellMetrics(font)

  def measuresProportionally(font: FontSpec): Boolean =
    !isMonospaced(font) || font.ligatures || hasFractionalAdvanceDrift(font)

  private def isMonospaced(font: FontSpec): Boolean =
    val face = skia(font)
    List("i", "m", "W").map(sample => math.round(face.measureTextWidth(sample))).distinct.sizeIs == 1

  private def hasFractionalAdvanceDrift(font: FontSpec): Boolean =
    val measured = skia(font).measureTextWidth(DriftSample)
    math.abs(measured - cellMetrics(font).charWidth.toFloat * DriftSample.length) > 0.5f

  def itemise(text: String, from: Int, until: Int, startColumn: Int, fonts: LineFonts): Vector[FontRun] =
    fontsOver(fonts, startColumn + from, startColumn + until)

  def lineMetrics(fonts: LineFonts, startColumn: Int, endColumn: Int): LineMetrics =
    val spans = fontsOver(fonts, startColumn, endColumn)
    val cells = (if spans.isEmpty then Vector(fonts.base) else spans.map(_.font)).map(cellMetrics)
    LineMetrics(
      cells.foldLeft(1)((tallest, cell) => math.max(tallest, cell.lineHeight)),
      cells.foldLeft(1)((highest, cell) => math.max(highest, cell.ascent))
    )

  def glyphWidthPx(font: FontSpec, text: String): Float =
    if text.isEmpty then 0.0f else skia(font).measureTextWidth(text)

  def inkBounds(font: FontSpec, text: String): InkBounds =
    if text.isEmpty then InkBounds.Empty
    else
      val box = skia(font).measureText(text, inkPaint)
      InkBounds(box.getLeft, box.getTop, box.getRight, box.getBottom)

  def shape(text: String, startColumn: Int, run: FontRun, level: Byte): ShapedRun =
    val slice = text.substring(run.startColumn - startColumn, run.endColumn - startColumn)
    if slice.isEmpty then ShapedRun(run.font, level, IArray.empty, IArray(0.0f), IArray(0.0f), IArray.empty)
    else
      val collector = shaped(slice, skia(run.font), level)
      val glyphs    = collector.glyphs
      ShapedRun(
        run.font,
        level,
        IArray.from(glyphs.map(_.toInt)),
        IArray.from(collector.xs :+ collector.width),
        IArray.from(Array.tabulate(glyphs.length)(collector.yOf) :+ 0.0f),
        IArray.from(collector.clusters)
      )

  def rawCaretXs(text: String, startColumn: Int, fonts: LineFonts): IArray[Float] =
    if text.isEmpty then IArray(0.0f)
    else
      val byAdvances =
        Option.when(mayMeasureByAdvances(text))(advances(text, 0, text.length, startColumn, fonts))
      byAdvances
        .filter(_.isMeasurable(0, text.length))
        .fold(exactCaretXs(text, startColumn, fonts))(_.caretsFrom(0, text.length))

  def exactCaretXs(text: String, startColumn: Int, fonts: LineFonts): IArray[Float] =
    exactCaretsAndEdges(text, startColumn, fonts)._1

  override def exactCaretsAndEdges(text: String, startColumn: Int, fonts: LineFonts): (IArray[Float], IArray[Float]) =
    if text.isEmpty then (IArray(0.0f), IArray(0.0f))
    else
      val carets = new Array[Float](text.length + 1)
      val edges  = new Array[Float](text.length + 1)
      val end = fontsOver(fonts, startColumn, startColumn + text.length).foldLeft(0.0f) { (origin, run) =>
        val from            = run.startColumn - startColumn
        val until           = run.endColumn - startColumn
        val (local, pulled) = shapedCaretsAndPulls(text.substring(from, until), skia(run.font))
        (0 until until - from).foreach { index =>
          carets(from + index) = origin + local(index)
          edges(from + index) = origin + local(index) - pulled(index)
        }
        origin + local(until - from)
      }
      carets(text.length) = end
      edges(text.length) = end
      (IArray.unsafeFromArray(carets), IArray.unsafeFromArray(edges))

  def shapingCutAtOrAfter(text: String, from: Int, font: FontSpec): Int =
    (math.max(1, from) until text.length).find(isShapingCut(text, _, skia(font))).getOrElse(text.length)

  def shapingCutAtOrBefore(text: String, limit: Int, font: FontSpec): Int =
    (math.min(limit, text.length - 1) to 1 by -1).find(isShapingCut(text, _, skia(font))).getOrElse(0)

  override def isShapingCut(text: String, cut: Int, font: FontSpec): Boolean =
    cut > 0 && cut < text.length && isShapingCut(text, cut, skia(font))

  private[ui] def mayMeasureByAdvances(text: String): Boolean = GlyphAdvances.hasContextFreeCharacters(text)

  private[ui] def advances(
    text: String,
    from: Int,
    until: Int,
    startColumn: Int,
    fonts: LineFonts
  ): GlyphAdvances =
    val base = startColumn + from
    val runs = fontsOver(fonts, base, startColumn + until)
    val key = AdvancesKey(
      text.substring(from, until),
      runs.map(run => FontRun(run.startColumn - base, run.endColumn - base, run.font))
    )
    memos.advances.getOrCompute(key)(measureAdvances(text, from, until, base, runs))

  // What a paragraph's advances come to does not depend on the width it is wrapped at, so every wrap of it, at any
  // width, reads the measurement the first made for as long as any of them holds it.
  private def measureAdvances(text: String, from: Int, until: Int, base: Int, runs: Vector[FontRun]): GlyphAdvances =
    val count      = until - from
    val advances   = new Array[Float](count)
    val kernAfter  = new Array[Float](count)
    val measurable = new Array[Boolean](count)
    runs.foreach { run =>
      val start = run.startColumn - base
      val end   = run.endColumn - base
      measureRun(text.substring(from + start, from + end), skia(run.font), start, advances, kernAfter, measurable)
    }
    val contextFree = contextFreeOf(measurable, kernAfter)
    GlyphAdvances.of(advances, contextFree, kernAfter, measurable)

  /** A character's nominal advance, and whether shaping leaves it that advance in a glyph of its own: not part of a
    * ligature, not kerned (a variant glyph set at the nominal advance, which a contextual alternate may choose, is
    * still that advance), or kerned only by the pair it makes with the next character, as shaping that pair alone kerns
    * it, which is kept in `kernAfter`. A run with a surrogate pair is left unmeasured, as AWT's is.
    */
  private def measureRun(
    chars: String,
    font: Font,
    offset: Int,
    advances: Array[Float],
    kernAfter: Array[Float],
    measurable: Array[Boolean]
  ): Unit =
    if chars.exists(Character.isSurrogate) then
      surrogateFreeStretches(chars).foreach((from, until) =>
        measureRun(chars.substring(from, until), font, offset + from, advances, kernAfter, measurable)
      )
    else
      StretchAdvances.measure(
        chars,
        font,
        offset,
        advances,
        kernAfter,
        measurable,
        shaped(_, _, 0.toByte),
        pairKerning(_, _, _)
      )

  private def surrogateFreeStretches(chars: String): Vector[(Int, Int)] =
    val bounds = chars.indices.filter(index => Character.isSurrogate(chars(index)))
    val edges  = (-1 +: bounds) :+ chars.length
    edges.zip(edges.drop(1)).map((before, after) => (before + 1, after)).filter((from, until) => from < until).toVector

  /** How far `first` pulls `second` closer (negative) when the two are shaped alone, read once per face and pair: a
    * width read off a long run carries rounding that differs from run to run.
    */
  private def pairKerning(font: Font, first: Char, second: Char): Float =
    if memos.pairs.size >= MaxPairs then memos.pairs.clear()
    memos.pairs
      .computeIfAbsent(
        PairKey(faces.shapingKeyOf(font), first, second),
        _ =>
          val text      = s"$first$second"
          val collector = shaped(text, font, 0.toByte)
          val glyphs    = font.getStringGlyphs(text)
          val kerning =
            if collector.glyphs.length == 2 && collector.glyphs.sameElements(glyphs) then
              collector.xs(1) - font.getWidths(glyphs)(0)
            else 0.0f
          java.lang.Float.valueOf(kerning)
      )
      .floatValue

  private def contextFreeOf(measurable: Array[Boolean], kernAfter: Array[Float]): Array[Boolean] =
    val flags = new Array[Boolean](measurable.length)
    @annotation.tailrec
    def fill(index: Int): Unit =
      if index < flags.length then
        flags(index) = measurable(index) && kernAfter(index) == 0.0f
        fill(index + 1)
    fill(0)
    flags

  /** Whether shaping `text` in `font` reads nothing across `cut`: the two halves shaped alone have the glyphs and the
    * positions the whole has (a contextual alternate changes a glyph without moving it). Never inside a surrogate pair,
    * a combining mark or a joiner.
    */
  private def isShapingCut(text: String, cut: Int, font: Font): Boolean =
    val splitsCluster =
      (Character.isHighSurrogate(text(cut - 1)) && Character.isLowSurrogate(text(cut))) ||
        CharWidth.isCombiningMark(text.codePointAt(cut)) || Character.getType(text(cut)) == Character.FORMAT ||
        Character.getType(text(cut - 1)) == Character.FORMAT
    !splitsCluster && {
      if text.length > MaxCachedCutChars then shapingCutHolds(text, cut, font)
      else
        val key = CutKey(text, cut, faces.shapingKeyOf(font))
        Option(memos.cuts.get(key)).map(_.booleanValue).getOrElse {
          val decision = shapingCutHolds(text, cut, font)
          if memos.cuts.size >= MaxCachedCuts then memos.cuts.clear()
          val _ = memos.cuts.put(key, java.lang.Boolean.valueOf(decision))
          decision
        }
    }

  /** What was decided for a text, a cut in it and a font: the text around a cut is a few characters, and the same few
    * recur across a document, so the three shapings a decision costs are made once and, being of no use to anything
    * else, not kept.
    */
  private def shapingCutHolds(text: String, cut: Int, font: Font): Boolean =
    val whole = shaped(text, font, 0.toByte)
    val head  = shapeNow(text.substring(0, cut), font, 0.toByte)
    val tail  = shapeNow(text.substring(cut), font, 0.toByte)
    val glyphsHold =
      whole.glyphs.length == head.glyphs.length + tail.glyphs.length &&
        java.util.Arrays.equals(whole.glyphs, 0, head.glyphs.length, head.glyphs, 0, head.glyphs.length) &&
        java.util.Arrays.equals(
          whole.glyphs,
          head.glyphs.length,
          whole.glyphs.length,
          tail.glyphs,
          0,
          tail.glyphs.length
        )
    glyphsHold && {
      if SkiaShapedTextCache.shapedByLatinRuns(text) then
        // Latin runs are drawn from these very positions, so the glyphs agreeing glyph for glyph is the cut holding.
        val headGlyphs = head.xs.length
        @annotation.tailrec
        def positionsHold(glyph: Int): Boolean =
          glyph >= whole.xs.length ||
            (math.abs(
              whole.xs(glyph) - (if glyph < headGlyphs then head.xs(glyph)
                                 else tail.xs(glyph - headGlyphs) + head.width)
            ) <= CutTolerancePx && positionsHold(glyph + 1))
        math.abs(whole.width - head.width - tail.width) <= CutTolerancePx && positionsHold(0)
      else
        val wholeCarets = shapedCarets(text, font)
        val headCarets  = shapedCarets(text.substring(0, cut), font)
        val tailCarets  = shapedCarets(text.substring(cut), font)
        wholeCarets.indices.forall { index =>
          val rebuilt = if index <= cut then headCarets(index) else headCarets(cut) + tailCarets(index - cut)
          math.abs(wholeCarets(index) - rebuilt) <= CutTolerancePx
        }
    }

  /** [[shapedCarets]] and how far kerning pulled each character's glyph towards the glyphs before it, which a row that
    * ends before the character does not feel: its glyph's x less the furthest any earlier glyph reaches at its own
    * advance. Only text shaped here by Latin runs is told apart; a mark, a glyph inside a cluster and the first glyph
    * feel no pull.
    */
  private def shapedCaretsAndPulls(text: String, font: Font): (Array[Float], Array[Float]) =
    if text.isEmpty then (Array(0.0f), Array(0.0f))
    else if SkiaShapedTextCache.shapedByLatinRuns(text) then
      val origins = shapedOrigins(text, font)
      (spreadCarets(text, origins.starts, origins.end), origins.pulls)
    else if BidiShaping.shapesAsRuns(text) then
      val origins = bidiOrigins(text, font)
      (spreadCarets(text, origins.starts, origins.end), origins.pulls)
    else
      val (starts, end) = drawnOrigins(text, font)
      (spreadCarets(text, starts, end), new Array[Float](text.length + 1))

  /** The x of each character of `text` shaped alone, then the trailing edge. The characters of one cluster, a
    * ligature's say, share its glyphs: they are spread evenly from the cluster's x to the next one's, so each takes a
    * stop of its own and a part of the glyph can be selected, as AWT places them.
    */
  private def shapedCarets(text: String, font: Font): Array[Float] =
    if text.isEmpty then Array(0.0f)
    else
      val (starts, end) = clusterOrigins(text, font)
      spreadCarets(text, starts, end)

  private def spreadCarets(text: String, starts: Array[Float], end: Float): Array[Float] =
    val length = text.length
    val carets = new Array[Float](length + 1)
    @annotation.tailrec
    def nextOrigin(from: Int): Int = if from < length && starts(from).isNaN then nextOrigin(from + 1) else from
    @annotation.tailrec
    def spread(from: Int, index: Int, until: Int): Unit =
      if index < until then
        val startX = starts(from)
        val endX   = if until >= length then end else starts(until)
        carets(index) = startX + (endX - startX) * (index - from) / (until - from)
        spread(from, index + 1, until)
    @annotation.tailrec
    def fromOrigin(from: Int): Unit =
      if from < length then
        val until = nextOrigin(from + 1)
        spread(from, from, until)
        fromOrigin(until)
    fromOrigin(nextOrigin(0))
    carets(length) = end
    carets

  /** The characters that begin a cluster, as `text` is laid out when drawn. */
  private[ui] def clusterStarts(text: String, font: FontSpec): Vector[Int] =
    val (starts, _) = clusterOrigins(text, skia(font))
    starts.indices.filter(index => !starts(index).isNaN).toVector

  /** The x at which each character's cluster begins, NaN for a character inside one, and the trailing edge. Text the
    * render surface hands to Skiko's `TextLine` (emoji sequences, combining marks, other scripts) is measured from that
    * stretch, so the origins are where it draws; Latin text is shaped here the way the surface shapes it.
    */
  private def clusterOrigins(text: String, font: Font): (Array[Float], Float) =
    if SkiaShapedTextCache.shapedByLatinRuns(text) then
      val origins = shapedOrigins(text, font)
      (origins.starts, origins.end)
    else if BidiShaping.shapesAsRuns(text) then
      val origins = bidiOrigins(text, font)
      (origins.starts, origins.end)
    else drawnOrigins(text, font)

  /** One shaping of Latin `text`: where each character's cluster starts (NaN inside one), the trailing edge, and the
    * pull of each character's own glyph as [[shapedCaretsAndPulls]] reads it.
    */
  private def shapedOrigins(text: String, font: Font): ShapedOrigins =
    val collector = shaped(text, font, 0.toByte)
    val starts    = Array.fill(text.length)(Float.NaN)
    val pulls     = new Array[Float](text.length + 1)
    val nominal   = font.getWidths(collector.glyphs)
    val _ = collector.clusters.indices.foldLeft(Float.NegativeInfinity) { (reach, glyph) =>
      val cluster = collector.clusters(glyph)
      val x       = collector.xs(glyph)
      if cluster >= 0 && cluster < text.length then
        if starts(cluster).isNaN && glyph > 0 && cluster > 0 && nominal(glyph) > 0.0f then pulls(cluster) = x - reach
        if starts(cluster).isNaN || x < starts(cluster) then starts(cluster) = x
      math.max(reach, x + nominal(glyph))
    }
    ShapedOrigins(starts, collector.width, pulls)

  /** Where each character's caret goes in text that reads right to left somewhere: the edge its glyph starts at in
    * reading direction, which is the glyph's x for a left-to-right character and its right edge for a right-to-left
    * one, NaN for a character inside a cluster, and the trailing edge, which is the advance of the whole line. Nothing
    * pulls a character in such text, so a row that ends before it feels no kerning from it.
    */
  private def bidiOrigins(text: String, font: Font): ShapedOrigins =
    val collector = shaped(text, font, 0.toByte)
    val fonts     = BidiShaping.fontsOf(BidiShaping.fontRuns(text, font, bidiFallbacks))
    val nominal = collector.glyphs.indices.map { glyph =>
      val cluster = collector.clusters(glyph)
      fonts(math.max(0, math.min(cluster, fonts.length - 1))).getWidths(Array(collector.glyphs(glyph)))(0)
    }
    val starts = Array.fill(text.length)(Float.NaN)
    collector.clusters.indices.foreach { glyph =>
      val cluster = collector.clusters(glyph)
      if cluster >= 0 && cluster < text.length then
        val rightToLeft = collector.levelOf(glyph) % 2 == 1
        val edge        = if rightToLeft then collector.xs(glyph) + nominal(glyph) else collector.xs(glyph)
        val keep = starts(cluster).isNaN || (if rightToLeft then edge > starts(cluster) else edge < starts(cluster))
        if keep then starts(cluster) = edge
    }
    ShapedOrigins(starts, collector.width, new Array[Float](text.length + 1))

  /** Where `TextLine` puts each character of `text`, made once however many candidate rows ask: shaping a line this way
    * costs milliseconds, and a wrap measures the same rows again and again. What is kept is JVM arrays, so it costs no
    * native memory; long text and an over-full memo are shaped afresh, so it stays bounded.
    */
  private def drawnOrigins(text: String, font: Font): (Array[Float], Float) =
    if text.length > MaxCachedChars then shapedByDrawing(text, font)
    else
      val key = ShapedKey(text, faces.shapingKeyOf(font), 0.toByte)
      Option(memos.drawn.get(key)).getOrElse {
        val origins = shapedByDrawing(text, font)
        if memos.drawn.size >= MaxCachedDrawn then memos.drawn.clear()
        val _ = memos.drawn.put(key, origins)
        origins
      }

  private def shapedByDrawing(text: String, font: Font): (Array[Float], Float) =
    val edges = stretches.reading(text, font)(stretch =>
      (0 to text.length).map(index => stretch.leftEdgeOf(index).getOrElse(0.0f))
    )
    val starts = Array.tabulate(text.length) { index =>
      if index == 0 || edges(index) > edges(index - 1) then edges(index) else Float.NaN
    }
    (starts, edges(text.length))

  /** The shaping of `text` in `font`, made once however many windows ask for it: what a shaping holds is JVM arrays, so
    * keeping them costs no native memory. Long text and an over-full cache are shaped afresh, so it stays bounded.
    */
  private def shaped(text: String, font: Font, level: Byte): ShapedGlyphs =
    if text.length > MaxCachedChars then shapeNow(text, font, level)
    else
      val key = ShapedKey(text, faces.shapingKeyOf(font), level)
      Option(memos.shapings.get(key)).getOrElse {
        val collector = shapeNow(text, font, level)
        if memos.shapings.size >= MaxCachedShapings || memos.shapedChars.get + text.length > MaxCachedShapingChars then
          memos.shapings.clear()
          memos.shapedChars.set(0L)
        if Option(memos.shapings.put(key, collector)).isEmpty then
          val _ = memos.shapedChars.addAndGet(text.length.toLong)
        collector
      }

  private def shapeNow(text: String, font: Font, level: Byte): ShapedGlyphs =
    val collector = new GlyphCollector()
    // One run in the one font, as the draw path shapes Latin text (`shapeLatin`). The overload that finds the runs
    // itself builds ICU and font fallback iterators on every call, milliseconds however short the text, and resolves a
    // fallback typeface for each that native memory keeps until the heap is next collected; layout gives text its
    // fallback fonts itself. The native copy of the text is freed here for the same reason.
    val end    = text.length
    val asBidi = level == 0 && BidiShaping.shapesAsRuns(text)
    val _      = shapingN.incrementAndGet()
    shaperLock.synchronized {
      val managed = new ManagedString(text)
      try
        val _ = shaper.shape(
          managed,
          (if asBidi then BidiShaping.skiaFontRuns(BidiShaping.fontRuns(text, font, bidiFallbacks)).iterator
           else Iterator.single(org.jetbrains.skia.shaper.FontRun(end, font))).asJava,
          (if asBidi then BidiShaping.levelRuns(text).iterator
           else Iterator.single(BidiRun(end, level.toInt))).asJava,
          (if asBidi then BidiShaping.scriptRuns(text).iterator
           else Iterator.single(ScriptRun(end, scriptOf(text)))).asJava,
          Iterator.single(LanguageRun(end, "en")).asJava,
          ShapingFeatures.optionsFor(faces.ligaturesOf(font)),
          Float.MaxValue,
          collector
        )
      finally managed.close()
    }
    collector.frozen

object SkiaTextMeasurer:

  private val MaxCachedChars = 4096

  private val MaxCachedShapings = 4096

  /** Characters of text whose shapings are kept before the memo starts again. What a wrap needs of a paragraph is read
    * once into its advances, which the wrap keeps, so what this holds beyond the text being worked on is resident for
    * little.
    */
  private[skia] val MaxCachedShapingChars = 49152

  private val MaxCachedDrawn = 32768

  final private case class ShapedOrigins(starts: Array[Float], end: Float, pulls: Array[Float])

  /** Texts longer than a cut's few characters of context are decided afresh. */
  private val MaxCachedCutChars = 64

  private val MaxCachedCuts = 65536

  /** Pairs whose kerning is kept before the memo starts again: far more than any document's distinct pairs. */
  private val MaxPairs = 65536

  private val Key = BackendCacheKey(classOf[SkiaTextMeasurer])

  /** The measurer for the faces and stretches `owner`'s surfaces draw with, so a line is measured where it is drawn. An
    * owner has one, made the first time it is asked for.
    */
  def of(owner: BackendCaches): SkiaTextMeasurer =
    owner.getOrCreate(Key) {
      val caches = SkiaTextCaches.of(owner)
      new SkiaTextMeasurer(caches.fonts, caches.shapedText, memos = SkiaTextCaches.shapingsOf(owner))
    }

  /** What a window's layout is wired to measure with: the measurer of the owner's own faces and stretches. */
  def forLayout(owner: BackendCaches): Option[TextMeasurer] = Some(of(owner))

  private val DriftSample = "iiiiiiiiiiii"

  /** Positions of the same text shaped whole and in two parts that differ by less than this are the same positions. */
  private val CutTolerancePx = 0.001f

  /** Whether the Skiko in use still answers [[verticalMetrics]]; `SkiaTextMeasurerSpec` fails if it does not. */
  private[ui] def hasVerticalMetrics: Boolean = SkiaVerticalMetrics.available

  private[skia] def verticalMetrics(font: Font): (Float, Float, Float) = SkiaVerticalMetrics.of(font)

  private def scriptOf(text: String): String =
    text
      .codePoints()
      .iterator()
      .asScala
      .map(Character.UnicodeScript.of(_))
      .collectFirst {
        case Character.UnicodeScript.HEBREW => "Hebr"
        case Character.UnicodeScript.ARABIC => "Arab"
        case Character.UnicodeScript.LATIN  => "Latn"
      }
      .getOrElse("Latn")

  /** The fonts of `[start, end)`: each run over it, `fonts.base` in the gaps, neighbours in one font made one run. */
  private def fontsOver(fonts: LineFonts, start: Int, end: Int): Vector[FontRun] =
    if end <= start then Vector.empty
    else
      val (at, spans) = fonts.runs
        .filter(run => run.endColumn > start && run.startColumn < end)
        .sortBy(_.startColumn)
        .foldLeft((start, Vector.empty[FontRun])) {
          case ((at, spans), run) =>
            val from  = math.max(run.startColumn, at)
            val until = math.min(run.endColumn, end)
            if until <= from then (at, spans)
            else
              val gap = if from > at then Vector(FontRun(at, from, fonts.base)) else Vector.empty
              (until, spans ++ gap :+ FontRun(from, until, run.font))
        }
      val all = if at < end then spans :+ FontRun(at, end, fonts.base) else spans
      all.foldLeft(Vector.empty[FontRun]) { (merged, run) =>
        merged.lastOption match
          case Some(last) if last.font == run.font && last.endColumn == run.startColumn =>
            merged.dropRight(1) :+ last.copy(endColumn = run.endColumn)
          case _ => merged :+ run
      }
