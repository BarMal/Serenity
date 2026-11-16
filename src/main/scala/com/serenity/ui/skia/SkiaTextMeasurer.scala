package com.serenity.ui.skia

import java.lang.invoke.{MethodHandle, MethodHandles, MethodType}

import scala.jdk.CollectionConverters.*
import scala.util.Try

import com.serenity.ui.layout.TextMeasurer.{FontRun, InkBounds, LineFonts, LineMetrics, MetricsKey, ShapedRun}
import com.serenity.ui.layout.{CellMetrics, CharWidth, GlyphAdvances, TextMeasurer}
import com.serenity.ui.renderer.{BackendCacheKey, BackendCaches, FontSpec}
import org.jetbrains.skia.shaper.{BidiRun, LanguageRun, ScriptRun, Shaper}
import org.jetbrains.skia.{Font, ManagedString, Paint}

/** [[TextMeasurer]] over Skia: HarfBuzz shaping and Skia font metrics, for the day layout measures with what draws
  * (#2660 item 2). Nothing measures with it yet; `TextMeasurerParitySpec` reports how it differs from AWT.
  *
  * Faces come from [[SkiaFontCache]], which picks a typeface from the spec's family and style and, for a spec read from
  * an AWT font, the advance AWT measures for it, so until that reconciliation goes a number here is Skia shaping on the
  * typeface AWT's advance selects.
  *
  * Shaping takes one lock because a `Shaper` holds state between calls and the font manager is not documented as safe
  * for two threads' first lookups; the faces are built under [[SkiaFontCache]]'s own lock.
  */
final class SkiaTextMeasurer(
    faces: SkiaFontCache,
    stretches: SkiaShapedTextCache
) extends TextMeasurer:

  def this(faces: SkiaFontCache) = this(faces, new SkiaShapedTextCache(ligaturesOf = faces.ligaturesOf))

  def this() = this(new SkiaFontCache())

  import SkiaTextMeasurer.*

  private val shaperLock = new Object
  private val inkPaint   = new Paint()
  private val shaper     = Shaper.Companion.make()
  private lazy val certShaper = Shaper.Companion.make()
  private val certLock        = new Object

  def metricsKey: MetricsKey = MetricsKey("skia", Vector.empty)

  private def skia(font: FontSpec): Font = faces.faceFor(font).font

  def cellMetrics(font: FontSpec): CellMetrics =
    val skiaFont                   = skia(font)
    val (ascent, descent, leading) = verticalMetrics(skiaFont)
    CellMetrics(
      math.max(1, math.round(skiaFont.measureTextWidth("M"))),
      math.max(1, math.ceil((descent - ascent + leading).toDouble).toInt),
      math.max(1, math.ceil((-ascent).toDouble).toInt),
      displayWidthAware = true
    )

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
        IArray.fill(glyphs.length + 1)(0.0f),
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
    val count      = until - from
    val advances   = new Array[Float](count)
    val kernAfter  = new Array[Float](count)
    val measurable = new Array[Boolean](count)
    fontsOver(fonts, startColumn + from, startColumn + until).foreach { run =>
      val start = run.startColumn - (startColumn + from)
      val end   = run.endColumn - (startColumn + from)
      measureRun(text.substring(from + start, from + end), skia(run.font), start, advances, kernAfter, measurable)
    }
    val contextFree = Array.tabulate(count)(index => measurable(index) && kernAfter(index) == 0.0f)
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
    if chars.nonEmpty && chars.codePointCount(0, chars.length) == chars.length then
      val nominalGlyphs = font.getStringGlyphs(chars)
      val nominal       = font.getWidths(nominalGlyphs)
      val collector     = shaped(chars, font, 0.toByte)
      val xs            = collector.xs :+ collector.width
      val clusters      = collector.clusters
      val glyphCount    = new Array[Int](chars.length)
      val glyphOf       = Array.fill(chars.length)(-1)
      clusters.indices.foreach { glyph =>
        val char = clusters(glyph)
        if char >= 0 && char < chars.length then
          glyphCount(char) += 1
          glyphOf(char) = glyph
      }
      def ownGlyph(index: Int): Option[Int] =
        Option.when(glyphCount(index) == 1)(glyphOf(index)).filter { glyph =>
          val after = if glyph + 1 < clusters.length then clusters(glyph + 1) else chars.length
          after == index + 1
        }
      val array = chars.toCharArray
      chars.indices.foreach { index =>
        advances(offset + index) = nominal(index)
        val plain = GlyphAdvances.isContextFreeCharacter(chars(index)) &&
          !GlyphAdvances.requiresLayout(array, index, index + 1) &&
          (nominal(index) == 0.0f || nominal(index) >= GlyphAdvances.SmallestDistinctAdvancePx)
        ownGlyph(index).filter(_ => plain).foreach { glyph =>
          val shift = xs(glyph + 1) - xs(glyph) - nominal(index)
          if math.abs(shift) <= GlyphAdvances.ShapingTolerancePx then measurable(offset + index) = true
          else if index + 1 < chars.length && ownGlyph(index + 1).isDefined then
            val pair = pairKerning(font, chars(index), chars(index + 1))
            if pair != 0.0f && math.abs(shift - pair) <= GlyphAdvances.ShapingTolerancePx then
              measurable(offset + index) = true
              kernAfter(offset + index) = pair
        }
      }

  /** How far `first` pulls `second` closer (negative) when the two are shaped alone, read once per face and pair: a
    * width read off a long run carries rounding that differs from run to run.
    */
  private def pairKerning(font: Font, first: Char, second: Char): Float =
    if pairs.size >= MaxPairs then pairs.clear()
    pairs
      .computeIfAbsent(
        PairKey(font, first, second),
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

  private val pairs = new java.util.concurrent.ConcurrentHashMap[PairKey, java.lang.Float]()

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
      val whole      = shaped(text, font, 0.toByte)
      val head       = shaped(text.substring(0, cut), font, 0.toByte)
      val tail       = shaped(text.substring(cut), font, 0.toByte)
      val sameGlyphs = whole.glyphs.toVector == head.glyphs.toVector ++ tail.glyphs.toVector
      sameGlyphs && {
        if SkiaShapedTextCache.shapedByLatinRuns(text) then
          // Latin runs are drawn from these very positions, so the glyphs agreeing glyph for glyph is the cut holding.
          val rebuilt = head.xs.toVector ++ tail.xs.toVector.map(_ + head.width)
          math.abs(whole.width - head.width - tail.width) <= CutTolerancePx &&
          whole.xs.toVector.zip(rebuilt).forall((x, y) => math.abs(x - y) <= CutTolerancePx)
        else
          val wholeCarets = shapedCarets(text, font)
          val headCarets  = shapedCarets(text.substring(0, cut), font)
          val tailCarets  = shapedCarets(text.substring(cut), font)
          wholeCarets.indices.forall { index =>
            val rebuilt = if index <= cut then headCarets(index) else headCarets(cut) + tailCarets(index - cut)
            math.abs(wholeCarets(index) - rebuilt) <= CutTolerancePx
          }
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
    val origins = starts.indices.filter(index => !starts(index).isNaN)
    val spread = origins.zip(origins.drop(1) :+ text.length).flatMap { (from, until) =>
      val startX = starts(from)
      val endX   = if until >= text.length then end else starts(until)
      (from until until).map(index => startX + (endX - startX) * (index - from) / (until - from))
    }
    val leading = Array.fill(origins.headOption.getOrElse(text.length))(0.0f)
    (leading ++ spread) :+ end

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

  private def drawnOrigins(text: String, font: Font): (Array[Float], Float) =
    val edges = stretches.reading(text, font)(stretch =>
      (0 to text.length).map(index => stretch.leftEdgeOf(index).getOrElse(0.0f))
    )
    val starts = Array.tabulate(text.length) { index =>
      if index == 0 || edges(index) > edges(index - 1) then edges(index) else Float.NaN
    }
    (starts, edges(text.length))

  private def shaped(text: String, font: Font, level: Byte): GlyphCollector =
    val collector = new GlyphCollector()
    // One run in the one font, as the draw path shapes Latin text (`shapeLatin`). The overload that finds the runs
    // itself builds ICU and font fallback iterators on every call, milliseconds however short the text, and resolves a
    // fallback typeface for each that native memory keeps until the heap is next collected; layout gives text its
    // fallback fonts itself. The native copy of the text is freed here for the same reason.
    val end = text.length
    val (shapingLock, shapingShaper) =
      if com.serenity.ui.layout.ExpCert.offShaperLock then (certLock, certShaper) else (shaperLock, shaper)
    shapingLock.synchronized {
      val managed = new ManagedString(text)
      try
        val _ = shapingShaper.shape(
          managed,
          Iterator.single(org.jetbrains.skia.shaper.FontRun(end, font)).asJava,
          Iterator.single(BidiRun(end, level.toInt)).asJava,
          Iterator.single(ScriptRun(end, scriptOf(text))).asJava,
          Iterator.single(LanguageRun(end, "en")).asJava,
          ShapingFeatures.optionsFor(faces.ligaturesOf(font)),
          Float.MaxValue,
          collector
        )
      finally managed.close()
    }
    collector

object SkiaTextMeasurer:

  final private case class ShapedOrigins(starts: Array[Float], end: Float, pulls: Array[Float])

  final private case class PairKey(font: Font, first: Char, second: Char)

  /** Pairs whose kerning is kept before the memo starts again: far more than any document's distinct pairs. */
  private val MaxPairs = 65536

  private val Key = BackendCacheKey(classOf[SkiaTextMeasurer])

  /** The measurer for the faces and stretches `owner`'s surfaces draw with, so a line is measured where it is drawn. An
    * owner has one, made the first time it is asked for.
    */
  def of(owner: BackendCaches): SkiaTextMeasurer =
    owner.getOrCreate(Key) {
      val caches = SkiaTextCaches.of(owner)
      new SkiaTextMeasurer(caches.fonts, caches.shapedText)
    }

  private val DriftSample = "iiiiiiiiiiii"

  /** Positions of the same text shaped whole and in two parts that differ by less than this are the same positions. */
  private val CutTolerancePx = 0.001f

  /** Skiko hands its `FontMetrics` value class out as a float array under a name Scala cannot call, in the order top,
    * ascent, descent, bottom, leading.
    */
  private val MetricsOf: Option[MethodHandle] =
    Try(
      MethodHandles
        .publicLookup()
        .findVirtual(classOf[Font], "getMetrics-l-OL_C8", MethodType.methodType(classOf[Array[Float]]))
    ).toOption

  /** Whether the Skiko in use still answers [[verticalMetrics]]; `SkiaTextMeasurerSpec` fails if it does not. */
  private[ui] def hasVerticalMetrics: Boolean = MetricsOf.isDefined

  /** Skia's ascent (negative above the baseline), descent and leading for a font; without the Skiko accessor, the
    * proportions of a typical face, which the spec above catches before anyone measures with them.
    */
  private[skia] def verticalMetrics(font: Font): (Float, Float, Float) =
    MetricsOf
      .map { handle =>
        val answer: Object = handle.invoke(font)
        answer
      }
      .collect { case metrics: Array[Float] => (metrics(1), metrics(2), metrics(4)) }
      .getOrElse(
        (-0.9f * font.getSize, 0.25f * font.getSize, 0.0f)
      )

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
