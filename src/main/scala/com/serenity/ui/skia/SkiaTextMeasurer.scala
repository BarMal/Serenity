package com.serenity.ui.skia

import java.lang.invoke.{MethodHandle, MethodHandles, MethodType}

import scala.jdk.CollectionConverters.*
import scala.util.Try

import com.serenity.ui.fonts.FontLoader
import com.serenity.ui.layout.TextMeasurer.{FontRun, LineFonts, LineMetrics, MetricsKey, ShapedRun}
import com.serenity.ui.layout.{CellMetrics, CharWidth, GlyphAdvances, TextMeasurer}
import com.serenity.ui.renderer.FontSpec
import org.jetbrains.skia.Font
import org.jetbrains.skia.shaper.{BidiRun, LanguageRun, ScriptRun, Shaper, ShapingOptions}

/** [[TextMeasurer]] over Skia: HarfBuzz shaping and Skia font metrics, for the day layout measures with what draws
  * (#2660 item 2). Nothing measures with it yet; `TextMeasurerParitySpec` reports how it differs from AWT.
  *
  * Faces still come from [[SkiaFontCache]], which picks a typeface from the AWT font's family, style and the advance
  * AWT measures for it, so until the font handle stops being a `java.awt.Font` a number here is Skia shaping on the
  * typeface AWT's advance selects.
  *
  * Shaping takes one lock because a `Shaper` holds state between calls and the font manager is not documented as safe
  * for two threads' first lookups; the faces are built under [[SkiaFontCache]]'s own lock.
  */
final class SkiaTextMeasurer(faces: SkiaFontCache = new SkiaFontCache()) extends TextMeasurer:

  import SkiaTextMeasurer.*

  private val shaperLock = new Object
  private val shaper     = Shaper.Companion.make()
  private val options    = ShapingOptions.Companion.getDEFAULT()

  def metricsKey: MetricsKey = MetricsKey("skia", Vector.empty)

  private def skia(font: FontSpec): Font = faces.faceFor(font.toAwt).font

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
    val awt = font.toAwt
    !FontLoader.isMonospacedFont(awt) || FontLoader.ligaturesEnabled(awt) || hasFractionalAdvanceDrift(font)

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
        .filter(_.isContextFree(0, text.length))
        .fold(exactCaretXs(text, startColumn, fonts))(_.caretsFrom(0, text.length))

  def exactCaretXs(text: String, startColumn: Int, fonts: LineFonts): IArray[Float] =
    if text.isEmpty then IArray(0.0f)
    else
      val carets = new Array[Float](text.length + 1)
      val end = fontsOver(fonts, startColumn, startColumn + text.length).foldLeft(0.0f) { (origin, run) =>
        val from  = run.startColumn - startColumn
        val until = run.endColumn - startColumn
        val local = shapedCarets(text.substring(from, until), skia(run.font))
        (0 until until - from).foreach(index => carets(from + index) = origin + local(index))
        origin + local(until - from)
      }
      carets(text.length) = end
      IArray.unsafeFromArray(carets)

  def shapingCutAtOrAfter(text: String, from: Int, font: FontSpec): Int =
    (math.max(1, from) until text.length).find(isShapingCut(text, _, skia(font))).getOrElse(text.length)

  def shapingCutAtOrBefore(text: String, limit: Int, font: FontSpec): Int =
    (math.min(limit, text.length - 1) to 1 by -1).find(isShapingCut(text, _, skia(font))).getOrElse(0)

  private[ui] def mayMeasureByAdvances(text: String): Boolean = GlyphAdvances.hasContextFreeCharacters(text)

  private[ui] def advances(
    text: String,
    from: Int,
    until: Int,
    startColumn: Int,
    fonts: LineFonts
  ): GlyphAdvances =
    val count       = until - from
    val advances    = new Array[Float](count)
    val contextFree = new Array[Boolean](count)
    fontsOver(fonts, startColumn + from, startColumn + until).foreach { run =>
      val start = run.startColumn - (startColumn + from)
      val end   = run.endColumn - (startColumn + from)
      measureRun(text.substring(from + start, from + end), skia(run.font), start, advances, contextFree)
    }
    GlyphAdvances.of(advances, contextFree, new Array[Float](count), contextFree.clone())

  /** A character's nominal advance, and whether shaping leaves it that advance in its own glyph: not kerned, not part
    * of a ligature, not substituted. A run with a surrogate pair is left unmeasured, as AWT's is.
    */
  private def measureRun(
    chars: String,
    font: Font,
    offset: Int,
    advances: Array[Float],
    contextFree: Array[Boolean]
  ): Unit =
    if chars.nonEmpty && chars.codePointCount(0, chars.length) == chars.length then
      val nominalGlyphs = font.getStringGlyphs(chars)
      val nominal       = font.getWidths(nominalGlyphs)
      val collector     = shaped(chars, font, 0.toByte)
      val glyphs        = collector.glyphs
      val xs            = collector.xs :+ collector.width
      val clusters      = collector.clusters
      val glyphAt       = clusters.indices.groupBy(clusters(_))
      chars.indices.foreach { index =>
        advances(offset + index) = nominal(index)
        val own = glyphAt.getOrElse(index, IndexedSeq.empty)
        contextFree(offset + index) = own.toList.headOption.filter(_ => own.size == 1).exists { glyph =>
          val after = if glyph + 1 < clusters.length then clusters(glyph + 1) else chars.length
          after == index + 1 && glyphs(glyph) == nominalGlyphs(index) &&
          math.abs(xs(glyph + 1) - xs(glyph) - nominal(index)) <= GlyphAdvances.ShapingTolerancePx &&
          GlyphAdvances.isContextFreeCharacter(chars(index)) &&
          !java.awt.Font.textRequiresLayout(chars.toCharArray, index, index + 1) &&
          (nominal(index) == 0.0f || nominal(index) >= GlyphAdvances.SmallestDistinctAdvancePx)
        }
      }

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
        val wholeCarets = shapedCarets(text, font)
        val headCarets  = shapedCarets(text.substring(0, cut), font)
        val tailCarets  = shapedCarets(text.substring(cut), font)
        wholeCarets.indices.forall { index =>
          val rebuilt = if index <= cut then headCarets(index) else headCarets(cut) + tailCarets(index - cut)
          math.abs(wholeCarets(index) - rebuilt) <= CutTolerancePx
        }
      }
    }

  /** The x of each character of `text` shaped alone, then the trailing edge: a character inside a cluster takes the
    * cluster's x, as a ligature's characters do.
    */
  private def shapedCarets(text: String, font: Font): Array[Float] =
    if text.isEmpty then Array(0.0f)
    else
      val collector = shaped(text, font, 0.toByte)
      val starts    = Array.fill(text.length)(Float.NaN)
      collector.clusters.zip(collector.xs).foreach { (cluster, x) =>
        if cluster >= 0 && cluster < text.length && (starts(cluster).isNaN || x < starts(cluster)) then
          starts(cluster) = x
      }
      val filled = starts.scanLeft(0.0f)((previous, x) => if x.isNaN then previous else x).drop(1)
      filled :+ collector.width

  private def shaped(text: String, font: Font, level: Byte): GlyphCollector =
    val collector = new GlyphCollector()
    shaperLock.synchronized {
      if level % 2 == 0 then
        val _ = shaper.shape(text, font, options, Float.MaxValue, collector)
      else
        val end = text.length
        val _ = shaper.shape(
          text,
          Iterator.single(org.jetbrains.skia.shaper.FontRun(end, font)).asJava,
          Iterator.single(BidiRun(end, level.toInt)).asJava,
          Iterator.single(ScriptRun(end, scriptOf(text))).asJava,
          Iterator.single(LanguageRun(end, "en")).asJava,
          options,
          Float.MaxValue,
          collector
        )
    }
    collector

object SkiaTextMeasurer:

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
