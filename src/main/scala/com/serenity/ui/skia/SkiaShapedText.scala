package com.serenity.ui.skia

import java.lang.invoke.{MethodHandle, MethodHandles, MethodType}
import java.util.concurrent.ConcurrentHashMap

import scala.jdk.CollectionConverters.*
import scala.util.Try

import cats.syntax.all.*
import com.serenity.ui.renderer.ShapeTraffic
import org.jetbrains.skia.shaper.{BidiRun, FontRun, LanguageRun, RunHandler, RunInfo, ScriptRun, Shaper}
import org.jetbrains.skia.{Canvas, Font, Paint, Point, TextBlob, TextBlobBuilder, TextLine}

/** Text shaped once in one Skia font, from which any range of it can be drawn without shaping it again. The Skia
  * counterpart of [[ShapedStretch]]: a range keeps the glyphs of its characters where whole-text shaping put them, so a
  * range drawn at its own left edge continues the glyphs beside it, and a ligature belongs to the range holding its
  * first character.
  */
sealed trait SkiaShapedStretch:

  /** The shaped x of the glyph that starts at this character, `None` when no glyph does (inside a ligature). */
  def leftEdgeOf(charIndex: Int): Option[Float]

  /** Draws the glyphs of `[from, until)` with the first of them at `x`. */
  def draw(canvas: Canvas, from: Int, until: Int, x: Float, baseline: Float, paint: Paint): Unit

  /** Frees the native glyphs of a stretch nothing will draw again, rather than leaving them to the heap's collector. */
  private[skia] def release(): Unit

/** Latin-script text, shaped by HarfBuzz with caller-supplied runs: the glyphs and their positions stay on the JVM, so
  * a range is a blob built from a slice of them. `TextLine.make` and `Shaper.shape(text, font)` build ICU and font
  * fallback iterators on every call, about 3.7 ms each however short the text (#1812 spike), which these runs skip.
  */
final private class GlyphStretch(
    font: Font,
    textLength: Int,
    glyphs: Array[Short],
    xs: Array[Float],
    clusters: Array[Int],
    fontOfChar: Int => Font
) extends SkiaShapedStretch:

  private val MaxRanges = 64
  private val whole     = blobOf(glyphs.indices)
  private val ranges    = new ConcurrentHashMap[Long, Option[TextBlob]]()

  def leftEdgeOf(charIndex: Int): Option[Float] =
    clusters.indexOf(charIndex) match
      case -1    => None
      case glyph => Some(xs(glyph))

  def draw(canvas: Canvas, from: Int, until: Int, x: Float, baseline: Float, paint: Paint): Unit =
    if from <= 0 && until >= textLength then whole.foreach(canvas.drawTextBlob(_, x, baseline, paint))
    else
      if ranges.size >= MaxRanges then ranges.clear()
      ranges
        .computeIfAbsent((from.toLong << 32) | (until.toLong & 0xffffffffL), _ => rangeOf(from, until))
        .foreach(canvas.drawTextBlob(_, x, baseline, paint))

  private[skia] def release(): Unit =
    whole.foreach(_.close())
    ranges.values.forEach(_.foreach(_.close()))
    ranges.clear()

  private def rangeOf(from: Int, until: Int): Option[TextBlob] =
    val selected = glyphs.indices.filter(glyph => clusters(glyph) >= from && clusters(glyph) < until)
    blobOf(selected, selected.foldLeft(Float.MaxValue)((left, glyph) => left.min(xs(glyph))))

  private def blobOf(selected: Seq[Int], leftmost: Float = 0f): Option[TextBlob] =
    Option.when(selected.nonEmpty) {
      val fonts = selected.map(glyph => fontOfChar(clusters(glyph)))
      if fonts.forall(_ eq font) then
        TextBlob.Companion.makeFromPosH(
          selected.map(glyphs(_)).toArray,
          selected.map(xs(_) - leftmost).toArray,
          0f,
          font
        )
      else mixedBlob(selected, fonts, leftmost)
    }

  /** The glyphs of fallback fonts beside the base font's, one run of the blob for each stretch in one font. */
  @SuppressWarnings(Array("org.wartremover.warts.Null"))
  private def mixedBlob(selected: Seq[Int], fonts: Seq[Font], leftmost: Float): TextBlob =
    val builder = new TextBlobBuilder()
    val groups = selected.zip(fonts).foldLeft(Vector.empty[(Font, Vector[Int])]) {
      case (groups, (glyph, glyphFont)) =>
        groups.lastOption match
          case Some((previous, members)) if previous eq glyphFont =>
            groups.dropRight(1) :+ ((previous, members :+ glyph))
          case _ => groups :+ ((glyphFont, Vector(glyph)))
    }
    groups.foreach { (groupFont, members) =>
      val _ = builder.appendRunPosH(
        groupFont,
        members.map(glyphs(_)).toArray,
        members.map(xs(_) - leftmost).toArray,
        0f,
        null
      )
    }
    builder.build()

/** Any other text, laid out by Skia's own shaper with its bidi, script and font-fallback runs. A line cannot be sliced,
  * so a range is the line drawn so its range starts at `x` and clipped to the range.
  */
final private class LineStretch(line: TextLine, textLength: Int) extends SkiaShapedStretch:

  def leftEdgeOf(charIndex: Int): Option[Float] =
    Option.when(charIndex >= 0 && charIndex <= textLength)(line.getCoordAtOffset(charIndex))

  def draw(canvas: Canvas, from: Int, until: Int, x: Float, baseline: Float, paint: Paint): Unit =
    val start = line.getCoordAtOffset(from.max(0).min(textLength))
    val end   = line.getCoordAtOffset(until.max(0).min(textLength))
    RangeClip.around(canvas, x, baseline, line.getAscent.abs, end - start) {
      val _ = canvas.drawTextLine(line, x - start, baseline, paint)
    }

  private[skia] def release(): Unit = line.close()

/** A range of a line that cannot be sliced is the whole line drawn so the range starts at `x`, clipped to the range. */
private[skia] object RangeClip:

  private val MarginPx = 4f

  def around(canvas: Canvas, x: Float, baseline: Float, ascent: Float, width: Float)(draw: => Unit): Unit =
    val saved = canvas.save()
    try
      val _ = canvas.clipRect(x, baseline - ascent - MarginPx, x + width, baseline + MarginPx * 8, false)
      draw
    finally
      val _ = canvas.restoreToCount(saved)

/** Text shaped in our own font runs, one glyph to each character, drawn the way [[LineStretch]] draws a line: the whole
  * line placed so the range starts at `x`, clipped to the range. The glyphs, positions and fonts are the ones
  * `TextLine` gives for the same text, which `FallbackRunsSpec` checks for every code point it is used for.
  */
final private class RunsStretch(blob: Option[TextBlob], xs: Array[Float], width: Float, ascent: Float)
    extends SkiaShapedStretch:

  def leftEdgeOf(charIndex: Int): Option[Float] =
    Option.when(charIndex >= 0 && charIndex <= xs.length)(edgeAt(charIndex))

  def draw(canvas: Canvas, from: Int, until: Int, x: Float, baseline: Float, paint: Paint): Unit =
    val start = edgeAt(from.max(0).min(xs.length))
    val end   = edgeAt(until.max(0).min(xs.length))
    RangeClip.around(canvas, x, baseline, ascent.abs, end - start) {
      blob.foreach(canvas.drawTextBlob(_, x - start, baseline, paint))
    }

  private def edgeAt(offset: Int): Float = if offset >= xs.length then width else xs(offset)

  private[skia] def release(): Unit = blob.foreach(_.close())

private object RunsStretch:

  /** `Font.metrics` is an inline class over a float array, so its JVM name carries a hash Scala cannot write; the array
    * is looked up by name, ascent being its second element. Without it these stretches cannot be clipped as `TextLine`
    * clips them, so none is made.
    */
  private val metricsOf: Option[MethodHandle] =
    Try(
      MethodHandles
        .publicLookup()
        .findVirtual(classOf[Font], "getMetrics-l-OL_C8", MethodType.methodType(classOf[Array[Float]]))
    ).toOption

  private val AscentIndex = 1

  private def ascentOf(font: Font): Option[Float] =
    metricsOf.flatMap { handle =>
      val metrics: Object = handle.invoke(font)
      Option(metrics).collect { case values: Array[Float] if values.length > AscentIndex => values(AscentIndex) }
    }

  /** `None` unless the shaper gave exactly one glyph to each of the `runs`' characters, in order. */
  def of(runs: Vector[(Int, Font)], collector: GlyphCollector): Option[RunsStretch] =
    val length = runs.map(_._1).sum
    Option
      .when(collector.glyphs.length == length && collector.clusters.toSeq == (0 until length)) {
        val fonts = runs.flatMap((count, font) => Vector.fill(count)(font))
        fonts.distinct.traverse(ascentOf).map(_.minOption.getOrElse(0f)).map { ascent =>
          val builder = new TextBlobBuilder()
          groupsOf(fonts).foreach { (font, from, until) =>
            val _ = appendRun(builder, font, collector.glyphs.slice(from, until), collector.xs.slice(from, until))
          }
          new RunsStretch(Option(builder.build()), collector.xs, collector.width, ascent)
        }
      }
      .flatten

  /** Maximal stretches of consecutive characters in the same font, as (font, from, until). */
  private def groupsOf(fonts: Vector[Font]): Vector[(Font, Int, Int)] =
    fonts.zipWithIndex.foldLeft(Vector.empty[(Font, Int, Int)]) {
      case (groups, (font, index)) =>
        groups.lastOption match
          case Some((previous, from, _)) if previous eq font => groups.dropRight(1) :+ ((font, from, index + 1))
          case _                                             => groups :+ ((font, index, index + 1))
    }

  @SuppressWarnings(Array("org.wartremover.warts.Null"))
  private def appendRun(builder: TextBlobBuilder, font: Font, glyphs: Array[Short], xs: Array[Float]): TextBlobBuilder =
    builder.appendRunPosH(font, glyphs, xs, 0f, null)

/** Shapes stretches and remembers them, found by text and font. Two generations stand in for an eviction order, as in
  * [[ShapedTextCache]]. Shaping runs under one lock because a Skia `Shaper` holds state between calls.
  */
final class SkiaShapedTextCache(
    capacityPerGeneration: Int = SkiaShapedTextCache.DefaultCapacity,
    warmup: WarmupStats = new WarmupStats(),
    fallbacks: FallbackFonts = FallbackFonts.system,
    ligaturesOf: Font => Boolean = _ => true,
    bidiFallbacks: FallbackFonts = FallbackFonts.systemByFamily
) extends ShapeTraffic:
  import SkiaShapedTextCache.*

  private val shaper     = timed(warmup.shaperMade)(Shaper.Companion.make())
  private val current    = new java.util.concurrent.atomic.AtomicReference(new Stretches())
  private val previous   = new java.util.concurrent.atomic.AtomicReference(new Stretches())
  private val shapedN    = new java.util.concurrent.atomic.AtomicLong(0)
  private val lineN      = new java.util.concurrent.atomic.AtomicLong(0)
  private val promotedN  = new java.util.concurrent.atomic.LongAdder()
  private val lookupN    = new java.util.concurrent.atomic.LongAdder()
  private val evictedN   = new java.util.concurrent.atomic.LongAdder()
  private val shapeLock  = new Object
  private val rotateLock = new Object

  def shapings: Long = shapedN.get()

  def promotions: Long = promotedN.sum()

  /** Every `stretchOf` call: the lookups that were answered are `lookups - shapings`. */
  def lookups: Long = lookupN.sum()

  /** Stretches a rotation dropped from the older generation, so held by neither generation any more. */
  def evictions: Long = evictedN.sum()

  /** How many of those went through Skiko's `TextLine`, the slow path. */
  private[skia] def lineShapings: Long = lineN.get()

  private def timed[A](record: Long => Unit)(work: => A): A =
    val started = System.nanoTime()
    val result  = work
    record(System.nanoTime() - started)
    result

  def stretchOf(text: String, font: Font): SkiaShapedStretch =
    lookupN.increment()
    val key = Key(text, font)
    Option(current.get.get(key)).getOrElse {
      Option(previous.get.get(key)).fold(shapeAndRemember(key))(promote(key, _))
    }

  /** What `read` finds in the stretch of `text`: the remembered one when the text was drawn, otherwise one shaped for
    * the reading alone and freed after it, so measuring text that is never drawn neither counts as a shaping nor fills
    * the cache and the native heap with stretches nothing draws.
    */
  private[skia] def reading[A](text: String, font: Font)(read: SkiaShapedStretch => A): A =
    val key = Key(text, font)
    Option(current.get.get(key)).orElse(Option(previous.get.get(key))) match
      case Some(stretch) => read(stretch)
      case None =>
        val stretch = shapeLock.synchronized(shape(text, font))
        try read(stretch)
        finally stretch.release()

  private def shapeAndRemember(key: Key): SkiaShapedStretch =
    val _ = shapedN.incrementAndGet()
    remember(key, timed(warmup.shaped)(shapeLock.synchronized(shape(key.text, key.font))))

  private def promote(key: Key, stretch: SkiaShapedStretch): SkiaShapedStretch =
    promotedN.increment()
    remember(key, stretch)

  private def remember(key: Key, stretch: SkiaShapedStretch): SkiaShapedStretch =
    rotateLock.synchronized {
      if current.get.size >= capacityPerGeneration then
        evictedN.add(previous.get.size.toLong)
        previous.set(current.get)
        current.set(new Stretches())
      val _ = current.get.put(key, stretch)
    }
    stretch

  private def shape(text: String, font: Font): SkiaShapedStretch =
    if text.forall(_ < FirstCharNeedingScriptRuns) then shapeLatin(text, font)
    else if BidiShaping.shapesAsRuns(text) then shapeBidi(text, font)
    else shapeWithRuns(text, font).getOrElse(shapeLine(text, font))

  /** Text of plain characters and a few simple symbols, shaped with the font runs `TextLine` would choose; `None` when
    * it is not such text, a character has no font, or the shaper does not give one glyph a character.
    */
  private def shapeWithRuns(text: String, font: Font): Option[SkiaShapedStretch] =
    Option.when(FallbackRuns.eligible(text))(text).flatMap { _ =>
      FallbackRuns.runsFor(text, font, fallbacks).flatMap { runs =>
        val collector = new GlyphCollector()
        val ends      = runs.scanLeft(0)(_ + _._1).drop(1)
        shaper.shape(
          text,
          runs.zip(ends).map((run, end) => FontRun(end, run._2)).iterator.asJava,
          Iterator.single(BidiRun(text.length, 0)).asJava,
          Iterator.single(ScriptRun(text.length, "Latn")).asJava,
          Iterator.single(LanguageRun(text.length, "en")).asJava,
          ShapingFeatures.optionsFor(ligaturesOf(font)),
          Float.MaxValue,
          collector
        )
        RunsStretch.of(runs, collector)
      }
    }

  private[skia] def shapeLine(text: String, font: Font): SkiaShapedStretch =
    val _ = lineN.incrementAndGet()
    new LineStretch(TextLine.Companion.make(text, font, ShapingFeatures.optionsFor(ligaturesOf(font))), text.length)

  /** Text that reads right to left somewhere, shaped in bidi and script runs so its glyphs come out in visual order,
    * each character in the font that has it: the base font where it can, a fallback font where it cannot.
    */
  private def shapeBidi(text: String, font: Font): SkiaShapedStretch =
    val end       = text.length
    val runs      = BidiShaping.fontRuns(text, font, bidiFallbacks)
    val fonts     = BidiShaping.fontsOf(runs)
    val collector = new GlyphCollector()
    shaper.shape(
      text,
      BidiShaping.skiaFontRuns(runs).iterator.asJava,
      BidiShaping.levelRuns(text).iterator.asJava,
      BidiShaping.scriptRuns(text).iterator.asJava,
      Iterator.single(LanguageRun(end, "en")).asJava,
      ShapingFeatures.optionsFor(ligaturesOf(font)),
      Float.MaxValue,
      collector
    )
    new GlyphStretch(font, end, collector.glyphs, collector.xs, collector.clusters, fonts(_))

  private def shapeLatin(text: String, font: Font): SkiaShapedStretch =
    val end       = text.length
    val collector = new GlyphCollector()
    shaper.shape(
      text,
      Iterator.single(FontRun(end, font)).asJava,
      Iterator.single(BidiRun(end, 0)).asJava,
      Iterator.single(ScriptRun(end, "Latn")).asJava,
      Iterator.single(LanguageRun(end, "en")).asJava,
      ShapingFeatures.optionsFor(ligaturesOf(font)),
      Float.MaxValue,
      collector
    )
    new GlyphStretch(font, end, collector.glyphs, collector.xs, collector.clusters, _ => font)

object SkiaShapedTextCache:

  val DefaultCapacity: Int = 2048

  /** Whether `drawString`, which maps characters to glyphs and shapes nothing, would draw `text` as it should.
    * Ligatures and kerning are asked of the font by the layout attributes on the AWT font; combining marks and scripts
    * that join or reorder need the shaper whatever the attributes say.
    */
  def worthShaping(awt: java.awt.Font, text: String): Boolean =
    awt.hasLayoutAttributes || text.exists(_ >= FirstCharNeedingScriptRuns)

  /** Below the combining marks, left-to-right Latin runs are all the explicit-run shaper is asked for. */
  private val FirstCharNeedingScriptRuns = '̀'

  /** Whether `text` is all below the combining marks, which is what decides whether a stretch of it is shaped here or
    * by Skiko's `TextLine`.
    */
  private[ui] def shapedByLatinRuns(text: String): Boolean = text.forall(_ < FirstCharNeedingScriptRuns)

  final private case class Key(text: String, font: Font)

  private type Stretches = ConcurrentHashMap[Key, SkiaShapedStretch]

/** The runs Skia's shaper is given for text that reads right to left somewhere: the bidi level of each stretch of it
  * and the script of each, so the shaper reorders the runs into visual order and shapes each in its direction. The
  * glyphs the draw path paints and the glyphs the measurer measures are both shaped from these runs.
  */
private[skia] object BidiShaping:

  /** Whether `text` has a right-to-left character and nothing Skiko's `TextLine` is better at (emoji sequences). */
  def shapesAsRuns(text: String): Boolean =
    text.exists(char =>
      Character.getDirectionality(char) match
        case Character.DIRECTIONALITY_RIGHT_TO_LEFT | Character.DIRECTIONALITY_RIGHT_TO_LEFT_ARABIC => true
        case _                                                                                      => false
    ) && !text.exists(Character.isSurrogate)

  /** The font of each stretch of `text`: the base font for the characters it has, the fallback font the system chooses
    * for the ones it lacks; a single base-font run when some character has no font at all.
    */
  def fontRuns(text: String, base: Font, fallbacks: FallbackFonts): Vector[(Int, Font)] =
    FallbackRuns.runsFor(text, base, fallbacks, spacesJoinRun = false).getOrElse(Vector((text.length, base)))

  /** The font of every character, from the runs [[fontRuns]] gives. */
  def fontsOf(runs: Vector[(Int, Font)]): Vector[Font] = runs.flatMap((count, font) => Vector.fill(count)(font))

  def skiaFontRuns(runs: Vector[(Int, Font)]): Vector[FontRun] =
    runs.scanLeft(0)(_ + _._1).drop(1).zip(runs).map((end, run) => FontRun(end, run._2))

  def levelRuns(text: String): Vector[BidiRun] =
    val bidi = java.text.Bidi(text, java.text.Bidi.DIRECTION_DEFAULT_LEFT_TO_RIGHT)
    Vector.tabulate(bidi.getRunCount)(run => BidiRun(bidi.getRunLimit(run), bidi.getRunLevel(run)))

  /** A script run for each stretch of one script; what has no script of its own (spaces, digits, punctuation) takes the
    * script before it, or the one after it at the start of the text.
    */
  def scriptRuns(text: String): Vector[ScriptRun] =
    val own: Vector[Option[String]] = text.toVector.map { char =>
      Character.UnicodeScript.of(char) match
        case Character.UnicodeScript.HEBREW                                     => Some("Hebr")
        case Character.UnicodeScript.ARABIC                                     => Some("Arab")
        case Character.UnicodeScript.COMMON | Character.UnicodeScript.INHERITED => None
        case _                                                                  => Some("Latn")
    }
    val first = own.flatten.headOption.getOrElse("Latn")
    val tags  = own.scanLeft(first)((previous, tag) => tag.getOrElse(previous)).drop(1)
    tags.indices
      .filter(index => index == tags.length - 1 || tags(index + 1) != tags(index))
      .toVector
      .map(index => ScriptRun(index + 1, tags(index)))

/** Collects the glyph ids, x positions and source clusters of each shaped run, left to right. */
final private class GlyphCollector extends RunHandler:
  private val glyphIds     = scala.collection.mutable.ArrayBuilder.ofShort()
  private val glyphXs      = scala.collection.mutable.ArrayBuilder.ofFloat()
  private val glyphYs      = scala.collection.mutable.ArrayBuilder.ofFloat()
  private val glyphLevels  = scala.collection.mutable.ArrayBuilder.ofByte()
  private val glyphCluster = scala.collection.mutable.ArrayBuilder.ofInt()
  private val penX         = new Array[Float](1)

  /** The advance of everything committed so far. */
  def width: Float = penX(0)

  lazy val glyphs: Array[Short] = glyphIds.result()
  lazy val xs: Array[Float]     = glyphXs.result()
  lazy val ys: Array[Float]     = glyphYs.result()

  /** The bidi level of the run each glyph was shaped in; odd levels read right to left. */
  lazy val levels: Array[Byte]  = glyphLevels.result()
  lazy val clusters: Array[Int] = glyphCluster.result()

  /** What the shaping came to, without the builders that gathered it: a builder keeps the over-sized buffer it grew
    * into, so a collector held in a memo holds every glyph twice.
    */
  def frozen: ShapedGlyphs = ShapedGlyphs(glyphs, xs, ys, levels, clusters, width)

  def beginLine(): Unit               = ()
  def runInfo(info: RunInfo): Unit    = ()
  def commitRunInfo(): Unit           = ()
  def runOffset(info: RunInfo): Point = Point(penX(0), 0f)
  def commitLine(): Unit              = ()

  def commitRun(info: RunInfo, runGlyphs: Array[Short], positions: Array[Point], runClusters: Array[Int]): Unit =
    glyphIds.addAll(runGlyphs)
    positions.foreach { position =>
      glyphXs.addOne(position.getX)
      glyphYs.addOne(position.getY)
    }
    glyphCluster.addAll(runClusters)
    runGlyphs.foreach(_ => glyphLevels.addOne(info.getBidiLevel.toByte))
    penX(0) += info.getAdvanceX

/** One shaping's glyphs, their positions, bidi levels and source clusters, and its advance. */
final private[skia] case class ShapedGlyphs(
  glyphs: Array[Short],
  xs: Array[Float],
  ys: Array[Float],
  levels: Array[Byte],
  clusters: Array[Int],
  width: Float
)
