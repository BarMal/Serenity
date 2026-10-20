package com.serenity.ui.renderer

import java.awt.Font
import java.awt.font.{FontRenderContext, GlyphVector}
import java.awt.geom.Point2D
import java.text.Bidi
import java.util.concurrent.ConcurrentHashMap
import java.util.concurrent.atomic.{AtomicLong, AtomicReference}

/** Text shaped once and kept: the glyphs a stretch of text paints with, found by text, font and render context.
  *
  * `Graphics2D.drawString` shapes its string afresh on every call, and a line painted as several colour runs shapes
  * every run on its own. Here a stretch is shaped whole, so ligatures and kerning reach across colour boundaries, and a
  * repeat is a lookup. Two generations stand in for an eviction order: a hit in the older one moves the stretch to the
  * newer, and when the newer fills, the older is dropped, so what the last frames painted survives and the cache holds
  * at most twice `capacityPerGeneration` stretches.
  */
final class ShapedTextCache(capacityPerGeneration: Int = ShapedTextCache.DefaultCapacity):
  import ShapedTextCache.*

  private val generations = AtomicReference(Generations(newMap(), newMap()))
  private val shapedN     = AtomicLong(0)
  private val exactN      = AtomicLong(0)

  /** The advances of the glyphs this cache's stretches are made of, and of any run measured beside them. */
  val glyphAdvances: GlyphAdvance = GlyphAdvance()

  /** How many stretches were shaped, as opposed to served from the cache. */
  def shapings: Long = shapedN.get()

  /** How many exact measurements (one `TextLayout` or one shaped stretch of a row or candidate) caret and fit
    * measurement built through this cache: the cost wrapping pays when summed glyph advances cannot answer, which the
    * wrap specs bound per paragraph.
    */
  def exactMeasurements: Long = exactN.get()

  private[serenity] def countExactMeasurement(): Unit =
    val _ = exactN.incrementAndGet()

  def holds(text: String, font: Font, fontRenderContext: FontRenderContext): Boolean =
    val key  = Key(text, font, fontRenderContext)
    val held = generations.get()
    held.current.containsKey(key) || held.previous.containsKey(key)

  def stretchOf(text: String, font: Font, fontRenderContext: FontRenderContext): ShapedStretch =
    val key  = Key(text, font, fontRenderContext)
    val held = generations.get()
    Option(held.current.get(key)).getOrElse {
      Option(held.previous.get(key)).fold(shapeAndRemember(key))(remember(key, _))
    }

  private def shapeAndRemember(key: Key): ShapedStretch =
    val _ = shapedN.incrementAndGet()
    remember(key, ShapedStretch.shape(key.text, key.font, key.fontRenderContext, glyphAdvances))

  private def remember(key: Key, stretch: ShapedStretch): ShapedStretch =
    val held = generations.get()
    if held.current.size < capacityPerGeneration then
      val _ = held.current.put(key, stretch)
    else
      val next = Generations(newMap(), held.current)
      val _    = next.current.put(key, stretch)
      val _    = generations.compareAndSet(held, next)
    stretch

object ShapedTextCache:

  val DefaultCapacity: Int = 2048

  /** Below the combining marks, in a font with no ligatures or kerning requested, `drawString` only maps characters to
    * glyphs and shapes nothing, so a lookup and a glyph vector would cost more than it saves.
    */
  def worthShaping(font: Font, text: String): Boolean =
    font.hasLayoutAttributes || text.exists(_ >= FirstCharNeedingLayout)

  private val FirstCharNeedingLayout = '\u0300'

  final private case class Key(text: String, font: Font, fontRenderContext: FontRenderContext)

  private type Stretches = ConcurrentHashMap[Key, ShapedStretch]

  final private case class Generations(current: Stretches, previous: Stretches)

  private def newMap(): Stretches = new ConcurrentHashMap[Key, ShapedStretch]()

/** The glyphs of one text in one font, in visual order with their shaped positions, from which any range of the text
  * can be drawn without shaping it again.
  *
  * A range keeps the glyphs of its characters exactly where whole-text shaping put them, so a range drawn at its own
  * left edge continues the glyphs beside it. A ligature belongs to the range holding its first character.
  */
final class ShapedStretch private (
    textLength: Int,
    font: Font,
    fontRenderContext: FontRenderContext,
    glyphAdvances: GlyphAdvance,
    glyphCodes: Array[Int],
    charIndices: Array[Int],
    rightToLeft: Array[Boolean],
    xs: Array[Float],
    ys: Array[Float],
    val advance: Float
):
  import ShapedStretch.*

  // Measuring reads the glyph arrays alone, so a stretch only measured and never drawn builds no glyph vector.
  private lazy val whole = glyphVector(font, fontRenderContext, glyphCodes, xs, ys)
  private val ranges     = new ConcurrentHashMap[Long, Option[GlyphVector]]()

  def glyphCount: Int = glyphCodes.length

  def glyphCodeAt(glyph: Int): Int = glyphCodes(glyph)

  /** The index in the text of the character the glyph was shaped from; a ligature reports its first character. */
  def charIndexAt(glyph: Int): Int = charIndices(glyph)

  /** Whether the glyph belongs to a right-to-left run, where its leading edge is its right one. */
  def isRightToLeft(glyph: Int): Boolean = rightToLeft(glyph)

  /** The glyph's own advance: unlike [[rightEdgeAt]] it holds nothing of the kerning with its neighbour. */
  def advanceAt(glyph: Int): Float = advances(glyph)

  private lazy val advances: Array[Float] =
    val byCode = glyphCodes.distinct.map(code => code -> glyphAdvances.of(font, fontRenderContext, code)).toMap
    glyphCodes.map(byCode)

  def xAt(glyph: Int): Float = xs(glyph)

  def yAt(glyph: Int): Float = ys(glyph)

  /** Where the glyph ends: the next glyph's x, or the stretch's advance after the last one. */
  def rightEdgeAt(glyph: Int): Float = if glyph + 1 < xs.length then xs(glyph + 1) else advance

  /** The shaped x of the glyph that starts at this character, `None` when no glyph does (inside a ligature). */
  def leftEdgeOf(charIndex: Int): Option[Float] =
    charIndices.indexOf(charIndex) match
      case -1    => None
      case glyph => Some(xs(glyph))

  /** The glyphs of `[from, until)`, with the leftmost at x = 0; `None` when no glyph starts in that range. */
  def glyphsOf(from: Int, until: Int): Option[GlyphVector] =
    if from <= 0 && until >= textLength then Some(whole)
    else
      if ranges.size >= MaxRangesPerStretch then ranges.clear()
      ranges.computeIfAbsent((from.toLong << 32) | (until.toLong & 0xffffffffL), _ => rangeOf(from, until))

  private def rangeOf(from: Int, until: Int): Option[GlyphVector] =
    val selected = glyphCodes.indices.filter(glyph => charIndices(glyph) >= from && charIndices(glyph) < until).toArray
    Option.when(selected.nonEmpty) {
      val leftmost = selected.foldLeft(Float.MaxValue)((left, glyph) => left.min(xs(glyph)))
      glyphVector(
        font,
        fontRenderContext,
        selected.map(glyphCodes(_)),
        selected.map(xs(_) - leftmost),
        selected.map(ys(_))
      )
    }

object ShapedStretch:

  /** Distinct colour splits of one text are few; this only stops a pathological editing session growing one stretch. */
  private val MaxRangesPerStretch = 64

  final private case class DirectionalRun(start: Int, end: Int, rightToLeft: Boolean)

  final private case class ShapedRun(
      glyphCodes: Array[Int],
      charIndices: Array[Int],
      rightToLeft: Boolean,
      xs: Array[Float],
      ys: Array[Float],
      advance: Float
  )

  def shape(text: String, font: Font, fontRenderContext: FontRenderContext): ShapedStretch =
    shape(text, font, fontRenderContext, GlyphAdvance())

  def shape(
    text: String,
    font: Font,
    fontRenderContext: FontRenderContext,
    glyphAdvances: GlyphAdvance
  ): ShapedStretch =
    FontFirstUse.settle(font, fontRenderContext)
    val chars   = text.toCharArray
    val shaped  = visualRuns(text, chars).map(shapeRun(chars, _, font, fontRenderContext))
    val offsets = shaped.scanLeft(0.0f)(_ + _.advance)
    new ShapedStretch(
      chars.length,
      font,
      fontRenderContext,
      glyphAdvances,
      shaped.flatMap(_.glyphCodes).toArray,
      shaped.flatMap(_.charIndices).toArray,
      shaped.flatMap(run => Array.fill(run.charIndices.length)(run.rightToLeft)).toArray,
      shaped.zip(offsets).flatMap((run, offset) => run.xs.map(_ + offset)).toArray,
      shaped.flatMap(_.ys).toArray,
      offsets.lastOption.getOrElse(0.0f)
    )

  /** What `TextLayout`, and so `drawString`, does for text that needs it: runs of one direction, left to right on the
    * line. A text with no right-to-left characters is the one run.
    */
  private def visualRuns(text: String, chars: Array[Char]): Vector[DirectionalRun] =
    if !Bidi.requiresBidi(chars, 0, chars.length) then Vector(DirectionalRun(0, chars.length, rightToLeft = false))
    else
      val bidi   = new Bidi(text, Bidi.DIRECTION_DEFAULT_LEFT_TO_RIGHT)
      val count  = bidi.getRunCount
      val levels = Array.tabulate(count)(run => bidi.getRunLevel(run).toByte)
      val order  = Array.tabulate[AnyRef](count)(run => Integer.valueOf(run))
      Bidi.reorderVisually(levels, 0, order, 0, count)
      order.toVector.collect {
        case boxed: Integer =>
          val run = boxed.intValue
          DirectionalRun(bidi.getRunStart(run), bidi.getRunLimit(run), bidi.getRunLevel(run) % 2 == 1)
      }

  private def shapeRun(chars: Array[Char], run: DirectionalRun, font: Font, frc: FontRenderContext): ShapedRun =
    val direction = if run.rightToLeft then Font.LAYOUT_RIGHT_TO_LEFT else Font.LAYOUT_LEFT_TO_RIGHT
    val glyphs    = font.layoutGlyphVector(frc, chars, run.start, run.end, direction)
    val count     = glyphs.getNumGlyphs
    val positions = glyphs.getGlyphPositions(0, count + 1, new Array[Float](2 * (count + 1)))
    ShapedRun(
      glyphs.getGlyphCodes(0, count, new Array[Int](count)),
      glyphs.getGlyphCharIndices(0, count, new Array[Int](count)).map(_ + run.start),
      run.rightToLeft,
      Array.tabulate(count)(glyph => positions(2 * glyph)),
      Array.tabulate(count)(glyph => positions(2 * glyph + 1)),
      positions(2 * count)
    )

  private def glyphVector(
    font: Font,
    frc: FontRenderContext,
    glyphCodes: Array[Int],
    xs: Array[Float],
    ys: Array[Float]
  ): GlyphVector =
    val vector = font.createGlyphVector(frc, glyphCodes)
    glyphCodes.indices.foreach(glyph => vector.setGlyphPosition(glyph, new Point2D.Float(xs(glyph), ys(glyph))))
    vector
