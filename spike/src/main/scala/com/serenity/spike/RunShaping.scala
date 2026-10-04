package com.serenity.spike

import scala.jdk.CollectionConverters.*

import org.jetbrains.skia.{Canvas, Font, Paint, Point, TextBlob, TextLine}
import org.jetbrains.skia.shaper.{BidiRun, FontRun, LanguageRun, RunHandler, RunInfo, ScriptRun, Shaper, ShapingOptions}

/** A shaped row the renderer can paint and read caret stops from. */
sealed trait ShapedText:
  def draw(canvas: Canvas, x: Float, baseline: Float, paint: Paint): Unit
  def coord(offset: Int): Float
  def width: Float
  def close(): Unit

/** Skiko's `TextLine` (from `TextLine.make` or `Shaper.shapeLine`): carets via one JNI call per query. */
final class LineShaped(line: TextLine) extends ShapedText:
  def draw(canvas: Canvas, x: Float, baseline: Float, paint: Paint): Unit = canvas.drawTextLine(line, x, baseline, paint)
  def coord(offset: Int): Float                                         = line.getCoordAtOffset(offset)
  def width: Float                                                      = line.getWidth
  def close(): Unit                                                     = line.close()

/** HarfBuzz output kept on the JVM: one positioned `TextBlob` to paint and a caret array read from the same glyph
  * positions, so painting and caret stops cannot disagree (the property #1876 wants).
  */
final class RunShaped(
    blob: Option[TextBlob],
    carets: Array[Float],
    val glyphs: Array[Short],
    val xs: Array[Float]
) extends ShapedText:
  def draw(canvas: Canvas, x: Float, baseline: Float, paint: Paint): Unit =
    blob.foreach(b => canvas.drawTextBlob(b, x, baseline, paint))
  def coord(offset: Int): Float = carets(math.max(0, math.min(carets.length - 1, offset)))
  def width: Float              = carets(carets.length - 1)
  def close(): Unit             = blob.foreach(_.close())

/** Shapes with caller-supplied single font/bidi/script/language runs, skipping the ICU and font-fallback iterators that
  * `Shaper.shape(text, font)` and `TextLine.make` build on every call (about 3.7 ms each in this container, whatever
  * the text length; see `shaping.skia.shaper_textblob_5_char_strings`). Assumes left-to-right Latin text in one font:
  * enough for the lorem spike, not for production, which needs real script/bidi/fallback runs (#1866).
  */
final class ExplicitRunShaper(font: Font):
  private val shaper  = Shaper.Companion.make()
  private val options = ShapingOptions.Companion.getDEFAULT()

  def shape(text: String): RunShaped =
    if text.isEmpty then RunShaped(None, Array(0f), Array.emptyShortArray, Array.emptyFloatArray)
    else
      val handler = GlyphCollector()
      val end     = text.length
      shaper.shape(
        text,
        Iterator.single(FontRun(end, font)).asJava,
        Iterator.single(BidiRun(end, 0)).asJava,
        Iterator.single(ScriptRun(end, "Latn")).asJava,
        Iterator.single(LanguageRun(end, "en")).asJava,
        options,
        Float.MaxValue,
        handler
      )
      val (glyphs, xs, carets) = handler.finish(end)
      val blob                 = Option.when(glyphs.nonEmpty)(TextBlob.Companion.makeFromPosH(glyphs, xs, 0f, font))
      RunShaped(blob, carets, glyphs, xs)

/** Collects glyph ids, x positions and clusters across runs. */
final class GlyphCollector extends RunHandler:
  private val glyphs   = scala.collection.mutable.ArrayBuilder.ofShort()
  private val xs       = scala.collection.mutable.ArrayBuilder.ofFloat()
  private val clusters = scala.collection.mutable.ArrayBuilder.ofInt()
  private var penX                                          = 0f

  def beginLine(): Unit               = ()
  def runInfo(info: RunInfo): Unit    = ()
  def commitRunInfo(): Unit           = ()
  def runOffset(info: RunInfo): Point = Point(penX, 0f)
  def commitLine(): Unit              = ()
  def commitRun(info: RunInfo, runGlyphs: Array[Short], positions: Array[Point], runClusters: Array[Int]): Unit =
    glyphs.addAll(runGlyphs)
    positions.foreach(p => xs.addOne(p.getX))
    clusters.addAll(runClusters)
    penX += info.getAdvanceX

  /** Glyph ids, glyph x positions, and the caret x for each UTF-16 offset in `[0, length]`: a cluster's first glyph x;
    * offsets inside a cluster (a ligature) take the cluster's leading edge; `length` is the pen position after the last
    * run. Call once: the builders hand over their arrays.
    */
  def finish(length: Int): (Array[Short], Array[Float], Array[Float]) =
    val xArray   = xs.result()
    val cArray   = clusters.result()
    val result   = Array.fill(length + 1)(Float.NaN)
    cArray.indices.foreach { glyph =>
      val cluster = cArray(glyph)
      if cluster >= 0 && cluster < length && result(cluster).isNaN then result(cluster) = xArray(glyph)
    }
    result(length) = penX
    (1 until length).foreach(i => if result(i).isNaN then result(i) = result(i - 1))
    if result(0).isNaN then result(0) = 0f
    (glyphs.result(), xArray, result)
