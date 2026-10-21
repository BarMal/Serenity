package com.serenity.ui.layout

import java.awt.Font
import java.awt.font.FontRenderContext

import com.serenity.ui.layout.TextCaretMeasurement.{ColumnFontRun, LineFontResolver}
import com.serenity.ui.layout.TextMeasurer.{FontRun, LineFonts, LineMetrics, MetricsKey, ShapedRun}
import com.serenity.ui.renderer.{FontFirstUse, FontSpec, ShapedTextCache}

/** [[TextMeasurer]] over Java2D at one `FontRenderContext`, answering exactly as the layout's own Java2D measurement
  * does.
  */
final case class Java2DTextMeasurer(
    frc: FontRenderContext,
    shapedText: ShapedTextCache = ShapedTextCache(),
    memos: LayoutMemos = LayoutMemos()
) extends TextMeasurer:

  val metricsKey: MetricsKey = MetricsKey("java2d", Vector(frc))

  def measuresProportionally(font: FontSpec): Boolean =
    TextLayoutSnapshot.shouldUseMeasuredLayout(font.toAwt, frc, shapedText.probes)

  def cellMetrics(font: FontSpec): CellMetrics = CellMetrics.fromFont(font.toAwt)

  /** Without a fallback chain each column keeps its styled font, so these are the resolver's own font runs. */
  def itemise(text: String, from: Int, until: Int, startColumn: Int, fonts: LineFonts): Vector[FontRun] =
    resolverFor(fonts).fontRuns(startColumn + from, startColumn + until).map { span =>
      FontRun(span.startColumn, span.endColumn, FontSpec.fromAwt(span.font))
    }

  def lineMetrics(fonts: LineFonts, startColumn: Int, endColumn: Int): LineMetrics =
    val (heightPx, ascentPx) = resolverFor(fonts).lineMetrics(frc, memos.lineExtents, startColumn, endColumn)
    LineMetrics(heightPx, ascentPx)

  def rawCaretXs(text: String, startColumn: Int, fonts: LineFonts): IArray[Float] =
    TextCaretMeasurement.rawMeasuredCaretXs(text, startColumn, resolverFor(fonts), frc, shapedText)

  def glyphWidthPx(font: FontSpec, text: String): Float = memos.dropCapWidths(font.toAwt, frc, text)

  def shape(text: String, startColumn: Int, run: FontRun, level: Byte): ShapedRun =
    FontFirstUse.settle(run.font.toAwt, frc)
    val chars     = text.substring(run.startColumn - startColumn, run.endColumn - startColumn).toCharArray
    val flags     = if level % 2 == 0 then Font.LAYOUT_LEFT_TO_RIGHT else Font.LAYOUT_RIGHT_TO_LEFT
    val shaped    = run.font.toAwt.layoutGlyphVector(frc, chars, 0, chars.length, flags)
    val glyphs    = shaped.getNumGlyphs
    val positions = shaped.getGlyphPositions(0, glyphs + 1, new Array[Float](2 * (glyphs + 1)))
    ShapedRun(
      run.font,
      level,
      IArray.unsafeFromArray(shaped.getGlyphCodes(0, glyphs, new Array[Int](glyphs))),
      IArray.tabulate(glyphs + 1)(index => positions(2 * index)),
      IArray.tabulate(glyphs + 1)(index => positions(2 * index + 1)),
      IArray.unsafeFromArray(shaped.getGlyphCharIndices(0, glyphs, new Array[Int](glyphs)))
    )

  def exactCaretXs(text: String, startColumn: Int, fonts: LineFonts): IArray[Float] =
    if text.isEmpty then IArray(0.0f)
    else ExactCaretXs.layoutCaretXs(text, startColumn, resolverFor(fonts), frc, shapedText)

  override def exactEdges(text: String, startColumn: Int, fonts: LineFonts): IArray[Float] =
    if text.isEmpty then IArray(0.0f)
    else
      val resolver = resolverFor(fonts)
      val stops    = ExactCaretXs.layoutStops(text, startColumn, resolver, frc, shapedText)
      stops.trailing.getOrElse(TextCaretMeasurement.normalizeCollapsedCarets(stops.carets, resolver, startColumn))

  def shapingCutAtOrAfter(text: String, from: Int, font: FontSpec): Int =
    ShapingBarriers.cutAtOrAfter(text, from, font.toAwt, frc, shapedText)

  def shapingCutAtOrBefore(text: String, limit: Int, font: FontSpec): Int =
    ShapingBarriers.cutAtOrBefore(text, limit, font.toAwt, frc, shapedText)

  private[layout] def mayMeasureByAdvances(text: String): Boolean = GlyphAdvances.hasContextFreeCharacters(text)

  private[layout] def advances(
    text: String,
    from: Int,
    until: Int,
    startColumn: Int,
    fonts: LineFonts
  ): GlyphAdvances =
    GlyphAdvances.measure(text, from, until, startColumn, resolverFor(fonts), frc, shapedText)

  private def resolverFor(fonts: LineFonts): LineFontResolver =
    LineFontResolver(
      fonts.base.toAwt,
      fonts.runs.map(run => ColumnFontRun(run.startColumn, run.endColumn, run.font.toAwt))
    )
