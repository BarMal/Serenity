package com.serenity.ui.layout

import java.awt.Font
import java.awt.font.{FontRenderContext, TextAttribute, TextHitInfo, TextLayout}
import java.text.AttributedString

import com.serenity.ui.layout.TextCaretMeasurement.{ColumnFontRun, LineFontResolver}
import com.serenity.ui.layout.TextMeasurer.{FontRun, LineFonts, LineMetrics, MetricsKey, ShapedRun}
import com.serenity.ui.renderer.FontSpec

/** [[TextMeasurer]] over Java2D at one `FontRenderContext`, answering exactly as the layout's own Java2D measurement
  * does.
  */
final case class Java2DTextMeasurer(frc: FontRenderContext) extends TextMeasurer:

  val metricsKey: MetricsKey = MetricsKey("java2d", Vector(frc))

  def measuresProportionally(font: FontSpec): Boolean = TextLayoutSnapshot.shouldUseMeasuredLayout(font.toAwt, frc)

  def cellMetrics(font: FontSpec): CellMetrics = CellMetrics.fromFont(font.toAwt)

  /** Without a fallback chain each column keeps its styled font, so these are the resolver's own font runs. */
  def itemise(text: String, from: Int, until: Int, startColumn: Int, fonts: LineFonts): Vector[FontRun] =
    resolverFor(fonts).fontRuns(startColumn + from, startColumn + until).map { span =>
      FontRun(span.startColumn, span.endColumn, FontSpec.fromAwt(span.font))
    }

  def lineMetrics(fonts: LineFonts, startColumn: Int, endColumn: Int): LineMetrics =
    val (heightPx, ascentPx) = resolverFor(fonts).lineMetrics(frc, startColumn, endColumn)
    LineMetrics(heightPx, ascentPx)

  def rawCaretXs(text: String, startColumn: Int, fonts: LineFonts): IArray[Float] =
    TextCaretMeasurement.rawMeasuredCaretXs(text, startColumn, resolverFor(fonts), frc)

  def glyphWidthPx(font: FontSpec, text: String): Float = DropCapLayout.glyphWidthPx(font.toAwt, frc, text)

  def shape(text: String, startColumn: Int, run: FontRun, level: Byte): ShapedRun =
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
    else
      val attributed = AttributedString(text)
      itemise(text, 0, text.length, startColumn, fonts).foreach { run =>
        attributed.addAttribute(
          TextAttribute.FONT,
          run.font.toAwt,
          run.startColumn - startColumn,
          run.endColumn - startColumn
        )
      }
      val layout = TextLayout(attributed.getIterator, frc)
      val carets = new Array[Float](text.length + 1)
      (0 until text.length).foreach(index => carets(index) = layout.getCaretInfo(TextHitInfo.leading(index))(0))
      carets(text.length) = layout.getAdvance
      IArray.unsafeFromArray(carets)

  def shapingCutAtOrAfter(text: String, from: Int, font: FontSpec): Int =
    ShapingBarriers.cutAtOrAfter(text, from, font.toAwt, frc)

  def shapingCutAtOrBefore(text: String, limit: Int, font: FontSpec): Int =
    ShapingBarriers.cutAtOrBefore(text, limit, font.toAwt, frc)

  private[layout] def mayMeasureByAdvances(text: String): Boolean = GlyphAdvances.hasContextFreeCharacters(text)

  private[layout] def advances(
    text: String,
    from: Int,
    until: Int,
    startColumn: Int,
    fonts: LineFonts
  ): GlyphAdvances =
    GlyphAdvances.measure(text, from, until, startColumn, resolverFor(fonts), frc)

  private def resolverFor(fonts: LineFonts): LineFontResolver =
    LineFontResolver(
      fonts.base.toAwt,
      fonts.runs.map(run => ColumnFontRun(run.startColumn, run.endColumn, run.font.toAwt))
    )
