package com.serenity.ui.layout

import java.awt.Font
import java.awt.font.{FontRenderContext, TextAttribute, TextHitInfo, TextLayout}
import java.text.AttributedString
import java.util.concurrent.atomic.AtomicLong

import com.serenity.ui.layout.TextCaretMeasurement.LineFontResolver
import com.serenity.ui.renderer.FontFirstUse

/** The measurement a row falls back to when summed glyph advances cannot answer: its glyphs shaped as the draw path
  * shapes them, or a `TextLayout` for the text shaping cannot stand in for.
  */
private[layout] object ExactCaretXs:

  private val exactMeasurementsBuilt: ThreadLocal[AtomicLong] = ThreadLocal.withInitial(() => AtomicLong(0))

  /** How many exact measurements (one `TextLayout` or one shaped stretch of a row or candidate) caret and fit
    * measurement has built on the calling thread: the cost wrapping pays when summed glyph advances cannot answer,
    * which the wrap specs bound per paragraph. Counted per thread so specs running side by side do not see each
    * other's.
    */
  private[layout] def exactMeasurementsConstructed: Long = exactMeasurementsBuilt.get().get()

  private def countExactMeasurement(): Unit =
    val _ = exactMeasurementsBuilt.get().incrementAndGet()

  private def settleFonts(resolver: LineFontResolver, frc: FontRenderContext): Unit =
    FontFirstUse.settleAll(resolver.baseFont +: resolver.runs.map(_.font), frc)

  private def textLayoutOf(text: AttributedString, frc: FontRenderContext): TextLayout =
    countExactMeasurement()
    TextLayout(text.getIterator, frc)

  /** Whether the glyphs the draw path places can stand for `TextLayout`: a row with right-to-left characters is ordered
    * by `TextLayout`, and a face whose layout advance is not its summed advances (a slanted one) is measured by it.
    */
  private def measuresByGlyphs(text: String, fonts: Iterable[Font], frc: FontRenderContext): Boolean =
    !java.text.Bidi.requiresBidi(text.toCharArray, 0, text.length) &&
      fonts.forall(GlyphAdvances.layoutAdvanceMatchesSum(_, frc))

  /** Carets read from one exact measurement of `text`: exact for any text, but a shaping of the whole row. */
  private[layout] def layoutCaretXs(
    text: String,
    absoluteStartColumn: Int,
    resolver: LineFontResolver,
    frc: FontRenderContext
  ): IArray[Float] =
    withExtraAdvances(
      if resolver.hasHidden then visibleLayoutCaretXs(text, absoluteStartColumn, resolver, frc)
      else plainLayoutCaretXs(text, absoluteStartColumn, resolver, frc),
      absoluteStartColumn,
      resolver
    )

  /** `carets` with each column's extra advance added to every caret after it. */
  private def withExtraAdvances(
    carets: IArray[Float],
    absoluteStartColumn: Int,
    resolver: LineFontResolver
  ): IArray[Float] =
    if resolver.extraAdvances.isEmpty then carets
    else
      val shifts = (0 until carets.length).scanLeft(0.0f) { (sum, index) =>
        sum + resolver.extraAdvances.getOrElse(absoluteStartColumn + index, 0.0f)
      }
      IArray.tabulate(carets.length)(index => carets(index) + shifts(index))

  private def plainLayoutCaretXs(
    text: String,
    absoluteStartColumn: Int,
    resolver: LineFontResolver,
    frc: FontRenderContext
  ): IArray[Float] =
    settleFonts(resolver, frc)
    // One font per contiguous run of equal per-column fonts, so a mixed-size rich line's caret advances (and thus wrap
    // points and widths) match the per-run glyphs the draw path paints.
    val spans = resolver.fontRuns(absoluteStartColumn, absoluteStartColumn + text.length)
    if measuresByGlyphs(text, spans.map(_.font), frc) then
      countExactMeasurement()
      ShapedCaretStops.of(
        text,
        spans.map(span =>
          ShapedCaretStops.Span(span.startColumn - absoluteStartColumn, span.endColumn - absoluteStartColumn, span.font)
        ),
        frc
      )
    else
      val attributed = AttributedString(text)
      spans.foreach { span =>
        attributed.addAttribute(
          TextAttribute.FONT,
          span.font,
          span.startColumn - absoluteStartColumn,
          span.endColumn - absoluteStartColumn
        )
      }
      val layout = textLayoutOf(attributed, frc)
      val carets = new Array[Float](text.length + 1)
      (0 until text.length).foreach(index => carets(index) = layout.getCaretInfo(TextHitInfo.leading(index))(0))
      carets(text.length) = layout.getAdvance
      IArray.unsafeFromArray(carets)

  /** [[layoutCaretXs]] for a line with hidden columns: the visible characters are laid out as if the hidden ones were
    * not in the text, and each hidden column takes the caret of the next visible one.
    */
  private def visibleLayoutCaretXs(
    text: String,
    absoluteStartColumn: Int,
    resolver: LineFontResolver,
    frc: FontRenderContext
  ): IArray[Float] =
    settleFonts(resolver, frc)
    val hidden        = resolver.hiddenMask(absoluteStartColumn, text.length)
    val visibleBefore = hidden.scanLeft(0)((count, isHidden) => if isHidden then count else count + 1)
    val visibleText   = text.indices.filterNot(hidden(_)).map(text.charAt(_)).mkString
    val carets        = new Array[Float](text.length + 1)
    if visibleText.nonEmpty then
      val spans = resolver.fontRuns(absoluteStartColumn, absoluteStartColumn + text.length).filterNot(_.hidden)
      val visibleCarets =
        if measuresByGlyphs(visibleText, spans.map(_.font), frc) then
          countExactMeasurement()
          ShapedCaretStops.of(
            visibleText,
            spans.map(span =>
              ShapedCaretStops.Span(
                visibleBefore(span.startColumn - absoluteStartColumn),
                visibleBefore(span.endColumn - absoluteStartColumn),
                span.font
              )
            ),
            frc
          )
        else
          val attributed = AttributedString(visibleText)
          spans.foreach { span =>
            attributed.addAttribute(
              TextAttribute.FONT,
              span.font,
              visibleBefore(span.startColumn - absoluteStartColumn),
              visibleBefore(span.endColumn - absoluteStartColumn)
            )
          }
          val layout = textLayoutOf(attributed, frc)
          IArray.tabulate(visibleText.length + 1)(index =>
            if index == visibleText.length then layout.getAdvance
            else layout.getCaretInfo(TextHitInfo.leading(index))(0)
          )
      carets(text.length) = visibleCarets(visibleText.length)
      (text.length - 1 to 0 by -1).foreach { index =>
        carets(index) = if hidden(index) then carets(index + 1) else visibleCarets(visibleBefore(index))
      }
    IArray.unsafeFromArray(carets)
