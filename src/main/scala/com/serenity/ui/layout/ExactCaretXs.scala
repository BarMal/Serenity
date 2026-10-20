package com.serenity.ui.layout

import java.awt.Font
import java.awt.font.{FontRenderContext, TextAttribute, TextHitInfo, TextLayout}
import java.text.AttributedString

import com.serenity.ui.layout.TextCaretMeasurement.LineFontResolver
import com.serenity.ui.renderer.{FontFirstUse, FontProbes, ShapedTextCache}

/** The measurement a row falls back to when summed glyph advances cannot answer: its glyphs shaped as the draw path
  * shapes them, or a `TextLayout` for the text shaping cannot stand in for.
  */
private[layout] object ExactCaretXs:

  private def settleFonts(resolver: LineFontResolver, frc: FontRenderContext): Unit =
    FontFirstUse.settleAll(resolver.baseFont +: resolver.runs.map(_.font), frc)

  private def textLayoutOf(text: AttributedString, frc: FontRenderContext, shapedText: ShapedTextCache): TextLayout =
    shapedText.countExactMeasurement()
    TextLayout(text.getIterator, frc)

  /** Whether the glyphs the draw path places can stand for `TextLayout`. A face whose layout advance is not its summed
    * advances (a slanted one) is measured by `TextLayout`, and so is a row of several fonts with right-to-left text in
    * it, which shaping one font at a time would not order across fonts.
    */
  private def measuresByGlyphs(text: String, fonts: Seq[Font], frc: FontRenderContext, probes: FontProbes): Boolean =
    fonts.forall(GlyphAdvances.layoutAdvanceMatchesSum(_, frc, probes)) &&
      (fonts.length <= 1 || !java.text.Bidi.requiresBidi(text.toCharArray, 0, text.length))

  /** Carets read from one exact measurement of `text`: exact for any text, but a shaping of the whole row. */
  private[layout] def layoutCaretXs(
    text: String,
    absoluteStartColumn: Int,
    resolver: LineFontResolver,
    frc: FontRenderContext,
    shapedText: ShapedTextCache
  ): IArray[Float] =
    layoutStops(text, absoluteStartColumn, resolver, frc, shapedText).carets

  /** [[layoutCaretXs]] with, where the row reads left to right, the edge each boundary has as the end of a row. */
  private[layout] def layoutStops(
    text: String,
    absoluteStartColumn: Int,
    resolver: LineFontResolver,
    frc: FontRenderContext,
    shapedText: ShapedTextCache
  ): ShapedCaretStops.Stops =
    val laidOut =
      if resolver.hasHidden then visibleStops(text, absoluteStartColumn, resolver, frc, shapedText)
      else plainStops(text, absoluteStartColumn, resolver, frc, shapedText)
    withExtraAdvances(snappedToCells(laidOut, text, absoluteStartColumn, resolver), absoluteStartColumn, resolver)

  /** `stops` with each wide character of a code line set to two cells and each one-cell fallback character to one
    * ([[WideCells]]), the edges after it moved to match.
    */
  private def snappedToCells(
    stops: ShapedCaretStops.Stops,
    text: String,
    absoluteStartColumn: Int,
    resolver: LineFontResolver
  ): ShapedCaretStops.Stops =
    if resolver.wideCellPx <= 0.0f then stops
    else
      val carets =
        WideCells.snapped(
          stops.carets,
          text,
          resolver.wideCellPx,
          index => resolver.isFallbackAt(absoluteStartColumn + index)
        )
      ShapedCaretStops.Stops(
        carets,
        stops.trailing.map(edges =>
          IArray.tabulate(edges.length)(index => edges(index) + carets(index) - stops.carets(index))
        )
      )

  /** `stops` with each column's extra advance added to every stop after it. */
  private def withExtraAdvances(
    stops: ShapedCaretStops.Stops,
    absoluteStartColumn: Int,
    resolver: LineFontResolver
  ): ShapedCaretStops.Stops =
    if resolver.extraAdvances.isEmpty then stops
    else
      val shifts = (0 until stops.carets.length).scanLeft(0.0f) { (sum, index) =>
        sum + resolver.extraAdvances.getOrElse(absoluteStartColumn + index, 0.0f)
      }
      def shifted(xs: IArray[Float]): IArray[Float] = IArray.tabulate(xs.length)(index => xs(index) + shifts(index))
      ShapedCaretStops.Stops(shifted(stops.carets), stops.trailing.map(shifted))

  private def layoutStopsOf(
    text: String,
    spans: Vector[ShapedCaretStops.Span],
    frc: FontRenderContext,
    shapedText: ShapedTextCache
  ): ShapedCaretStops.Stops =
    if measuresByGlyphs(text, spans.map(_.font), frc, shapedText.probes) then
      shapedText.countExactMeasurement()
      ShapedCaretStops.of(text, spans, frc, shapedText.glyphAdvances)
    else
      val attributed = AttributedString(text)
      spans.foreach(span => attributed.addAttribute(TextAttribute.FONT, span.font, span.start, span.end))
      val layout = textLayoutOf(attributed, frc, shapedText)
      ShapedCaretStops.Stops(
        IArray.tabulate(text.length + 1)(index =>
          if index == text.length then layout.getAdvance else layout.getCaretInfo(TextHitInfo.leading(index))(0)
        ),
        None
      )

  private def plainStops(
    text: String,
    absoluteStartColumn: Int,
    resolver: LineFontResolver,
    frc: FontRenderContext,
    shapedText: ShapedTextCache
  ): ShapedCaretStops.Stops =
    settleFonts(resolver, frc)
    // One font per contiguous run of equal per-column fonts, so a mixed-size rich line's caret advances (and thus wrap
    // points and widths) match the per-run glyphs the draw path paints.
    layoutStopsOf(
      text,
      resolver
        .fontRuns(absoluteStartColumn, absoluteStartColumn + text.length)
        .map(span =>
          ShapedCaretStops.Span(span.startColumn - absoluteStartColumn, span.endColumn - absoluteStartColumn, span.font)
        ),
      frc,
      shapedText
    )

  /** [[layoutStops]] for a line with hidden columns: the visible characters are laid out as if the hidden ones were not
    * in the text, and each hidden column takes the stop of the next visible one.
    */
  private def visibleStops(
    text: String,
    absoluteStartColumn: Int,
    resolver: LineFontResolver,
    frc: FontRenderContext,
    shapedText: ShapedTextCache
  ): ShapedCaretStops.Stops =
    settleFonts(resolver, frc)
    val hidden        = resolver.hiddenMask(absoluteStartColumn, text.length)
    val visibleBefore = hidden.scanLeft(0)((count, isHidden) => if isHidden then count else count + 1)
    val visibleText   = text.indices.filterNot(hidden(_)).map(text.charAt(_)).mkString
    if visibleText.isEmpty then ShapedCaretStops.Stops(IArray.fill(text.length + 1)(0.0f), None)
    else
      val spans = resolver
        .fontRuns(absoluteStartColumn, absoluteStartColumn + text.length)
        .filterNot(_.hidden)
        .map(span =>
          ShapedCaretStops.Span(
            visibleBefore(span.startColumn - absoluteStartColumn),
            visibleBefore(span.endColumn - absoluteStartColumn),
            span.font
          )
        )
      val visible = layoutStopsOf(visibleText, spans, frc, shapedText)
      def spread(xs: IArray[Float]): IArray[Float] =
        val full = new Array[Float](text.length + 1)
        full(text.length) = xs(visibleText.length)
        (text.length - 1 to 0 by -1).foreach { index =>
          full(index) = if hidden(index) then full(index + 1) else xs(visibleBefore(index))
        }
        IArray.unsafeFromArray(full)
      ShapedCaretStops.Stops(spread(visible.carets), visible.trailing.map(spread))
