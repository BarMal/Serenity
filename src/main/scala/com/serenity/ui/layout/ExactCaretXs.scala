package com.serenity.ui.layout

import com.serenity.ui.layout.TextCaretMeasurement.{FontSpan, LineFontResolver}
import com.serenity.ui.layout.TextMeasurer.LineFonts

/** The measurement a row falls back to when summed glyph advances cannot answer: each of its font runs shaped whole by
  * the measurer, as the draw path shapes them.
  */
private[layout] object ExactCaretXs:

  /** A row's caret stops, and for each boundary the right edge of the characters before it shaped alone, which the stop
    * itself cannot say: a stop carries the kerning of the pair it ends, and a row that ends there feels none.
    */
  final case class Stops(carets: IArray[Float], edges: IArray[Float])

  /** Carets read from one exact measurement of `text`: exact for any text, but a shaping of the whole row. */
  private[layout] def layoutCaretXs(
    text: String,
    absoluteStartColumn: Int,
    resolver: LineFontResolver,
    measuring: MeasuredBy
  ): IArray[Float] =
    layoutStops(text, absoluteStartColumn, resolver, measuring).carets

  private[layout] def layoutStops(
    text: String,
    absoluteStartColumn: Int,
    resolver: LineFontResolver,
    measuring: MeasuredBy
  ): Stops =
    measuring.memos.countExactMeasurement()
    val laidOut =
      if resolver.hasHidden then visibleStops(text, absoluteStartColumn, resolver, measuring.measurer)
      else stopsOf(text, spansOf(resolver, absoluteStartColumn, text.length, _ => true), measuring.measurer)
    withExtraAdvances(snappedToCells(laidOut, text, absoluteStartColumn, resolver), absoluteStartColumn, resolver)

  /** `stops` with each wide character of a code line set to two cells and each one-cell fallback character to one
    * ([[WideCells]]), the edges after it moved to match.
    */
  private def snappedToCells(stops: Stops, text: String, absoluteStartColumn: Int, resolver: LineFontResolver): Stops =
    if resolver.wideCellPx <= 0.0f then stops
    else
      val carets =
        WideCells.snapped(
          stops.carets,
          text,
          resolver.wideCellPx,
          index => resolver.isFallbackAt(absoluteStartColumn + index)
        )
      Stops(carets, IArray.tabulate(carets.length)(index => stops.edges(index) + carets(index) - stops.carets(index)))

  /** `stops` with each column's extra advance added to every stop after it. */
  private def withExtraAdvances(stops: Stops, absoluteStartColumn: Int, resolver: LineFontResolver): Stops =
    if resolver.extraAdvances.isEmpty then stops
    else
      val shifts = (0 until stops.carets.length).scanLeft(0.0f) { (sum, index) =>
        sum + resolver.extraAdvances.getOrElse(absoluteStartColumn + index, 0.0f)
      }
      def shifted(xs: IArray[Float]): IArray[Float] = IArray.tabulate(xs.length)(index => xs(index) + shifts(index))
      Stops(shifted(stops.carets), shifted(stops.edges))

  /** The resolver's runs over `length` columns from `absoluteStartColumn` that `keep` holds, in the text's columns. */
  private def spansOf(
    resolver: LineFontResolver,
    absoluteStartColumn: Int,
    length: Int,
    keep: FontSpan => Boolean
  ): Vector[FontSpan] =
    resolver
      .fontRuns(absoluteStartColumn, absoluteStartColumn + length)
      .filter(keep)
      .map(span =>
        span
          .copy(startColumn = span.startColumn - absoluteStartColumn, endColumn = span.endColumn - absoluteStartColumn)
      )

  /** The stops of `text` cut into consecutive `spans` of one font each. Nothing kerns across a change of font, so every
    * span is shaped on its own and starts where the one before it ended.
    */
  private def stopsOf(text: String, spans: Vector[FontSpan], measurer: TextMeasurer): Stops =
    val carets = new Array[Float](text.length + 1)
    val edges  = new Array[Float](text.length + 1)
    val advance = spans.foldLeft(0.0f) { (offset, span) =>
      val length = span.endColumn - span.startColumn
      val (spanCarets, spanEdges) =
        measurer.exactCaretsAndEdges(
          text.substring(span.startColumn, span.endColumn),
          0,
          LineFonts(span.font, Vector.empty)
        )
      (0 until length).foreach { char =>
        carets(span.startColumn + char) = offset + spanCarets(char)
        edges(span.startColumn + char) = offset + spanEdges(char)
      }
      offset + spanCarets(length)
    }
    carets(text.length) = advance
    edges(text.length) = advance
    Stops(IArray.unsafeFromArray(carets), IArray.unsafeFromArray(edges))

  /** [[layoutStops]] for a line with hidden columns: the visible characters are laid out as if the hidden ones were not
    * in the text, and each hidden column takes the stop of the next visible one.
    */
  private def visibleStops(
    text: String,
    absoluteStartColumn: Int,
    resolver: LineFontResolver,
    measurer: TextMeasurer
  ): Stops =
    val hidden        = resolver.hiddenMask(absoluteStartColumn, text.length)
    val visibleBefore = hidden.scanLeft(0)((count, isHidden) => if isHidden then count else count + 1)
    val visibleText   = text.indices.filterNot(hidden(_)).map(text.charAt(_)).mkString
    if visibleText.isEmpty then
      val none = IArray.fill(text.length + 1)(0.0f)
      Stops(none, none)
    else
      val spans = spansOf(resolver, absoluteStartColumn, text.length, !_.hidden).map(span =>
        span.copy(startColumn = visibleBefore(span.startColumn), endColumn = visibleBefore(span.endColumn))
      )
      val visible = stopsOf(visibleText, spans, measurer)
      def spread(xs: IArray[Float]): IArray[Float] =
        val full = new Array[Float](text.length + 1)
        full(text.length) = xs(visibleText.length)
        (text.length - 1 to 0 by -1).foreach { index =>
          full(index) = if hidden(index) then full(index + 1) else xs(visibleBefore(index))
        }
        IArray.unsafeFromArray(full)
      Stops(spread(visible.carets), spread(visible.edges))
