package com.serenity.ui.renderer

import scala.annotation.tailrec

import com.serenity.state.models.TextVisualLine
import com.serenity.ui.color.RenderColor
import com.serenity.ui.renderer.CharacterRenderer.GraphemeSpan
import com.serenity.ui.theme.{StyledText, TextStyle, Theme}

/** A stretch of a measured visual line drawn in one call: one colour pair, one style, and its pixel extent measured
  * from the line's left edge.
  */
final private[renderer] case class MeasuredRun(
    text: String,
    foreground: RenderColor,
    background: RenderColor,
    style: TextStyle,
    minXPx: Float,
    maxXPx: Float
)

/** A [[MeasuredRun]] with the stretch of text it shares a style, and so a font, with its neighbours: `stretch` is that
  * text and the run is `stretch.substring(from, until)`.
  */
final private[renderer] case class StretchedRun(run: MeasuredRun, stretch: String, from: Int, until: Int)

/** The part of a measured line drawn in one font: `family` is the fallback family, `None` for the line's own font. Its
  * `runs` are the line's runs cut to the segment, and `text` is the segment, which is what a stretch is shaped from so
  * that shaping never reaches across a font boundary.
  */
final private[renderer] case class FontSegment(family: Option[String], runs: Vector[MeasuredRun], text: String)

/** Splits a measured visual line into [[MeasuredRun]]s wherever a grapheme cluster's effective colours or style differ
  * from the cluster before it, so no cluster is ever split. Runs a full frame of prose through it, so it walks arrays
  * instead of building per-character collections.
  */
private[renderer] object MeasuredLineRuns:

  final private case class Attributes(foreground: RenderColor, background: RenderColor, style: TextStyle)

  def of(
    visualLine: TextVisualLine,
    segments: List[StyledText],
    spans: Vector[GraphemeSpan],
    theme: Theme
  ): Vector[MeasuredRun] =
    val text        = visualLine.text
    val bounds      = ClusterBounds(visualLine, spans)
    val styled      = segments.toArray
    val segmentEnds = styled.scanLeft(0)(_ + _.content.length).drop(1)
    val unstyled    = Attributes(theme.foreground, theme.background, TextStyle.normal)
    val attributesBySegment =
      styled.map(segment => Attributes(segment.foregroundColor, segment.backgroundColor, segment.style))
    val runs = Vector.newBuilder[MeasuredRun]

    @tailrec
    def segmentAt(localIndex: Int, from: Int): Int =
      if from < segmentEnds.length && segmentEnds(from) <= localIndex then segmentAt(localIndex, from + 1) else from

    def attributesOf(segment: Int): Attributes =
      if segment < attributesBySegment.length then attributesBySegment(segment) else unstyled

    def emit(start: Int, end: Int, attributes: Attributes, runText: java.lang.StringBuilder, boundsFrom: Int): Int =
      val (minXPx, maxXPx, next) = bounds.extent(start, end, boundsFrom)
      runs += MeasuredRun(
        runText.toString,
        attributes.foreground,
        attributes.background,
        attributes.style,
        minXPx,
        maxXPx
      )
      next

    def textOf(span: GraphemeSpan): java.lang.StringBuilder =
      new java.lang.StringBuilder().append(text, span.startLocalIndex, span.endLocalIndex)

    @tailrec
    def group(
      spanIndex: Int,
      segment: Int,
      start: Int,
      end: Int,
      attributes: Attributes,
      runText: java.lang.StringBuilder,
      boundsFrom: Int
    ): Unit =
      if spanIndex >= spans.length then
        val _ = emit(start, end, attributes, runText, boundsFrom)
      else
        val span           = spans(spanIndex)
        val spanSegment    = segmentAt(span.startLocalIndex, segment)
        val spanAttributes = attributesOf(spanSegment)
        if spanAttributes == attributes then
          val _ = runText.append(text, span.startLocalIndex, span.endLocalIndex)
          group(spanIndex + 1, spanSegment, start, span.endLocalIndex, attributes, runText, boundsFrom)
        else
          val next = emit(start, end, attributes, runText, boundsFrom)
          group(
            spanIndex + 1,
            spanSegment,
            span.startLocalIndex,
            span.endLocalIndex,
            spanAttributes,
            textOf(span),
            next
          )

    spans.headOption.foreach { first =>
      val segment = segmentAt(first.startLocalIndex, 0)
      group(1, segment, 0, first.endLocalIndex, attributesOf(segment), textOf(first), 0)
    }
    runs.result()

  /** `runs` cut at the boundaries of the line's font runs. The line's own font fills the gaps between them. A run cut
    * in two keeps the line's caret stop at the cut as the edge of both halves.
    */
  def byFont(visualLine: TextVisualLine, runs: Vector[MeasuredRun]): Vector[FontSegment] =
    val text    = visualLine.text
    val offsets = runs.scanLeft(0)(_ + _.text.length)
    val fonts   = visualLine.fontRuns

    def xAt(offset: Int): Float =
      visualLine.row.xForColumn(visualLine.startColumn + offset).fold(0.0f)(_ + visualLine.xOffsetPx)

    def cut(from: Int, until: Int): Vector[MeasuredRun] =
      runs.indices.flatMap { index =>
        val low  = math.max(offsets(index), from)
        val high = math.min(offsets(index + 1), until)
        Option.when(low < high) {
          val run = runs(index)
          run.copy(
            text = run.text.substring(low - offsets(index), high - offsets(index)),
            minXPx = if low == offsets(index) then run.minXPx else xAt(low),
            maxXPx = if high == offsets(index + 1) then run.maxXPx else xAt(high)
          )
        }
      }.toVector

    def segment(from: Int, until: Int, slot: Int): FontSegment =
      FontSegment(
        Option.when(slot > 0)(visualLine.fallbackFamilies.lift(slot - 1)).flatten,
        cut(from, until),
        text.substring(from, until)
      )

    val declared = Vector.tabulate(fonts.length / 3)(run => (fonts(3 * run), fonts(3 * run + 1), fonts(3 * run + 2)))
    val (covered, segments) = declared.foldLeft((0, Vector.empty[FontSegment])) {
      case ((reached, found), (from, until, slot)) =>
        val gap = Option.when(from > reached)(segment(reached, from, 0))
        (until, found ++ gap :+ segment(from, until, slot))
    }
    segments ++ Option.when(covered < text.length)(segment(covered, text.length, 0))

  /** Places each run in its style stretch -- the consecutive runs of one style, which draw in one font and so can be
    * shaped as one text -- where `runs` are the runs of `text`.
    */
  def stretched(runs: Vector[MeasuredRun], text: String): Vector[StretchedRun] =
    val offsets = runs.scanLeft(0)(_ + _.text.length)
    if runs.headOption.forall(first => runs.forall(_.style == first.style)) then
      Vector.tabulate(runs.length)(index => StretchedRun(runs(index), text, offsets(index), offsets(index + 1)))
    else mixedStretches(runs, text, offsets)

  private def mixedStretches(runs: Vector[MeasuredRun], text: String, offsets: Vector[Int]): Vector[StretchedRun] =
    val firsts = runs.indices.foldLeft(Vector.empty[Int]) { (found, index) =>
      found :+ (if index > 0 && runs(index - 1).style == runs(index).style then found(index - 1) else index)
    }
    val stretchTexts = runs.indices
      .groupMapReduce(firsts(_))(index => offsets(index + 1))(math.max)
      .map((first, end) => first -> text.substring(offsets(first), end.min(text.length)))
    runs.indices.map { index =>
      val first = firsts(index)
      StretchedRun(
        runs(index),
        stretchTexts.getOrElse(first, runs(index).text),
        offsets(index) - offsets(first),
        offsets(index + 1) - offsets(first)
      )
    }.toVector

  /** The pixel extent of each grapheme cluster whose two boundaries both have a caret stop, in line order. A cluster
    * whose stops are missing has no extent of its own.
    */
  final private class ClusterBounds(
      starts: Array[Int],
      ends: Array[Int],
      startXs: Array[Float],
      endXs: Array[Float],
      count: Int
  ):

    /** The extent of the clusters overlapping `[start, end)`, `(0, 0)` when there are none, and where the next, later
      * run may begin searching. Clusters are in ascending order, so the overlapping ones are contiguous.
      */
    def extent(start: Int, end: Int, from: Int): (Float, Float, Int) =
      val first = firstEndingAfter(start, from)
      @tailrec
      def widen(index: Int, minXPx: Float, maxXPx: Float, found: Boolean): (Float, Float) =
        if index >= count || starts(index) >= end then if found then (minXPx, maxXPx) else (0.0f, 0.0f)
        else if found then widen(index + 1, minXPx.min(startXs(index)), maxXPx.max(endXs(index)), found = true)
        else widen(index + 1, startXs(index), endXs(index), found = true)
      val (minXPx, maxXPx) = widen(first, 0.0f, 0.0f, found = false)
      (minXPx, maxXPx, first)

    @tailrec
    private def firstEndingAfter(start: Int, from: Int): Int =
      if from < count && ends(from) <= start then firstEndingAfter(start, from + 1) else from

  private object ClusterBounds:

    def apply(visualLine: TextVisualLine, spans: Vector[GraphemeSpan]): ClusterBounds =
      val measured = visualLine.row
      val offset   = visualLine.xOffsetPx
      val starts   = new Array[Int](spans.length)
      val ends     = new Array[Int](spans.length)
      val startXs  = new Array[Float](spans.length)
      val endXs    = new Array[Float](spans.length)
      @tailrec
      def collect(spanIndex: Int, stopIndex: Int, count: Int): Int =
        if spanIndex >= spans.length || stopIndex + 1 >= measured.caretCount then count
        else
          val span = spans(spanIndex)
          if measured.caretColumn(stopIndex) == visualLine.startColumn + span.startLocalIndex &&
              measured.caretColumn(stopIndex + 1) == visualLine.startColumn + span.endLocalIndex
          then
            starts(count) = span.startLocalIndex
            ends(count) = span.endLocalIndex
            startXs(count) = measured.caretXs(stopIndex) + offset
            endXs(count) = measured.caretXs(stopIndex + 1) + offset
            collect(spanIndex + 1, stopIndex + 1, count + 1)
          else collect(spanIndex, stopIndex + 1, count)
      new ClusterBounds(starts, ends, startXs, endXs, collect(0, 0, 0))
