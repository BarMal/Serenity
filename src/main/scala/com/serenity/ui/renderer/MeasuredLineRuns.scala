package com.serenity.ui.renderer

import java.awt.Color

import scala.annotation.tailrec

import com.serenity.state.models.TextVisualLine
import com.serenity.ui.renderer.CharacterRenderer.GraphemeSpan
import com.serenity.ui.theme.{StyledText, TextStyle, Theme}

/** A stretch of a measured visual line drawn in one call: one colour pair, one style, and its pixel extent measured
  * from the line's left edge.
  */
final private[renderer] case class MeasuredRun(
    text: String,
    foreground: Color,
    background: Color,
    style: TextStyle,
    minXPx: Float,
    maxXPx: Float
)

/** Splits a measured visual line into [[MeasuredRun]]s wherever a grapheme cluster's effective colours or style differ
  * from the cluster before it, so no cluster is ever split. Runs a full frame of prose through it, so it walks arrays
  * instead of building per-character collections.
  */
private[renderer] object MeasuredLineRuns:

  final private case class Attributes(foreground: Color, background: Color, style: TextStyle)

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
    val runs        = Vector.newBuilder[MeasuredRun]

    @tailrec
    def segmentAt(localIndex: Int, from: Int): Int =
      if from < segmentEnds.length && segmentEnds(from) <= localIndex then segmentAt(localIndex, from + 1) else from

    def attributesOf(segment: Int): Attributes =
      styled.lift(segment).fold(unstyled)(s => Attributes(s.foregroundColor, s.backgroundColor, s.style))

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
      val stops   = visualLine.caretStops
      val starts  = new Array[Int](spans.length)
      val ends    = new Array[Int](spans.length)
      val startXs = new Array[Float](spans.length)
      val endXs   = new Array[Float](spans.length)
      @tailrec
      def collect(spanIndex: Int, stopIndex: Int, count: Int): Int =
        if spanIndex >= spans.length || stopIndex + 1 >= stops.length then count
        else
          val span      = spans(spanIndex)
          val startStop = stops(stopIndex)
          val endStop   = stops(stopIndex + 1)
          if startStop.column == visualLine.startColumn + span.startLocalIndex &&
              endStop.column == visualLine.startColumn + span.endLocalIndex
          then
            starts(count) = span.startLocalIndex
            ends(count) = span.endLocalIndex
            startXs(count) = startStop.xPx
            endXs(count) = endStop.xPx
            collect(spanIndex + 1, stopIndex + 1, count + 1)
          else collect(spanIndex, stopIndex + 1, count)
      new ClusterBounds(starts, ends, startXs, endXs, collect(0, 0, 0))
