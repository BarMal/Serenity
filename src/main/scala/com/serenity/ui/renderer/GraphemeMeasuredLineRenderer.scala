package com.serenity.ui.renderer

import java.awt.Color
import java.util.LinkedHashMap

import com.serenity.animation.AnimationState
import com.serenity.lsp.config.LanguageId
import com.serenity.state.models.TextVisualLine
import com.serenity.text.TextEditing
import com.serenity.ui.theme.{LexState, StyledText, TextStyle, Theme}

/** The pixel-precision, grapheme-cluster-aware line renderer behind [[CharacterRenderer.renderMeasuredLineWithAnimation]],
  * extracted to keep that one rendering concern -- and its dedicated grapheme-boundary cache -- separate from
  * [[CharacterRenderer]]'s cell-grid text painting.
  */
private[renderer] object GraphemeMeasuredLineRenderer:

  /** A grapheme cluster's boundaries within a line's text, as local character indices. */
  private case class GraphemeSpan(startLocalIndex: Int, endLocalIndex: Int)

  /** A grapheme cluster paired with the effective style it should draw with, before any per-frame animation override is
    * applied.
    */
  private case class MeasuredGrapheme(
      text: String,
      foreground: Color,
      background: Color,
      style: TextStyle,
      startLocalIndex: Int,
      endLocalIndex: Int
  )

  /** A grapheme cluster's measured pixel extent along a specific set of caret stops. */
  private case class GraphemeBounds(startLocalIndex: Int, endLocalIndex: Int, startXPx: Float, endXPx: Float)

  private val MaxGraphemeSegmentationCacheEntries = 4096

  /** Grapheme-cluster boundaries are a pure function of a line's text content alone -- independent of theme, animation
    * state, or caret-stop layout -- so they're memoized the same way `ThemeManager.highlightCache` caches syntax
    * highlighting: same input, forever the same output, nothing to invalidate. This also removes the redundant double
    * walk that used to happen every render call, where `graphemeBounds` and `graphemeChars` each independently re-ran
    * `TextEditing.nextGraphemeBoundary` over the same text.
    */
  private val graphemeSegmentationCache =
    new LinkedHashMap[String, Vector[GraphemeSpan]](16, 0.75f, true):
      override def removeEldestEntry(eldest: java.util.Map.Entry[String, Vector[GraphemeSpan]]): Boolean =
        size() > MaxGraphemeSegmentationCacheEntries

  private def graphemeSpans(text: String): Vector[GraphemeSpan] =
    graphemeSegmentationCache.synchronized(Option(graphemeSegmentationCache.get(text))) match
      case Some(cached) => cached
      case None =>
        val computed = computeGraphemeSpans(text)
        graphemeSegmentationCache.synchronized {
          val _ = graphemeSegmentationCache.put(text, computed)
        }
        computed

  private def computeGraphemeSpans(text: String): Vector[GraphemeSpan] =
    @annotation.tailrec
    def collect(localIndex: Int, acc: List[GraphemeSpan]): Vector[GraphemeSpan] =
      if localIndex >= text.length then acc.reverse.toVector
      else
        val nextIndex = TextEditing.nextGraphemeBoundary(text, localIndex).min(text.length)
        collect(nextIndex, GraphemeSpan(localIndex, nextIndex) :: acc)

    collect(0, Nil)

  private def withStyle(surface: RenderSurface, style: TextStyle)(render: => Unit): Unit =
    surface.enableStyle(style)
    try render
    finally surface.disableStyle(style)

  /** Render a visual line using pixel-precision caret stops.
    *
    * Groups consecutive characters that share the same effective fg/bg color into runs, then calls
    * [[RenderSurface.drawRunPx]] for each run. Callers must set the surface font before this call.
    *
    * @param xOriginPx
    *   pixel X of the pane's left edge
    * @param yPx
    *   pixel Y of the top of this visual line
    */
  def render(
    surface: RenderSurface,
    xOriginPx: Float,
    yPx: Int,
    lineHeightPx: Int,
    ascentPx: Int,
    visualLine: TextVisualLine,
    theme: Theme,
    animations: AnimationState,
    syntaxHighlightingEnabled: Boolean,
    language: Option[LanguageId],
    styledSegments: Option[List[StyledText]],
    clipRightXPx: Option[Float],
    lexStartState: LexState
  ): Unit =
    val text = visualLine.text
    if text.nonEmpty then
      val stops = visualLine.caretStops
      val styledSegments0 =
        styledSegments.getOrElse {
          if syntaxHighlightingEnabled then
            com.serenity.ui.theme.ThemeManager.highlightLine(text, theme, language, lexStartState)
          else List(StyledText(text, TextStyle.normal, theme.foreground, theme.background))
        }

      final case class MeasuredRun(
          startLocalIndex: Int,
          foreground: Color,
          background: Color,
          style: TextStyle,
          text: StringBuilder,
          endLocalIndex: Int
      )

      val spans = graphemeSpans(text)

      val graphemeBounds =
        @annotation.tailrec
        def collect(
          spanIndex: Int,
          stopIndex: Int,
          acc: List[GraphemeBounds]
        ): List[GraphemeBounds] =
          if spanIndex >= spans.length || stopIndex + 1 >= stops.length then acc.reverse
          else
            val span      = spans(spanIndex)
            val startStop = stops.lift(stopIndex).filter(_.column == visualLine.startColumn + span.startLocalIndex)
            val endStop   = stops.lift(stopIndex + 1).filter(_.column == visualLine.startColumn + span.endLocalIndex)
            (startStop, endStop) match
              case (Some(start), Some(end)) =>
                collect(
                  spanIndex + 1,
                  stopIndex + 1,
                  GraphemeBounds(span.startLocalIndex, span.endLocalIndex, start.xPx, end.xPx) :: acc
                )
              case _ => collect(spanIndex, stopIndex + 1, acc)

        collect(0, 0, Nil)

      // Every local index within a grapheme cluster's [start, end) shares that cluster's pixel bounds, so a run's
      // extent only needs the clusters it overlaps -- not a Vector expanded to one entry per character, which
      // `graphemeBoundsByLocalIndex` used to allocate before this was inlined into a single pass over the (much
      // smaller) cluster list.
      def visualExtentsForRange(startLocalIndex: Int, endLocalIndex: Int): (Float, Float) =
        graphemeBounds
          .foldLeft(Option.empty[(Float, Float)]) {
            case (acc, GraphemeBounds(clusterStart, clusterEnd, startXPx, endXPx))
                if clusterStart < endLocalIndex && clusterEnd > startLocalIndex =>
              acc match
                case Some((minXPx, maxXPx)) => Some((minXPx.min(startXPx), maxXPx.max(endXPx)))
                case None                   => Some((startXPx, endXPx))
            case (acc, _) => acc
          }
          .getOrElse((0.0f, 0.0f))

      def drawRun(run: MeasuredRun): Unit =
        val (minXPx, maxXPx) = visualExtentsForRange(run.startLocalIndex, run.endLocalIndex)
        val startXPx         = xOriginPx + minXPx
        val endXPx           = xOriginPx + maxXPx
        val clippedEndXPx    = clipRightXPx.fold(endXPx)(_.min(endXPx))
        val widthPx          = clippedEndXPx - startXPx
        if widthPx > 0.0f then
          surface.setForegroundColor(run.foreground)
          surface.setBackgroundColor(run.background)
          withStyle(surface, run.style) {
            surface.text.drawRunPx(startXPx, yPx, widthPx, lineHeightPx, ascentPx, run.text.toString)
          }

      val chars = styledSegments0
        .flatMap(segment =>
          segment.content.map(char => (char, segment.foregroundColor, segment.backgroundColor, segment.style))
        )
        .toVector
      val hasAnimations = animations.animations.nonEmpty

      val graphemeChars: Vector[MeasuredGrapheme] =
        spans.map { span =>
          val segmentStyle      = chars.lift(span.startLocalIndex).map(_._4).getOrElse(TextStyle.normal)
          val segmentForeground = chars.lift(span.startLocalIndex).map(_._2).getOrElse(theme.foreground)
          val segmentBackground = chars.lift(span.startLocalIndex).map(_._3).getOrElse(theme.background)
          MeasuredGrapheme(
            text.substring(span.startLocalIndex, span.endLocalIndex),
            segmentForeground,
            segmentBackground,
            segmentStyle,
            span.startLocalIndex,
            span.endLocalIndex
          )
        }

      val (runs, currentRun, _) = graphemeChars.foldLeft((List.empty[MeasuredRun], Option.empty[MeasuredRun], 0)) {
        case (
              (completed, current, localIndex),
              MeasuredGrapheme(grapheme, segmentForeground, segmentBackground, style, graphemeStart, graphemeEnd)
            ) =>
          val bufferColumn = visualLine.startColumn + graphemeStart
          val cell         = if hasAnimations then animations.getCell(bufferColumn, visualLine.bufferLine) else None
          val foreground   = cell.flatMap(_.currentForeground).getOrElse(segmentForeground)
          val background   = cell.flatMap(_.currentBackground).getOrElse(segmentBackground)

          current match
            case None =>
              val run = MeasuredRun(
                localIndex,
                foreground,
                background,
                style,
                StringBuilder(grapheme),
                graphemeEnd
              )
              (completed, Some(run), graphemeEnd)

            case Some(run) if foreground == run.foreground && background == run.background && style == run.style =>
              run.text.append(grapheme)
              val updatedRun = run.copy(endLocalIndex = graphemeEnd)
              (completed, Some(updatedRun), graphemeEnd)

            case Some(run) =>
              val nextRun = MeasuredRun(
                graphemeStart,
                foreground,
                background,
                style,
                StringBuilder(grapheme),
                graphemeEnd
              )
              (run :: completed, Some(nextRun), graphemeEnd)
      }

      (currentRun.toList ::: runs).reverse.foreach(drawRun)
