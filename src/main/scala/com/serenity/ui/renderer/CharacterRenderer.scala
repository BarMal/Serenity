package com.serenity.ui.renderer

import java.awt.Color
import java.util.LinkedHashMap

import com.serenity.animation.AnimationState
import com.serenity.lsp.config.LanguageId
import com.serenity.state.models.TextVisualLine
import com.serenity.text.TextEditing
import com.serenity.ui.theme.{StyledText, TextStyle, Theme}

object CharacterRenderer:

  final private case class TextRun(startX: Int, content: String)
  final private case class CollectedRuns(runs: List[TextRun], endX: Int)

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

  def renderString(
    surface: RenderSurface,
    x: Int,
    y: Int,
    content: String
  ): Unit =
    renderStringPlain(surface, x, y, content)

  def renderStringPlain(
    surface: RenderSurface,
    x: Int,
    y: Int,
    content: String,
    tabWidth: Int = 4
  ): Unit =
    val collectedRuns = collectPlainRuns(x, content, tabWidth)
    flushPlainRuns(surface, y, collectedRuns.runs)

  def renderChar(
    surface: RenderSurface,
    x: Int,
    y: Int,
    char: Char
  ): Unit =
    val displayChar = char match
      case '_'                           => '_'
      case '\t'                          => '\t'
      case c if c.isControl && c != '\t' => ' '
      case c                             => c

    surface.putString(x, y, displayChar.toString)

  def isVisibleChar(char: Char): Boolean =
    isVisibleCodePoint(char.toInt)

  def renderCharWithOpacity(
    surface: RenderSurface,
    x: Int,
    y: Int,
    char: Char,
    foregroundColor: Color,
    backgroundColor: Color,
    opacity: Double
  ): Unit =
    if opacity >= 1.0 then
      surface.setForegroundColor(foregroundColor)
      surface.setBackgroundColor(backgroundColor)
      renderChar(surface, x, y, char)
    else if opacity <= 0.0 then ()
    else
      val blendedForeground = blendColors(foregroundColor, backgroundColor, opacity)
      surface.setForegroundColor(blendedForeground)
      surface.setBackgroundColor(backgroundColor)
      renderChar(surface, x, y, char)

  def renderStringWithAnimation(
    surface: RenderSurface,
    x: Int,
    y: Int,
    content: String,
    theme: Theme,
    screenAnimations: AnimationState,
    syntaxHighlightingEnabled: Boolean = true,
    language: Option[LanguageId] = None,
    bufferLine: Int = 0,
    bufferStartColumn: Int = 0,
    styledSegments: Option[List[StyledText]] = None
  ): Unit =
    styledSegments match
      case Some(styledTexts) =>
        renderStyledLineWithAnimation(
          surface,
          x,
          y,
          styledTexts,
          theme,
          screenAnimations,
          bufferLine,
          bufferStartColumn
        )
      case None if syntaxHighlightingEnabled =>
        val styledTexts = com.serenity.ui.theme.ThemeManager.highlightLine(content, theme, language)
        renderStyledLineWithAnimation(
          surface,
          x,
          y,
          styledTexts,
          theme,
          screenAnimations,
          bufferLine,
          bufferStartColumn
        )
      case None =>
        renderStringWithAnimationPlain(
          surface,
          x,
          y,
          content,
          theme,
          screenAnimations,
          bufferLine = bufferLine,
          bufferStartColumn = bufferStartColumn
        )

  def renderStringWithAnimationPlain(
    surface: RenderSurface,
    x: Int,
    y: Int,
    content: String,
    theme: Theme,
    screenAnimations: AnimationState,
    tabWidth: Int = 4,
    bufferLine: Int = 0,
    bufferStartColumn: Int = 0
  ): Unit =
    val collectedRuns = collectPlainRuns(x, content, tabWidth)
    renderAnimatedRuns(surface, x, y, collectedRuns.runs, theme, screenAnimations, bufferLine, bufferStartColumn)

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
  def renderMeasuredLineWithAnimation(
    surface: RenderSurface,
    xOriginPx: Float,
    yPx: Int,
    lineHeightPx: Int,
    ascentPx: Int,
    visualLine: TextVisualLine,
    theme: Theme,
    animations: AnimationState,
    syntaxHighlightingEnabled: Boolean = false,
    language: Option[LanguageId] = None,
    styledSegments: Option[List[StyledText]] = None,
    clipRightXPx: Option[Float] = None
  ): Unit =
    val text = visualLine.text
    if text.nonEmpty then
      val stops = visualLine.caretStops
      val styledSegments0 =
        styledSegments.getOrElse {
          if syntaxHighlightingEnabled then com.serenity.ui.theme.ThemeManager.highlightLine(text, theme, language)
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

  private def renderStyledLineWithAnimation(
    surface: RenderSurface,
    x: Int,
    y: Int,
    styledTexts: List[com.serenity.ui.theme.StyledText],
    theme: Theme,
    screenAnimations: AnimationState,
    bufferLine: Int = 0,
    bufferStartColumn: Int = 0
  ): Unit =
    styledTexts.foldLeft(x) { (currentX, styledText) =>
      val segmentTheme = theme.copy(
        foreground = styledText.foregroundColor,
        background = styledText.backgroundColor
      )
      val collectedRuns = collectPlainRuns(currentX, styledText.content, tabWidth = 4)
      withStyle(surface, styledText.style) {
        renderAnimatedRuns(
          surface,
          currentX,
          y,
          collectedRuns.runs,
          segmentTheme,
          screenAnimations,
          bufferLine,
          bufferStartColumn + (currentX - x)
        )
      }
      collectedRuns.endX
    }

  private def withStyle(surface: RenderSurface, style: TextStyle)(render: => Unit): Unit =
    surface.enableStyle(style)
    try render
    finally surface.disableStyle(style)

  private def collectPlainRuns(
    startX: Int,
    content: String,
    tabWidth: Int
  ): CollectedRuns =
    final case class PlainRunState(
        completed: List[TextRun],
        currentText: StringBuilder,
        currentStartX: Int,
        currentX: Int
    ):
      def flush: PlainRunState =
        if currentText.length > 0 then
          copy(completed = TextRun(currentStartX, currentText.toString) :: completed, currentText = StringBuilder())
        else this

    val initial    = PlainRunState(Nil, StringBuilder(), startX, startX)
    val codePoints = content.codePoints().iterator()
    @annotation.tailrec
    def consume(state: PlainRunState): PlainRunState =
      if !codePoints.hasNext then state.flush
      else
        val codePoint = codePoints.nextInt()
        val nextState = codePoint match
          case '\t' =>
            val flushed     = state.flush
            val spacesToAdd = tabWidth - (flushed.currentX % tabWidth)
            val tabSpaces   = " " * spacesToAdd
            flushed.copy(
              completed = TextRun(flushed.currentX, tabSpaces) :: flushed.completed,
              currentStartX = flushed.currentX + spacesToAdd,
              currentX = flushed.currentX + spacesToAdd
            )
          case visible if isVisibleCodePoint(visible) =>
            val start = if state.currentText.length == 0 then state.currentX else state.currentStartX
            state.currentText.appendAll(Character.toChars(visible))
            state.copy(currentStartX = start, currentX = state.currentX + displayWidth(visible))
          case _ =>
            val flushed = state.flush
            flushed.copy(currentStartX = flushed.currentX)
        consume(nextState)

    val finalState = consume(initial)

    CollectedRuns(finalState.completed.reverse, finalState.currentX)

  private def isVisibleCodePoint(codePoint: Int): Boolean =
    !Character.isISOControl(codePoint)

  private def displayWidth(codePoint: Int): Int =
    val category = Character.getType(codePoint)
    if category == Character.NON_SPACING_MARK ||
        category == Character.COMBINING_SPACING_MARK ||
        category == Character.ENCLOSING_MARK
    then 0
    else 1

  private def flushPlainRuns(surface: RenderSurface, y: Int, runs: List[TextRun]): Unit =
    runs.foreach(run => surface.putString(run.startX, y, run.content))

  private def renderAnimatedRuns(
    surface: RenderSurface,
    screenOriginX: Int,
    y: Int,
    runs: List[TextRun],
    theme: Theme,
    screenAnimations: AnimationState,
    bufferLine: Int,
    bufferStartColumn: Int
  ): Unit =
    if screenAnimations.animations.isEmpty then
      surface.setForegroundColor(theme.foreground)
      surface.setBackgroundColor(theme.background)
      runs.foreach(run => surface.putString(run.startX, y, run.content))
    else
      runs.foreach { run =>
        val grouped =
          groupRunByEffectiveColors(run, screenOriginX, theme, screenAnimations, bufferLine, bufferStartColumn)
        grouped.foreach {
          case (startX, text, foreground, background) =>
            surface.setForegroundColor(foreground)
            surface.setBackgroundColor(background)
            surface.putString(startX, y, text)
        }
      }

  private def groupRunByEffectiveColors(
    run: TextRun,
    screenOriginX: Int,
    theme: Theme,
    screenAnimations: AnimationState,
    bufferLine: Int,
    bufferStartColumn: Int
  ): List[(Int, String, Color, Color)] =
    val text = run.content

    final case class ColorRunState(
        completed: List[(Int, String, Color, Color)],
        currentText: StringBuilder,
        currentStartX: Int,
        currentForeground: Color,
        currentBackground: Color
    ):
      def flush: ColorRunState =
        if currentText.length > 0 then
          copy(
            completed = (currentStartX, currentText.toString, currentForeground, currentBackground) :: completed,
            currentText = StringBuilder()
          )
        else this

    val initial = ColorRunState(Nil, StringBuilder(), run.startX, theme.foreground, theme.background)
    val finalState = text.zipWithIndex
      .foldLeft(initial) {
        case (state, (char, index)) =>
          val bufferColumn = bufferStartColumn + (run.startX - screenOriginX) + index
          val cell         = screenAnimations.getCell(bufferColumn, bufferLine)
          val foreground   = cell.flatMap(_.currentForeground).getOrElse(theme.foreground)
          val background   = cell.flatMap(_.currentBackground).getOrElse(theme.background)

          if state.currentText.length == 0 then
            state.copy(
              currentText = StringBuilder(char.toString),
              currentStartX = run.startX + index,
              currentForeground = foreground,
              currentBackground = background
            )
          else if foreground == state.currentForeground && background == state.currentBackground then
            state.currentText.append(char)
            state
          else
            state.flush.copy(
              currentText = StringBuilder(char.toString),
              currentStartX = run.startX + index,
              currentForeground = foreground,
              currentBackground = background
            )
      }
      .flush

    finalState.completed.reverse

  private def blendColors(foreground: Color, background: Color, opacity: Double): Color =
    val t = opacity.max(0.0).min(1.0)
    new Color(
      math.round(background.getRed + (foreground.getRed - background.getRed) * t).toInt,
      math.round(background.getGreen + (foreground.getGreen - background.getGreen) * t).toInt,
      math.round(background.getBlue + (foreground.getBlue - background.getBlue) * t).toInt
    )
