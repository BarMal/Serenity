package com.serenity.ui.renderer

import java.awt.Color
import java.util.LinkedHashMap

import com.serenity.lsp.config.LanguageId
import com.serenity.lsp.model.SemanticToken
import com.serenity.state.models.TextVisualLine
import com.serenity.text.TextEditing
import com.serenity.ui.layout.CharWidth
import com.serenity.ui.theme.{StyledText, TextStyle, Theme}

/** Paints text onto a [[RenderSurface]] in the two ways a surface can accept it: the cell-grid path (`putString`,
  * counting screen columns) and the measured pixel path ([[renderMeasuredLine]], laying glyph runs out along a visual
  * line's own caret stops). Both share this object's run-splitting and codepoint width rules, so a line drawn either
  * way agrees with the other about where each character sits.
  */
object CharacterRenderer:

  /** A run of text to paint; `startX` is a *cell* column on the screen grid (a wide glyph occupies two of them). */
  final private case class TextRun(startX: Int, content: String)
  final private case class CollectedRuns(runs: List[TextRun], endX: Int)

  /** A grapheme cluster's boundaries within a line's text, as local character indices. */
  private[renderer] case class GraphemeSpan(startLocalIndex: Int, endLocalIndex: Int)

  /** A grapheme cluster paired with the effective style it should draw with. */
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

  private[renderer] def computeGraphemeSpans(text: String): Vector[GraphemeSpan] =
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

  def renderStyledString(
    surface: RenderSurface,
    x: Int,
    y: Int,
    content: String,
    theme: Theme,
    syntaxHighlightingEnabled: Boolean = true,
    language: Option[LanguageId] = None,
    styledSegments: Option[List[StyledText]] = None,
    semanticTokens: Option[List[SemanticToken]] = None,
    maxColumn: Option[Int] = None,
    highlightCache: com.serenity.ui.theme.ThemeHighlightCache = com.serenity.ui.theme.ThemeHighlightCache()
  ): Unit =
    styledSegments match
      case Some(styledTexts) =>
        renderStyledLine(surface, x, y, styledTexts, theme, maxColumn)
      case None if syntaxHighlightingEnabled =>
        val styledTexts = highlightCache.highlightLine(content, theme, language, semanticTokens)
        renderStyledLine(surface, x, y, styledTexts, theme, maxColumn)
      case None =>
        renderThemedPlain(surface, x, y, content, theme, maxColumn = maxColumn)

  def renderThemedPlain(
    surface: RenderSurface,
    x: Int,
    y: Int,
    content: String,
    theme: Theme,
    tabWidth: Int = 4,
    maxColumn: Option[Int] = None
  ): Unit =
    val collectedRuns = collectPlainRuns(x, content, tabWidth)
    renderThemedRuns(surface, y, collectedRuns.runs, theme, maxColumn)

  /** Render a visual line using pixel-precision caret stops.
    *
    * Walks the line by grapheme cluster (so a combining mark or an emoji sequence is never split across two runs),
    * groups consecutive clusters sharing the same effective fg/bg colour and style into runs, and calls
    * [[RenderSurface.drawRunPx]] for each. Callers must set the surface font before this call.
    *
    * @param xOriginPx
    *   pixel X of the pane's left edge
    * @param yPx
    *   pixel Y of the top of this visual line
    */
  def renderMeasuredLine(
    surface: RenderSurface,
    xOriginPx: Float,
    yPx: Int,
    lineHeightPx: Int,
    ascentPx: Int,
    visualLine: TextVisualLine,
    theme: Theme,
    syntaxHighlightingEnabled: Boolean = false,
    language: Option[LanguageId] = None,
    styledSegments: Option[List[StyledText]] = None,
    clipRightXPx: Option[Float] = None,
    semanticTokens: Option[List[SemanticToken]] = None,
    highlightCache: com.serenity.ui.theme.ThemeHighlightCache = com.serenity.ui.theme.ThemeHighlightCache(),
    graphemeCache: GraphemeSegmentationCache = GraphemeSegmentationCache()
  ): Unit =
    val text = visualLine.text
    if text.nonEmpty then
      val stops = visualLine.caretStops
      val styledSegments0 =
        styledSegments.getOrElse {
          if syntaxHighlightingEnabled then highlightCache.highlightLine(text, theme, language, semanticTokens)
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

      val spans = graphemeCache.spansFor(text)

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
          current match
            case None =>
              val run = MeasuredRun(
                localIndex,
                segmentForeground,
                segmentBackground,
                style,
                StringBuilder(grapheme),
                graphemeEnd
              )
              (completed, Some(run), graphemeEnd)

            case Some(run)
                if segmentForeground == run.foreground && segmentBackground == run.background && style == run.style =>
              run.text.append(grapheme)
              val updatedRun = run.copy(endLocalIndex = graphemeEnd)
              (completed, Some(updatedRun), graphemeEnd)

            case Some(run) =>
              val nextRun = MeasuredRun(
                graphemeStart,
                segmentForeground,
                segmentBackground,
                style,
                StringBuilder(grapheme),
                graphemeEnd
              )
              (run :: completed, Some(nextRun), graphemeEnd)
      }

      (currentRun.toList ::: runs).reverse.foreach(drawRun)

  private def renderStyledLine(
    surface: RenderSurface,
    x: Int,
    y: Int,
    styledTexts: List[com.serenity.ui.theme.StyledText],
    theme: Theme,
    maxColumn: Option[Int]
  ): Unit =
    styledTexts.foldLeft(x) { (currentX, styledText) =>
      val segmentTheme = theme.copy(
        foreground = styledText.foregroundColor,
        background = styledText.backgroundColor
      )
      val collectedRuns = collectPlainRuns(currentX, styledText.content, tabWidth = 4)
      withStyle(surface, styledText.style) {
        renderThemedRuns(surface, y, collectedRuns.runs, segmentTheme, maxColumn)
      }
      collectedRuns.endX
    }: Unit

  private def withStyle(surface: RenderSurface, style: TextStyle)(render: => Unit): Unit =
    surface.enableStyle(style)
    try render
    finally surface.disableStyle(style)

  private def collectPlainRuns(startX: Int, content: String, tabWidth: Int): CollectedRuns =
    final case class PlainRunState(
        completed: List[TextRun],
        currentText: StringBuilder,
        currentStartX: Int,
        currentX: Int
    ):
      def flush: PlainRunState =
        if currentText.length > 0 then
          copy(
            completed = TextRun(currentStartX, currentText.toString) :: completed,
            currentText = StringBuilder()
          )
        else this

    val initial    = PlainRunState(Nil, StringBuilder(), startX, startX)
    val codePoints = content.codePoints().iterator()
    @annotation.tailrec
    def consume(state: PlainRunState): PlainRunState =
      if !codePoints.hasNext then state.flush
      else
        val nextState = codePoints.nextInt() match
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

  /** How many cells a codepoint occupies on the grid: none for a combining mark, which composes onto the glyph before
    * it, otherwise its display width. [[com.serenity.ui.tui.TerminalScreenBuffer.putString]] advances by exactly this,
    * so a run laid out here lands on the cells the one before it actually filled -- counting one per codepoint instead
    * left every run after a wide glyph starting inside that glyph.
    */
  private def displayWidth(codePoint: Int): Int =
    val category = Character.getType(codePoint)
    if category == Character.NON_SPACING_MARK ||
        category == Character.COMBINING_SPACING_MARK ||
        category == Character.ENCLOSING_MARK
    then 0
    else CharWidth.of(codePoint)

  private def flushRun(surface: RenderSurface, y: Int, run: TextRun): Unit =
    surface.putString(run.startX, y, run.content)

  private def flushPlainRuns(surface: RenderSurface, y: Int, runs: List[TextRun]): Unit =
    runs.foreach(flushRun(surface, y, _))

  private def renderThemedRuns(
    surface: RenderSurface,
    y: Int,
    runs: List[TextRun],
    theme: Theme,
    maxColumn: Option[Int]
  ): Unit =
    surface.setForegroundColor(theme.foreground)
    surface.setBackgroundColor(theme.background)
    runs.flatMap(clipRunToColumn(_, maxColumn)).foreach(flushRun(surface, y, _))

  /** Cell-grid runs need no sub-character precision (unlike the measured pixel path's `clipRightXPx`), but a column is
    * a cell rather than a character: the content is truncated at the last glyph that fits whole, so a wide one is
    * dropped rather than half-drawn at the limit. `None` (the common case: word wrap on, or no pane-width limit given)
    * leaves every run untouched.
    */
  private def clipRunToColumn(run: TextRun, maxColumn: Option[Int]): Option[TextRun] =
    maxColumn match
      case None                               => Some(run)
      case Some(limit) if run.startX >= limit => None
      case Some(limit) =>
        val allowedCells = limit - run.startX
        if allowedCells >= run.content.length then Some(run)
        else
          @annotation.tailrec
          def fittingLength(index: Int, usedCells: Int): Int =
            if index >= run.content.length then index
            else
              val codePoint = run.content.codePointAt(index)
              val width     = displayWidth(codePoint)
              if usedCells + width > allowedCells then index
              else fittingLength(index + Character.charCount(codePoint), usedCells + width)

          Some(run.copy(content = run.content.take(fittingLength(0, 0))))

/** Bounded, access-order-eviction memoization cache of [[CharacterRenderer.computeGraphemeSpans]]'s result, one
  * grapheme boundary walk per distinct line of text. Grapheme-cluster boundaries are a pure function of a line's text
  * content alone -- independent of theme or caret-stop layout -- so they're memoized the same way `ThemeHighlightCache`
  * caches syntax highlighting: same input, forever the same output, nothing to invalidate. This also removes the
  * redundant double walk that used to happen every render call, where `graphemeBounds` and `graphemeChars` each
  * independently re-ran `TextEditing.nextGraphemeBoundary` over the same text.
  *
  * Instance-scoped (issue #1677): one instance is created per render-owning entity (held on
  * [[com.serenity.state.manager.RenderCaches]], threaded through [[RenderContext]] to
  * [[CharacterRenderer.renderMeasuredLine]]) rather than a JVM-wide singleton, so two independently constructed
  * instances share no cache state.
  */
final class GraphemeSegmentationCache:
  import CharacterRenderer.GraphemeSpan

  private val MaxGraphemeSegmentationCacheEntries = 4096

  private val cache =
    new LinkedHashMap[String, Vector[GraphemeSpan]](16, 0.75f, true):
      override def removeEldestEntry(eldest: java.util.Map.Entry[String, Vector[GraphemeSpan]]): Boolean =
        size() > MaxGraphemeSegmentationCacheEntries

  private[renderer] def spansFor(text: String): Vector[GraphemeSpan] =
    cache.synchronized(Option(cache.get(text))) match
      case Some(cached) => cached
      case None =>
        val computed = CharacterRenderer.computeGraphemeSpans(text)
        cache.synchronized {
          val _ = cache.put(text, computed)
        }
        computed

object GraphemeSegmentationCache:
  def apply(): GraphemeSegmentationCache = new GraphemeSegmentationCache
