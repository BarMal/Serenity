package com.serenity.ui.renderer

import java.util.LinkedHashMap

import com.serenity.lsp.config.LanguageId
import com.serenity.lsp.model.SemanticToken
import com.serenity.state.models.{IndentationSettings, TextVisualLine}
import com.serenity.text.TextEditing
import com.serenity.ui.layout.{CharWidth, TabStops}
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
    tabWidth: Int = IndentationSettings.DefaultTabSize
  ): Unit =
    val collectedRuns = collectPlainRuns(x, x, content, tabWidth)
    flushPlainRuns(surface, y, collectedRuns.runs)

  /** The characters of `visualLine` in buffer columns `[rangeStart, rangeEnd)` painted on the cell grid in the colours
    * and style the surface has, each on the cell the row's caret stops give it (so after a wide character, a tab or a
    * character a fallback font draws, it is still where the row measured it) and in the fallback font the row draws it
    * in. `originCell` is the cell of the row's first caret stop; cells outside `[leftCell, rightCell)` are not painted.
    */
  def renderCellRange(
    surface: RenderSurface,
    visualLine: TextVisualLine,
    rangeStart: Int,
    rangeEnd: Int,
    originCell: Int,
    y: Int,
    leftCell: Int,
    rightCell: Int,
    cellPx: Int,
    fallback: Option[CellFallback]
  ): Unit =
    val text      = visualLine.text
    val stretches = fallback.fold(Vector.empty)(_.stretchesOf(text))
    def cellOf(column: Int): Int =
      originCell + math.round(visualLine.xForColumn(column).getOrElse(visualLine.widthPx) / cellPx.toFloat)
    def familyAt(index: Int): Option[String] =
      stretches.collectFirst { case (from, until, family) if index >= from && index < until => family }.flatten
    @annotation.tailrec
    def paint(column: Int): Unit =
      val index = column - visualLine.startColumn
      if column < rangeEnd && index < text.length then
        if index < 0 then paint(column + 1)
        else
          val codePoint = text.codePointAt(index)
          val end       = CellRuns.endOfMarks(text, index + Character.charCount(codePoint))
          val cell      = cellOf(column)
          val cells     = cellOf(column + end - index) - cell
          if cell >= leftCell && cell < rightCell then
            val shown =
              if Character.isISOControl(codePoint) then " ".repeat(math.max(1, cells)) else text.substring(index, end)
            familyAt(index).fold(surface.putString(cell, y, shown))(family =>
              surface.text.withFallbackFamily(family)(
                withinCells(surface, cell, y, math.max(1, cells))(surface.putString(cell, y, shown))
              )
            )
          paint(column + end - index)
    paint(rangeStart)

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
    highlightCache: com.serenity.ui.theme.ThemeHighlightCache = com.serenity.ui.theme.ThemeHighlightCache(),
    tabWidth: Int = IndentationSettings.DefaultTabSize,
    fallback: Option[CellFallback] = None
  ): Unit =
    styledSegments match
      case Some(styledTexts) =>
        renderStyledLine(surface, x, y, styledTexts, theme, maxColumn, tabWidth, fallback)
      case None if syntaxHighlightingEnabled =>
        val styledTexts = highlightCache.highlightLine(content, theme, language, semanticTokens)
        renderStyledLine(surface, x, y, styledTexts, theme, maxColumn, tabWidth, fallback)
      case None =>
        renderThemedPlain(surface, x, y, content, theme, tabWidth, maxColumn, fallback)

  def renderThemedPlain(
    surface: RenderSurface,
    x: Int,
    y: Int,
    content: String,
    theme: Theme,
    tabWidth: Int = IndentationSettings.DefaultTabSize,
    maxColumn: Option[Int] = None,
    fallback: Option[CellFallback] = None
  ): Unit =
    val collectedRuns = collectPlainRuns(x, x, content, tabWidth)
    renderThemedRuns(surface, y, collectedRuns.runs, theme, maxColumn, fallback)

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
      val segments =
        styledSegments.getOrElse {
          if syntaxHighlightingEnabled then highlightCache.highlightLine(text, theme, language, semanticTokens)
          else List(StyledText(text, TextStyle.normal, theme.foreground, theme.background))
        }
      val runs = MeasuredLineRuns.of(visualLine, segments, graphemeCache.spansFor(text), theme)

      val shaped = surface.text.shapesStretchesOf(text)

      def paintRun(
        run: MeasuredRun,
        stretch: String,
        from: Int,
        until: Int,
        family: Option[String] = None
      ): Unit =
        val startXPx      = xOriginPx + run.minXPx
        val endXPx        = xOriginPx + run.maxXPx
        val clippedEndXPx = clipRightXPx.fold(endXPx)(_.min(endXPx))
        val widthPx       = clippedEndXPx - startXPx
        if widthPx > 0.0f then
          surface.setForegroundColor(run.foreground)
          surface.setBackgroundColor(run.background)
          withStyle(surface, run.style) {
            // A fallback glyph can stand taller than the row it sits in, and ink outside the row is not ink a repaint
            // of the rows beside it would put back, so it is cut to the row.
            val clipToRow = family.isDefined
            def draw(): Unit =
              if shaped then
                surface.text.drawShapedRunPx(
                  startXPx,
                  yPx,
                  widthPx,
                  lineHeightPx,
                  ascentPx,
                  stretch,
                  from,
                  until,
                  clipToRow
                )
              else surface.text.drawRunPx(startXPx, yPx, widthPx, lineHeightPx, ascentPx, run.text, clipToRow)
            family.fold(draw())(surface.text.withFallbackFamily(_)(draw()))
          }

      def paintSegment(segment: FontSegment): Unit =
        if shaped then
          MeasuredLineRuns
            .stretched(segment.runs, segment.text)
            .foreach(placed => paintRun(placed.run, placed.stretch, placed.from, placed.until, segment.family))
        else segment.runs.foreach(run => paintRun(run, run.text, 0, run.text.length, segment.family))

      if visualLine.fontRuns.isEmpty then
        if shaped then
          MeasuredLineRuns
            .stretched(runs, text)
            .foreach(placed => paintRun(placed.run, placed.stretch, placed.from, placed.until))
        else runs.foreach(run => paintRun(run, run.text, 0, run.text.length))
      else MeasuredLineRuns.byFont(visualLine, runs).foreach(paintSegment)

  private def renderStyledLine(
    surface: RenderSurface,
    x: Int,
    y: Int,
    styledTexts: List[com.serenity.ui.theme.StyledText],
    theme: Theme,
    maxColumn: Option[Int],
    tabWidth: Int,
    fallback: Option[CellFallback]
  ): Unit =
    styledTexts.foldLeft(x) { (currentX, styledText) =>
      val segmentTheme = theme.copy(
        foreground = styledText.foregroundColor,
        background = styledText.backgroundColor
      )
      val collectedRuns = collectPlainRuns(x, currentX, styledText.content, tabWidth)
      withStyle(surface, styledText.style) {
        renderThemedRuns(surface, y, collectedRuns.runs, segmentTheme, maxColumn, fallback)
      }
      collectedRuns.endX
    }: Unit

  private def withStyle(surface: RenderSurface, style: TextStyle)(render: => Unit): Unit =
    surface.enableStyle(style)
    try render
    finally surface.disableStyle(style)

  private def collectPlainRuns(originX: Int, startX: Int, content: String, tabWidth: Int): CollectedRuns =
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
            val spacesToAdd = TabStops.cellsAt(flushed.currentX - originX, tabWidth)
            val tabSpaces   = " " * spacesToAdd
            flushed.copy(
              completed = TextRun(flushed.currentX, tabSpaces) :: flushed.completed,
              currentStartX = flushed.currentX + spacesToAdd,
              currentX = flushed.currentX + spacesToAdd
            )
          case visible if isVisibleCodePoint(visible) =>
            val start = if state.currentText.length == 0 then state.currentX else state.currentStartX
            state.currentText.appendAll(Character.toChars(visible))
            state.copy(currentStartX = start, currentX = state.currentX + CharWidth.of(visible))
          case _ =>
            val flushed = state.flush
            flushed.copy(currentStartX = flushed.currentX)
        consume(nextState)

    val finalState = consume(initial)

    CollectedRuns(finalState.completed.reverse, finalState.currentX)

  private def isVisibleCodePoint(codePoint: Int): Boolean =
    !Character.isISOControl(codePoint)

  /** A run on the cell grid: the stretches the primary font draws as one string, and each character a fallback font
    * draws alone on the cell the caret stops put it on, since its glyph's advance has nothing to do with the grid.
    */
  private def flushRun(surface: RenderSurface, y: Int, run: TextRun, fallback: Option[CellFallback]): Unit =
    val stretches = fallback.fold(Vector.empty)(_.stretchesOf(run.content))
    if stretches.isEmpty then surface.putString(run.startX, y, run.content)
    else
      stretches.foreach { (from, until, family) =>
        val startCell = run.startX + CellRuns.cellsIn(run.content.substring(0, from))
        val stretch   = run.content.substring(from, until)
        family match
          case None => surface.putString(startCell, y, stretch)
          case Some(name) =>
            surface.text.withFallbackFamily(name) {
              CellRuns.clusters(stretch).foldLeft(startCell) { (cell, shown) =>
                val cells = CellRuns.cellsIn(shown)
                withinCells(surface, cell, y, cells)(surface.putString(cell, y, shown))
                cell + cells
              }: Unit
            }
      }

  /** A fallback glyph can stand taller than the row it sits in, and ink outside the row is not ink a repaint of the
    * rows beside it would put back, so it is cut to its own cells.
    */
  private def withinCells(surface: RenderSurface, cell: Int, y: Int, cells: Int)(draw: => Unit): Unit =
    surface.panelOutlines.fold(draw)(_.withRectClip(cell, y, cells, 1)(draw))

  private def flushPlainRuns(surface: RenderSurface, y: Int, runs: List[TextRun]): Unit =
    runs.foreach(flushRun(surface, y, _, None))

  private def renderThemedRuns(
    surface: RenderSurface,
    y: Int,
    runs: List[TextRun],
    theme: Theme,
    maxColumn: Option[Int],
    fallback: Option[CellFallback]
  ): Unit =
    surface.setForegroundColor(theme.foreground)
    surface.setBackgroundColor(theme.background)
    runs.flatMap(clipRunToColumn(_, maxColumn)).foreach(flushRun(surface, y, _, fallback))

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
              val width     = CharWidth.of(codePoint)
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
