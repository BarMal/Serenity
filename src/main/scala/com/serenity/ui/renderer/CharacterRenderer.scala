package com.serenity.ui.renderer

import java.awt.Color

import com.serenity.animation.AnimationState
import com.serenity.lsp.config.LanguageId
import com.serenity.state.models.TextVisualLine
import com.serenity.ui.layout.CharWidth
import com.serenity.ui.theme.{LexState, StyledText, TextStyle, Theme}

object CharacterRenderer:

  /** A run of text to paint, with both coordinates it sits at: `startX` is a *cell* column on the screen grid (a wide
    * glyph occupies two of them), `bufferStartColumn` the column of its first character in the buffer line. They
    * advance at different rates, so a caller that reconstructed one from the other drifted by one per wide glyph
    * (#1271) -- the run carries both instead.
    */
  final private case class TextRun(startX: Int, content: String, bufferStartColumn: Int = 0)
  final private case class CollectedRuns(runs: List[TextRun], endX: Int)

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
    styledSegments: Option[List[StyledText]] = None,
    lexStartState: LexState = LexState.Default,
    maxColumn: Option[Int] = None
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
          bufferStartColumn,
          maxColumn
        )
      case None if syntaxHighlightingEnabled =>
        val styledTexts = com.serenity.ui.theme.ThemeManager.highlightLine(content, theme, language, lexStartState)
        renderStyledLineWithAnimation(
          surface,
          x,
          y,
          styledTexts,
          theme,
          screenAnimations,
          bufferLine,
          bufferStartColumn,
          maxColumn
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
          bufferStartColumn = bufferStartColumn,
          maxColumn = maxColumn
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
    bufferStartColumn: Int = 0,
    maxColumn: Option[Int] = None
  ): Unit =
    val collectedRuns = collectPlainRuns(x, content, tabWidth, bufferStartColumn)
    renderAnimatedRuns(surface, y, collectedRuns.runs, theme, screenAnimations, bufferLine, maxColumn)

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
    clipRightXPx: Option[Float] = None,
    lexStartState: LexState = LexState.Default
  ): Unit =
    GraphemeMeasuredLineRenderer.render(
      surface,
      xOriginPx,
      yPx,
      lineHeightPx,
      ascentPx,
      visualLine,
      theme,
      animations,
      syntaxHighlightingEnabled,
      language,
      styledSegments,
      clipRightXPx,
      lexStartState
    )

  private def renderStyledLineWithAnimation(
    surface: RenderSurface,
    x: Int,
    y: Int,
    styledTexts: List[com.serenity.ui.theme.StyledText],
    theme: Theme,
    screenAnimations: AnimationState,
    bufferLine: Int,
    bufferStartColumn: Int,
    maxColumn: Option[Int]
  ): Unit =
    // Both coordinates are carried across the segments rather than one being derived from the other: `currentX` counts
    // screen cells (a wide glyph takes two, a tab as many as it expands to) and `currentColumn` buffer characters, so
    // the animation lookup inside each segment stays on the right column past a wide glyph (#1271).
    styledTexts.foldLeft((x, bufferStartColumn)) {
      case ((currentX, currentColumn), styledText) =>
        val segmentTheme = theme.copy(
          foreground = styledText.foregroundColor,
          background = styledText.backgroundColor
        )
        val collectedRuns = collectPlainRuns(currentX, styledText.content, tabWidth = 4, currentColumn)
        withStyle(surface, styledText.style) {
          renderAnimatedRuns(surface, y, collectedRuns.runs, segmentTheme, screenAnimations, bufferLine, maxColumn)
        }
        (collectedRuns.endX, currentColumn + styledText.content.length)
    }: Unit

  private def withStyle(surface: RenderSurface, style: TextStyle)(render: => Unit): Unit =
    surface.enableStyle(style)
    try render
    finally surface.disableStyle(style)

  /** `bufferStartColumn` is the buffer column `content`'s first character sits at, tracked alongside the cell column so
    * each run can carry its own (see [[TextRun]]). The two advance independently: a cell column moves by a glyph's
    * display width and by a tab's expansion, a buffer column by one character per codepoint.
    */
  private def collectPlainRuns(
    startX: Int,
    content: String,
    tabWidth: Int,
    bufferStartColumn: Int = 0
  ): CollectedRuns =
    final case class PlainRunState(
        completed: List[TextRun],
        currentText: StringBuilder,
        currentStartX: Int,
        currentX: Int,
        currentStartColumn: Int,
        currentColumn: Int
    ):
      def flush: PlainRunState =
        if currentText.length > 0 then
          copy(
            completed = TextRun(currentStartX, currentText.toString, currentStartColumn) :: completed,
            currentText = StringBuilder()
          )
        else this

    val initial    = PlainRunState(Nil, StringBuilder(), startX, startX, bufferStartColumn, bufferStartColumn)
    val codePoints = content.codePoints().iterator()
    @annotation.tailrec
    def consume(state: PlainRunState): PlainRunState =
      if !codePoints.hasNext then state.flush
      else
        val codePoint  = codePoints.nextInt()
        val nextColumn = state.currentColumn + Character.charCount(codePoint)
        val nextState = codePoint match
          case '\t' =>
            val flushed     = state.flush
            val spacesToAdd = tabWidth - (flushed.currentX % tabWidth)
            val tabSpaces   = " " * spacesToAdd
            // The whole expansion stands for the one tab character, so the run starts at the tab's own buffer column.
            flushed.copy(
              completed = TextRun(flushed.currentX, tabSpaces, flushed.currentColumn) :: flushed.completed,
              currentStartX = flushed.currentX + spacesToAdd,
              currentX = flushed.currentX + spacesToAdd,
              currentStartColumn = nextColumn,
              currentColumn = nextColumn
            )
          case visible if isVisibleCodePoint(visible) =>
            val start       = if state.currentText.length == 0 then state.currentX else state.currentStartX
            val startColumn = if state.currentText.length == 0 then state.currentColumn else state.currentStartColumn
            state.currentText.appendAll(Character.toChars(visible))
            state.copy(
              currentStartX = start,
              currentX = state.currentX + displayWidth(visible),
              currentStartColumn = startColumn,
              currentColumn = nextColumn
            )
          case _ =>
            val flushed = state.flush
            flushed.copy(currentStartX = flushed.currentX, currentStartColumn = nextColumn, currentColumn = nextColumn)
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

  private def flushPlainRuns(surface: RenderSurface, y: Int, runs: List[TextRun]): Unit =
    runs.foreach(run => surface.putString(run.startX, y, run.content))

  private def renderAnimatedRuns(
    surface: RenderSurface,
    y: Int,
    runs: List[TextRun],
    theme: Theme,
    screenAnimations: AnimationState,
    bufferLine: Int,
    maxColumn: Option[Int]
  ): Unit =
    val clippedRuns = runs.flatMap(clipRunToColumn(_, maxColumn))
    if screenAnimations.animations.isEmpty then
      surface.setForegroundColor(theme.foreground)
      surface.setBackgroundColor(theme.background)
      clippedRuns.foreach(run => surface.putString(run.startX, y, run.content))
    else
      clippedRuns.foreach { run =>
        val grouped = groupRunByEffectiveColors(run, theme, screenAnimations, bufferLine)
        grouped.foreach {
          case (startX, text, foreground, background) =>
            surface.setForegroundColor(foreground)
            surface.setBackgroundColor(background)
            surface.putString(startX, y, text)
        }
      }

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

  /** Splits one run into the sub-runs that share an effective colour, walking it by codepoint rather than by `Char`.
    *
    * Both coordinates it tracks come from the run itself and advance at their own rate: the screen column by each
    * glyph's display width, the buffer column by each codepoint's character count. Deriving one from the other by a
    * single character index put every group after a wide glyph one cell early and looked up the wrong animation cell
    * (#1271), and stepping by `Char` could split a surrogate pair across two groups when the animation gave its halves
    * different colours, painting two broken halves instead of one glyph.
    */
  private def groupRunByEffectiveColors(
    run: TextRun,
    theme: Theme,
    screenAnimations: AnimationState,
    bufferLine: Int
  ): List[(Int, String, Color, Color)] =
    final case class ColorRunState(
        completed: List[(Int, String, Color, Color)],
        currentText: StringBuilder,
        currentStartX: Int,
        currentForeground: Color,
        currentBackground: Color,
        screenX: Int,
        bufferColumn: Int
    ):
      def flush: ColorRunState =
        if currentText.length > 0 then
          copy(
            completed = (currentStartX, currentText.toString, currentForeground, currentBackground) :: completed,
            currentText = StringBuilder()
          )
        else this

    val initial =
      ColorRunState(
        Nil,
        StringBuilder(),
        run.startX,
        theme.foreground,
        theme.background,
        run.startX,
        run.bufferStartColumn
      )
    val codePoints = run.content.codePoints().iterator()

    @annotation.tailrec
    def consume(state: ColorRunState): ColorRunState =
      if !codePoints.hasNext then state.flush
      else
        val codePoint  = codePoints.nextInt()
        val glyph      = new String(Character.toChars(codePoint))
        val cell       = screenAnimations.getCell(state.bufferColumn, bufferLine)
        val foreground = cell.flatMap(_.currentForeground).getOrElse(theme.foreground)
        val background = cell.flatMap(_.currentBackground).getOrElse(theme.background)
        val advanced = state.copy(
          screenX = state.screenX + displayWidth(codePoint),
          bufferColumn = state.bufferColumn + Character.charCount(codePoint)
        )
        val nextState =
          if state.currentText.length == 0 then
            advanced.copy(
              currentText = StringBuilder(glyph),
              currentStartX = state.screenX,
              currentForeground = foreground,
              currentBackground = background
            )
          else if foreground == state.currentForeground && background == state.currentBackground then
            state.currentText.append(glyph)
            advanced
          else
            advanced.flush.copy(
              currentText = StringBuilder(glyph),
              currentStartX = state.screenX,
              currentForeground = foreground,
              currentBackground = background
            )
        consume(nextState)

    consume(initial).completed.reverse

  private def blendColors(foreground: Color, background: Color, opacity: Double): Color =
    val t = opacity.max(0.0).min(1.0)
    new Color(
      math.round(background.getRed + (foreground.getRed - background.getRed) * t).toInt,
      math.round(background.getGreen + (foreground.getGreen - background.getGreen) * t).toInt,
      math.round(background.getBlue + (foreground.getBlue - background.getBlue) * t).toInt
    )
