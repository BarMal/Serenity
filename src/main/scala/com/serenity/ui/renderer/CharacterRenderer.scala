package com.serenity.ui.renderer

import java.awt.Color

import com.serenity.animation.AnimationState
import com.serenity.lsp.config.LanguageId
import com.serenity.ui.theme.{StyledText, TextStyle, Theme}

object CharacterRenderer:

  private case class TextRun(startX: Int, content: String)
  private case class CollectedRuns(runs: List[TextRun], endX: Int)

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
    visualLine: com.serenity.ui.layout.TextVisualLine,
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
      val stopXPxByLocalIndex =
        @annotation.tailrec
        def expand(localIndex: Int, stopIndex: Int, acc: List[Float]): List[Float] =
          if localIndex > text.length then acc.reverse
          else if stopIndex < stops.length && stops(stopIndex).column == visualLine.startColumn + localIndex then
            expand(localIndex + 1, stopIndex + 1, stops(stopIndex).xPx :: acc)
          else expand(localIndex + 1, stopIndex, 0.0f :: acc)

        expand(0, 0, Nil).toVector
      val styledSegments0 =
        styledSegments.getOrElse {
          if syntaxHighlightingEnabled then com.serenity.ui.theme.ThemeManager.highlightLine(text, theme, language)
          else List(StyledText(text, TextStyle.normal, theme.foreground, theme.background))
        }

      case class MeasuredRun(
          startLocalIndex: Int,
          foreground: Color,
          background: Color,
          style: TextStyle,
          text: StringBuilder,
          endLocalIndex: Int
      )

      def drawRun(run: MeasuredRun): Unit =
        val visualExtents = stopXPxByLocalIndex.slice(run.startLocalIndex, run.endLocalIndex + 1)
        val startXPx      = xOriginPx + visualExtents.minOption.getOrElse(0.0f)
        val endXPx        = xOriginPx + visualExtents.maxOption.getOrElse(0.0f)
        val clippedEndXPx = clipRightXPx.fold(endXPx)(_.min(endXPx))
        val widthPx       = clippedEndXPx - startXPx
        if widthPx > 0.0f then
          surface.setForegroundColor(run.foreground)
          surface.setBackgroundColor(run.background)
          withStyle(surface, run.style) {
            surface.drawRunPx(startXPx, yPx, widthPx, lineHeightPx, ascentPx, run.text.toString)
          }

      val chars = styledSegments0.flatMap(segment =>
        segment.content.map(char => (char, segment.foregroundColor, segment.backgroundColor, segment.style))
      )
      val hasAnimations = animations.animations.nonEmpty

      val (runs, currentRun, _) = chars.foldLeft((List.empty[MeasuredRun], Option.empty[MeasuredRun], 0)) {
        case ((completed, current, localIndex), (char, segmentForeground, segmentBackground, style)) =>
          val bufferColumn = visualLine.startColumn + localIndex
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
                StringBuilder(char.toString),
                localIndex + 1
              )
              (completed, Some(run), localIndex + 1)

            case Some(run) if foreground == run.foreground && background == run.background && style == run.style =>
              run.text.append(char)
              val updatedRun = run.copy(endLocalIndex = localIndex + 1)
              (completed, Some(updatedRun), localIndex + 1)

            case Some(run) =>
              val nextRun = MeasuredRun(
                localIndex,
                foreground,
                background,
                style,
                StringBuilder(char.toString),
                localIndex + 1
              )
              (run :: completed, Some(nextRun), localIndex + 1)
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
    case class PlainRunState(
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

    case class ColorRunState(
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
