package com.serenity.ui.renderer

import java.awt.Color

import com.serenity.animation.AnimationState
import com.serenity.lsp.config.LanguageId
import com.serenity.ui.theme.{StyledText, TextStyle, Theme}

object CharacterRenderer:

  private case class TextRun(startX: Int, content: String)
  private case class CollectedRuns(runs: List[TextRun], endX: Int)

  private case class MeasuredGlyph(
      char: Char,
      localIndex: Int,
      foreground: Color,
      background: Color,
      style: TextStyle
  )

  private case class MeasuredRun(
      startXPx: Float,
      startLocalIndex: Int,
      foreground: Color,
      background: Color,
      style: TextStyle,
      content: String
  )

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
    char match
      case c if c >= 32 && c <= 126 => true
      case '_'                      => true
      case '\t'                     => true
      case _                        => false

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
    bufferStartColumn: Int = 0
  ): Unit =
    if syntaxHighlightingEnabled then
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
    else
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
    language: Option[LanguageId] = None
  ): Unit =
    val text = visualLine.text
    if text.nonEmpty then
      val stops = visualLine.caretStops
      val styledSegments =
        if syntaxHighlightingEnabled then com.serenity.ui.theme.ThemeManager.highlightLine(text, theme, language)
        else List(StyledText(text, TextStyle.normal, theme.foreground, theme.background))

      def stopXPx(localIndex: Int): Float =
        stops.find(_.column == visualLine.startColumn + localIndex).map(_.xPx).getOrElse(0.0f)

      val glyphs =
        styledSegments
          .foldLeft((Vector.empty[MeasuredGlyph], 0)) {
            case ((acc, localIndex), segment) =>
              val segmentGlyphs = segment.content.zipWithIndex.map {
                case (char, offset) =>
                  val glyphIndex = localIndex + offset
                  val bufCol     = visualLine.startColumn + glyphIndex
                  val cell       = animations.getCell(bufCol, visualLine.bufferLine)
                  MeasuredGlyph(
                    char,
                    glyphIndex,
                    cell.flatMap(_.currentForeground).getOrElse(segment.foregroundColor),
                    cell.flatMap(_.currentBackground).getOrElse(segment.backgroundColor),
                    segment.style
                  )
              }
              (acc ++ segmentGlyphs, localIndex + segment.content.length)
          }
          ._1

      val runs = glyphs.foldLeft(Vector.empty[MeasuredRun]) { (acc, glyph) =>
        acc.lastOption match
          case Some(last)
              if last.foreground == glyph.foreground &&
                last.background == glyph.background &&
                last.style == glyph.style =>
            acc.updated(acc.length - 1, last.copy(content = last.content + glyph.char))
          case _ =>
            acc :+ MeasuredRun(
              xOriginPx + stopXPx(glyph.localIndex),
              glyph.localIndex,
              glyph.foreground,
              glyph.background,
              glyph.style,
              glyph.char.toString
            )
      }

      runs.foreach { run =>
        val endLocalIndex = run.startLocalIndex + run.content.length
        val endXPx        = xOriginPx + stopXPx(endLocalIndex)
        val widthPx       = endXPx - run.startXPx
        surface.setForegroundColor(run.foreground)
        surface.setBackgroundColor(run.background)
        withStyle(surface, run.style) {
          surface.drawRunPx(run.startXPx, yPx, widthPx, lineHeightPx, ascentPx, run.content)
        }
      }

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
    case class CollectState(runs: Vector[TextRun], currentText: String, currentStartX: Int, currentX: Int)

    def flushCurrent(state: CollectState): CollectState =
      if state.currentText.nonEmpty then
        state.copy(
          runs = state.runs :+ TextRun(state.currentStartX, state.currentText),
          currentText = ""
        )
      else state

    val finalState = content.foldLeft(CollectState(Vector.empty, "", startX, startX)) { (state, char) =>
      char match
        case '\t' =>
          val flushed     = flushCurrent(state)
          val spacesToAdd = tabWidth - (flushed.currentX % tabWidth)
          flushed.copy(
            runs = flushed.runs :+ TextRun(flushed.currentX, " " * spacesToAdd),
            currentStartX = flushed.currentX + spacesToAdd,
            currentX = flushed.currentX + spacesToAdd
          )
        case c if isVisibleChar(c) =>
          val start = if state.currentText.isEmpty then state.currentX else state.currentStartX
          state.copy(
            currentText = state.currentText + c,
            currentStartX = start,
            currentX = state.currentX + 1
          )
        case _ =>
          val flushed = flushCurrent(state)
          flushed.copy(currentStartX = flushed.currentX)
    }

    val flushed = flushCurrent(finalState)
    CollectedRuns(flushed.runs.toList, flushed.currentX)

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
    case class ColorGroup(startX: Int, text: String, foreground: Color, background: Color)

    val groups = run.content.zipWithIndex.foldLeft(Vector.empty[ColorGroup]) {
      case (acc, (char, index)) =>
        val bufferColumn = bufferStartColumn + (run.startX - screenOriginX) + index
        val cell         = screenAnimations.getCell(bufferColumn, bufferLine)
        val foreground   = cell.flatMap(_.currentForeground).getOrElse(theme.foreground)
        val background   = cell.flatMap(_.currentBackground).getOrElse(theme.background)

        acc.lastOption match
          case Some(last) if last.foreground == foreground && last.background == background =>
            acc.updated(acc.length - 1, last.copy(text = last.text + char))
          case _ =>
            acc :+ ColorGroup(run.startX + index, char.toString, foreground, background)
    }

    groups.map(group => (group.startX, group.text, group.foreground, group.background)).toList

  private def renderCharAtPosition(
    surface: RenderSurface,
    x: Int,
    y: Int,
    char: Char,
    theme: Theme,
    screenAnimations: AnimationState,
    bufferLine: Int,
    bufferColumn: Int
  ): Unit =
    val cell = screenAnimations.getCell(bufferColumn, bufferLine)
    cell.flatMap(_.currentForeground) match
      case Some(fg) => surface.setForegroundColor(fg)
      case None     => surface.setForegroundColor(theme.foreground)
    cell.flatMap(_.currentBackground) match
      case Some(bg) => surface.setBackgroundColor(bg)
      case None     => surface.setBackgroundColor(theme.background)
    renderChar(surface, x, y, char)

  private def blendColors(foreground: Color, background: Color, opacity: Double): Color =
    val t = opacity.max(0.0).min(1.0)
    new Color(
      math.round(background.getRed + (foreground.getRed - background.getRed) * t).toInt,
      math.round(background.getGreen + (foreground.getGreen - background.getGreen) * t).toInt,
      math.round(background.getBlue + (foreground.getBlue - background.getBlue) * t).toInt
    )
