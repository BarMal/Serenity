package com.serenity.ui.renderer

import java.awt.Color

import com.serenity.animation.AnimationState
import com.serenity.ui.theme.Theme

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
    bufferLine: Int = 0,
    bufferStartColumn: Int = 0,
    preserveContinuousRuns: Boolean = false
  ): Unit =
    if syntaxHighlightingEnabled then
      val styledTexts = com.serenity.ui.theme.ThemeManager.highlightLine(content, theme)
      renderStyledLineWithAnimation(
        surface,
        x,
        y,
        styledTexts,
        theme,
        screenAnimations,
        bufferLine,
        bufferStartColumn,
        preserveContinuousRuns
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
        bufferStartColumn = bufferStartColumn,
        preserveContinuousRuns = preserveContinuousRuns
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
    preserveContinuousRuns: Boolean = false
  ): Unit =
    val collectedRuns = collectPlainRuns(x, content, tabWidth)
    renderAnimatedRuns(
      surface,
      x,
      y,
      collectedRuns.runs,
      theme,
      screenAnimations,
      bufferLine,
      bufferStartColumn,
      preserveContinuousRuns
    )

  private def renderStyledLineWithAnimation(
    surface: RenderSurface,
    x: Int,
    y: Int,
    styledTexts: List[com.serenity.ui.theme.StyledText],
    theme: Theme,
    screenAnimations: AnimationState,
    bufferLine: Int = 0,
    bufferStartColumn: Int = 0,
    preserveContinuousRuns: Boolean = false
  ): Unit =
    styledTexts.foldLeft(x) { (currentX, styledText) =>
      val segmentTheme = theme.copy(
        foreground = styledText.foregroundColor,
        background = styledText.backgroundColor
      )
      val collectedRuns = collectPlainRuns(currentX, styledText.content, tabWidth = 4)
      renderAnimatedRuns(
        surface,
        currentX,
        y,
        collectedRuns.runs,
        segmentTheme,
        screenAnimations,
        bufferLine,
        bufferStartColumn + (currentX - x),
        preserveContinuousRuns
      )
      collectedRuns.endX
    }

  private def collectPlainRuns(
    startX: Int,
    content: String,
    tabWidth: Int
  ): CollectedRuns =
    val runs          = scala.collection.mutable.ListBuffer.empty[TextRun]
    val currentText   = StringBuilder()
    var currentStartX = startX
    var currentX      = startX

    def flushCurrent(): Unit =
      if currentText.nonEmpty then
        runs += TextRun(currentStartX, currentText.result())
        currentText.clear()

    content.foreach { char =>
      char match
        case '\t' =>
          flushCurrent()
          val spacesToAdd = tabWidth - (currentX % tabWidth)
          val tabSpaces   = " " * spacesToAdd
          runs += TextRun(currentX, tabSpaces)
          currentX += spacesToAdd
          currentStartX = currentX
        case c if isVisibleChar(c) =>
          if currentText.isEmpty then currentStartX = currentX
          currentText.append(c)
          currentX += 1
        case _ =>
          flushCurrent()
          currentStartX = currentX
    }

    flushCurrent()
    CollectedRuns(runs.toList, currentX)

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
    bufferStartColumn: Int,
    preserveContinuousRuns: Boolean
  ): Unit =
    runs.foreach { run =>
      if preserveContinuousRuns then
        surface.setForegroundColor(theme.foreground)
        surface.setBackgroundColor(theme.background)
        surface.putString(run.startX, y, run.content)
      else
        val grouped =
          groupRunByEffectiveColors(run, screenOriginX, theme, screenAnimations, bufferLine, bufferStartColumn)
        grouped.foreach { case (startX, text, foreground, background) =>
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
    val groups = scala.collection.mutable.ListBuffer.empty[(Int, String, Color, Color)]
    val text   = run.content

    if text.nonEmpty then
      var currentText       = StringBuilder()
      var currentStartX     = run.startX
      var currentForeground = theme.foreground
      var currentBackground = theme.background

      def flushCurrent(): Unit =
        if currentText.nonEmpty then
          groups += ((currentStartX, currentText.result(), currentForeground, currentBackground))
          currentText.clear()

      text.zipWithIndex.foreach { case (char, index) =>
        val bufferColumn = bufferStartColumn + (run.startX - screenOriginX) + index
        val cell         = screenAnimations.getCell(bufferColumn, bufferLine)
        val foreground   = cell.flatMap(_.currentForeground).getOrElse(theme.foreground)
        val background   = cell.flatMap(_.currentBackground).getOrElse(theme.background)

        if currentText.isEmpty then
          currentStartX = run.startX + index
          currentForeground = foreground
          currentBackground = background
          currentText.append(char)
        else if foreground == currentForeground && background == currentBackground then
          currentText.append(char)
        else
          flushCurrent()
          currentStartX = run.startX + index
          currentForeground = foreground
          currentBackground = background
          currentText.append(char)
      }

      flushCurrent()

    groups.toList

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
