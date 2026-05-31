package com.serenity.ui.renderer

import java.awt.Color
import com.serenity.animation.AnimationState
import com.serenity.ui.theme.Theme

object CharacterRenderer:

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
    content.foldLeft(x) { (currentX, char) =>
      char match
        case '\t' =>
          val spacesToAdd = tabWidth - (currentX % tabWidth)
          val tabSpaces   = " " * spacesToAdd
          surface.putString(currentX, y, tabSpaces)
          currentX + spacesToAdd
        case '_' =>
          surface.putString(currentX, y, "_")
          currentX + 1
        case c if c >= 32 && c <= 126 =>
          surface.putString(currentX, y, c.toString)
          currentX + 1
        case _ =>
          currentX
    }

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
    else if opacity <= 0.0 then
      ()
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
    bufferStartColumn: Int = 0
  ): Unit =
    if syntaxHighlightingEnabled then
      val styledTexts = com.serenity.ui.theme.ThemeManager.highlightLine(content, theme)
      renderStyledLineWithAnimation(surface, x, y, styledTexts, theme, screenAnimations, bufferLine, bufferStartColumn)
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
    content.foldLeft(x) { (currentX, char) =>
      char match
        case '\t' =>
          val spacesToAdd = tabWidth - (currentX % tabWidth)
          (0 until spacesToAdd).foldLeft(currentX) { (posX, _) =>
            val bufferColumn = bufferStartColumn + (posX - x)
            renderCharAtPosition(surface, posX, y, ' ', theme, screenAnimations, bufferLine, bufferColumn)
            posX + 1
          }
        case c if c >= 32 && c <= 126 =>
          val bufferColumn = bufferStartColumn + (currentX - x)
          renderCharAtPosition(surface, currentX, y, c, theme, screenAnimations, bufferLine, bufferColumn)
          currentX + 1
        case _ =>
          currentX
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

      styledText.content.foldLeft(currentX) { (posX, char) =>
        val bufferColumn = bufferStartColumn + (posX - x)
        renderCharAtPosition(surface, posX, y, char, segmentTheme, screenAnimations, bufferLine, bufferColumn)
        posX + 1
      }
    }

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
      math.round(background.getRed   + (foreground.getRed   - background.getRed)   * t).toInt,
      math.round(background.getGreen + (foreground.getGreen - background.getGreen) * t).toInt,
      math.round(background.getBlue  + (foreground.getBlue  - background.getBlue)  * t).toInt
    )
