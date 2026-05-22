package com.serenity.ui.renderer

import com.googlecode.lanterna.TextColor
import com.googlecode.lanterna.graphics.TextGraphics
import com.serenity.animation.AnimationState
import com.serenity.ui.theme.{Theme, ThemeManager, ThemeRenderer}

object CharacterRenderer:

  /** Render a string with proper character handling for all printable characters. This ensures that special characters
    * like underscore render correctly even in terminals that might have font or rendering issues.
    */
  def renderString(
    graphics: TextGraphics,
    x: Int,
    y: Int,
    content: String
  ): Unit =
    renderStringPlain(graphics, x, y, content)

  /** Render string with proper tab expansion and character handling */
  def renderStringPlain(
    graphics: TextGraphics,
    x: Int,
    y: Int,
    content: String,
    tabWidth: Int = 4
  ): Unit =
    content.foldLeft(x) { (currentX, char) =>
      char match
        case '\t' =>
          // Expand tab to spaces to reach next tab stop
          val spacesToAdd = tabWidth - (currentX % tabWidth)
          val tabSpaces   = " " * spacesToAdd
          graphics.putString(currentX, y, tabSpaces)
          currentX + spacesToAdd
        case '_' =>
          // Explicitly handle underscore to ensure visibility
          graphics.putString(currentX, y, "_")
          currentX + 1
        case c if c >= 32 && c <= 126 =>
          graphics.putString(currentX, y, c.toString)
          currentX + 1
        case _ =>
          // Skip non-printable characters except tab (handled above)
          currentX
    }

  /** Render a string with theme-based syntax highlighting (if enabled) */
  def renderStringWithTheme(
    graphics: TextGraphics,
    x: Int,
    y: Int,
    content: String,
    theme: Theme,
    syntaxHighlightingEnabled: Boolean = true
  ): Unit =
    if syntaxHighlightingEnabled then
      val styledSegments = ThemeManager.highlightLine(content, theme)
      ThemeRenderer.renderStyledLine(graphics, x, y, styledSegments)
    else renderStringPlain(graphics, x, y, content)

  /** Render a single character with special handling if needed. This can be extended to handle specific characters that
    * might not render properly in certain terminals.
    */
  def renderChar(
    graphics: TextGraphics,
    x: Int,
    y: Int,
    char: Char
  ): Unit =
    // Handle special character cases - tabs should be handled in layout, not here
    val displayChar = char match
      case '_'                           => '_'  // Ensure underscore is preserved
      case '\t'                          => '\t' // Preserve tab character - layout should handle tab width
      case c if c.isControl && c != '\t' => ' '  // Replace control chars (except tab) with space
      case c                             => c

    graphics.putString(x, y, displayChar.toString)

  /** Check if a character should be rendered visibly */
  def isVisibleChar(char: Char): Boolean =
    char match
      case c if c >= 32 && c <= 126 => true // Standard printable ASCII
      case '_'                      => true // Explicitly include underscore
      case '\t'                     => true // Tab (though converted to space)
      case _                        => false

  /** Render a character with opacity support (simulated through color blending) */
  def renderCharWithOpacity(
    graphics: TextGraphics,
    x: Int,
    y: Int,
    char: Char,
    foregroundColor: TextColor,
    backgroundColor: TextColor,
    opacity: Double
  ): Unit =
    if opacity >= 1.0 then
      // Full opacity - render normally
      graphics.setForegroundColor(foregroundColor)
      graphics.setBackgroundColor(backgroundColor)
      renderChar(graphics, x, y, char)
    else if opacity <= 0.0 then
      // Fully transparent - don't render
      ()
    else
      // Simulate opacity by blending foreground with background
      val blendedForeground = blendColors(foregroundColor, backgroundColor, opacity)
      graphics.setForegroundColor(blendedForeground)
      graphics.setBackgroundColor(backgroundColor)
      renderChar(graphics, x, y, char)

  /** Render a string with animation support.
    * bufferLine and bufferStartColumn identify the buffer position of the first character,
    * so animations keyed by buffer coordinates are applied to the correct screen cell.
    */
  def renderStringWithAnimation(
    graphics: TextGraphics,
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
      val styledTexts = ThemeManager.highlightLine(content, theme)
      renderStyledLineWithAnimation(graphics, x, y, styledTexts, theme, screenAnimations, bufferLine, bufferStartColumn)
    else renderStringWithAnimationPlain(graphics, x, y, content, theme, screenAnimations, bufferLine = bufferLine, bufferStartColumn = bufferStartColumn)

  /** Render a string with animation support (plain, no syntax highlighting) */
  def renderStringWithAnimationPlain(
    graphics: TextGraphics,
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
            renderCharAtPosition(graphics, posX, y, ' ', theme, screenAnimations, bufferLine, bufferColumn)
            posX + 1
          }
        case c if c >= 32 && c <= 126 =>
          val bufferColumn = bufferStartColumn + (currentX - x)
          renderCharAtPosition(graphics, currentX, y, c, theme, screenAnimations, bufferLine, bufferColumn)
          currentX + 1
        case _ =>
          currentX
    }

  /** Render styled line with animation support */
  private def renderStyledLineWithAnimation(
    graphics: TextGraphics,
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
        foregroundColor = styledText.foregroundColor,
        backgroundColor = styledText.backgroundColor
      )

      styledText.content.foldLeft(currentX) { (posX, char) =>
        val bufferColumn = bufferStartColumn + (posX - x)
        renderCharAtPosition(graphics, posX, y, char, segmentTheme, screenAnimations, bufferLine, bufferColumn)
        posX + 1
      }
    }

  /** Render a single character at position, looking up animation by buffer coordinates */
  private def renderCharAtPosition(
    graphics: TextGraphics,
    x: Int,
    y: Int,
    char: Char,
    theme: Theme,
    screenAnimations: AnimationState,
    bufferLine: Int,
    bufferColumn: Int
  ): Unit =
    screenAnimations.getCharacterColor(bufferColumn, bufferLine) match
      case Some(animatedColor) =>
        graphics.setForegroundColor(animatedColor)
        graphics.setBackgroundColor(theme.backgroundColor)
        renderChar(graphics, x, y, char)
      case None =>
        graphics.setForegroundColor(theme.foregroundColor)
        graphics.setBackgroundColor(theme.backgroundColor)
        renderChar(graphics, x, y, char)

  /** Blend two colors with given opacity (alpha blending simulation) */
  private def blendColors(foreground: TextColor, background: TextColor, opacity: Double): TextColor =
    // For terminal colors, we'll approximate blending by choosing intermediate colors
    // This is a simplified approach since we can't do true RGB blending with ANSI colors
    if opacity > 0.7 then foreground
    else if opacity > 0.4 then
      // Try to find a dimmer version of the foreground color
      foreground match
        case TextColor.ANSI.WHITE        => TextColor.ANSI.WHITE_BRIGHT
        case TextColor.ANSI.WHITE_BRIGHT => TextColor.ANSI.BLACK_BRIGHT
        case TextColor.ANSI.RED          => TextColor.ANSI.RED_BRIGHT
        case TextColor.ANSI.GREEN        => TextColor.ANSI.GREEN_BRIGHT
        case TextColor.ANSI.BLUE         => TextColor.ANSI.BLUE_BRIGHT
        case TextColor.ANSI.YELLOW       => TextColor.ANSI.YELLOW_BRIGHT
        case TextColor.ANSI.MAGENTA      => TextColor.ANSI.MAGENTA_BRIGHT
        case TextColor.ANSI.CYAN         => TextColor.ANSI.CYAN_BRIGHT
        case _                           => TextColor.ANSI.BLACK_BRIGHT
    else TextColor.ANSI.BLACK_BRIGHT
