package com.serenity.ui.renderer

import com.googlecode.lanterna.graphics.TextGraphics
import com.serenity.ui.theme.{Theme, ThemeManager, ThemeRenderer}

object CharacterRenderer:

  /** Render a string with proper character handling for all printable characters.
   * This ensures that special characters like underscore render correctly
   * even in terminals that might have font or rendering issues.
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
    var currentX = x
    for char <- content do
      char match
        case '\t' =>
          // Expand tab to spaces to reach next tab stop
          val spacesToAdd = tabWidth - (currentX % tabWidth)
          val tabSpaces = " " * spacesToAdd
          graphics.putString(currentX, y, tabSpaces)
          currentX += spacesToAdd
        case '_' =>
          // Explicitly handle underscore to ensure visibility
          graphics.putString(currentX, y, "_")
          currentX += 1
        case c if c >= 32 && c <= 126 =>
          graphics.putString(currentX, y, c.toString)
          currentX += 1
        case _ =>
          // Skip non-printable characters except tab (handled above)

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
    else
      renderStringPlain(graphics, x, y, content)

  /** Render a single character with special handling if needed.
   * This can be extended to handle specific characters that might not 
   * render properly in certain terminals.
   */
  def renderChar(
    graphics: TextGraphics,
    x: Int,
    y: Int,
    char: Char
  ): Unit =
    // Handle special character cases - tabs should be handled in layout, not here
    val displayChar = char match
      case '_' => '_'  // Ensure underscore is preserved
      case '\t' => '\t' // Preserve tab character - layout should handle tab width
      case c if c.isControl && c != '\t' => ' ' // Replace control chars (except tab) with space
      case c => c
    
    graphics.putString(x, y, displayChar.toString)

  /** Check if a character should be rendered visibly */
  def isVisibleChar(char: Char): Boolean =
    char match
      case c if c >= 32 && c <= 126 => true  // Standard printable ASCII
      case '_' => true                       // Explicitly include underscore
      case '\t' => true                      // Tab (though converted to space)
      case _ => false