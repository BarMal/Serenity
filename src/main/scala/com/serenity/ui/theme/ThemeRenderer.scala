package com.serenity.ui.theme

import com.googlecode.lanterna.SGR
import com.googlecode.lanterna.graphics.TextGraphics

object ThemeRenderer:

  /** Render styled text with proper Lanterna formatting */
  def renderStyledText(
    graphics: TextGraphics,
    x: Int,
    y: Int,
    styledText: StyledText
  ): Unit =
    // Set colors
    graphics.setForegroundColor(styledText.foregroundColor)
    graphics.setBackgroundColor(styledText.backgroundColor)

    // Apply text styles (SGR modifiers)
    val modifiers = buildSGRModifiers(styledText.style)
    if modifiers.nonEmpty then graphics.enableModifiers(modifiers.toSeq*)

    // Render the text
    graphics.putString(x, y, styledText.content)

    // Disable modifiers after rendering
    if modifiers.nonEmpty then graphics.disableModifiers(modifiers.toSeq*)

  /** Render a list of styled text segments on the same line */
  def renderStyledLine(
    graphics: TextGraphics,
    x: Int,
    y: Int,
    styledSegments: List[StyledText]
  ): Unit =
    styledSegments.foldLeft(x) { (currentX, styledText) =>
      renderStyledText(graphics, currentX, y, styledText)
      currentX + styledText.content.length
    }

  /** Convert TextStyle to Lanterna SGR modifiers */
  private def buildSGRModifiers(style: TextStyle): Set[SGR] =
    List(
      if style.isBold then Some(SGR.BOLD) else None,
      if style.isItalic then Some(SGR.ITALIC) else None,
      if style.isUnderlined then Some(SGR.UNDERLINE) else None
    ).flatten.toSet

  /** Render styled text preserving existing background */
  def renderStyledTextPreserveBackground(
    graphics: TextGraphics,
    x: Int,
    y: Int,
    styledText: StyledText,
    preserveBackground: Boolean = true
  ): Unit =
    val originalBackground = graphics.getBackgroundColor

    // Set colors
    graphics.setForegroundColor(styledText.foregroundColor)
    if !preserveBackground then graphics.setBackgroundColor(styledText.backgroundColor)

    // Apply text styles
    val modifiers = buildSGRModifiers(styledText.style)
    if modifiers.nonEmpty then graphics.enableModifiers(modifiers.toSeq*)

    // Render the text
    graphics.putString(x, y, styledText.content)

    // Restore and cleanup
    if modifiers.nonEmpty then graphics.disableModifiers(modifiers.toSeq*)

    if !preserveBackground then graphics.setBackgroundColor(originalBackground)
