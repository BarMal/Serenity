package com.serenity.ui.renderer

import com.serenity.markdown.MarkdownInlineView
import com.serenity.state.models.TextVisualLine
import com.serenity.ui.theme.Theme

/** The horizontal line a live preview draws in place of a thematic break or a table's delimiter row, whose characters
  * are hidden and take no width.
  */
private[renderer] object RendererMarkdownRules:

  def paint(
    surface: RenderSurface,
    view: MarkdownInlineView,
    visualLine: TextVisualLine,
    theme: Theme,
    leftPx: Float,
    rightPx: Float,
    lineTopPx: Int,
    lineHeightPx: Int
  ): Unit =
    if view.isActive && visualLine.startColumn == 0 && view.drawsRuleOn(visualLine.bufferLine, visualLine.text) then
      surface.pixels.fillPixelRect(leftPx.toInt, lineTopPx + lineHeightPx / 2, (rightPx - leftPx).toInt, 1, theme.muted)
