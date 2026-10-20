package com.serenity.ui.renderer

import com.serenity.richtext.ParagraphRole
import com.serenity.state.models.TextVisualLine
import com.serenity.ui.layout.{LayoutRect, TextLayoutSnapshot}
import com.serenity.ui.theme.Theme

/** The rule a terminal draws under a heading that asks for one: a line of box-drawing cells across the text area, in
  * the row the heading's space below gives it. A heading with no such row has no rule, and a pixel surface has none
  * either, since its settings never switch one on.
  */
private[renderer] object RendererHeadingRules:

  private val HeavyGlyph = "━"
  private val LightGlyph = "─"
  private val DashGlyph  = "┄"

  def paintCells(
    surface: RenderSurface,
    snapshot: TextLayoutSnapshot,
    visualLine: TextVisualLine,
    theme: Theme,
    rect: LayoutRect,
    screenRow: Int,
    dimmed: Boolean
  ): Unit =
    if visualLine.spaceAfterPx > 0 then
      headingLevel(snapshot, visualLine).filter(snapshot.headings.rule).foreach { level =>
        val ruleRow = screenRow + 1
        if ruleRow < rect.bottom && ruleRow < surface.viewportHeight && rect.right > rect.x then
          surface.setForegroundColor(
            if dimmed then theme.muted else snapshot.headings.colourFor(level, markdown = false).resolve(theme)
          )
          surface.setBackgroundColor(theme.background)
          surface.putString(rect.x, ruleRow, glyphFor(level) * (rect.right - rect.x))
      }

  private def headingLevel(snapshot: TextLayoutSnapshot, visualLine: TextVisualLine): Option[Int] =
    snapshot.richTextDocument.flatMap { document =>
      snapshot.roles.layoutRole(document, visualLine.bufferLine) match
        case ParagraphRole.Heading(level) => Some(level)
        case _                            => None
    }

  private def glyphFor(level: Int): String =
    level match
      case 1 => HeavyGlyph
      case 2 => LightGlyph
      case _ => DashGlyph
