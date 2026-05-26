package com.serenity.ui.renderer

import com.googlecode.lanterna.TextColor
import com.googlecode.lanterna.graphics.TextGraphics
import com.serenity.ui.theme.Theme

object PinnedPanelRenderer:

  def render(
    graphics: TextGraphics,
    panel: TextPanelView,
    theme: Theme
  ): Unit =
    val rect = panel.rect

    graphics.setForegroundColor(theme.foregroundColor)
    graphics.setBackgroundColor(TextColor.ANSI.BLACK_BRIGHT)

    for y <- rect.y until rect.bottom do
      graphics.putString(rect.x, y, " " * rect.width)

    drawBorder(graphics, panel)
    drawTitle(graphics, panel)
    drawLines(graphics, panel)

    graphics.setForegroundColor(theme.foregroundColor)
    graphics.setBackgroundColor(theme.backgroundColor)

  private def drawBorder(graphics: TextGraphics, panel: TextPanelView): Unit =
    val rect = panel.rect

    if rect.width >= 2 && rect.height >= 2 then
      graphics.putString(rect.x, rect.y, "+" + "-" * (rect.width - 2) + "+")

      for y <- (rect.y + 1) until (rect.bottom - 1) do
        graphics.putString(rect.x, y, "|")
        graphics.putString(rect.right - 1, y, "|")

      graphics.putString(rect.x, rect.bottom - 1, "+" + "-" * (rect.width - 2) + "+")

  private def drawTitle(graphics: TextGraphics, panel: TextPanelView): Unit =
    val rect = panel.rect
    val title = panel.title.take(math.max(0, rect.width - 2)).padTo(math.max(0, rect.width - 2), ' ')
    if rect.width >= 2 then
      CharacterRenderer.renderStringPlain(graphics, rect.x + 1, rect.y, title)

  private def drawLines(graphics: TextGraphics, panel: TextPanelView): Unit =
    val rect        = panel.rect
    val maxLineSize = math.max(0, rect.width - 2)
    val maxLines    = math.max(0, rect.height - 2)

    panel.lines.take(maxLines).zipWithIndex.foreach { case (line, index) =>
      val padded = line.take(maxLineSize).padTo(maxLineSize, ' ')
      CharacterRenderer.renderStringPlain(graphics, rect.x + 1, rect.y + 1 + index, padded)
    }
