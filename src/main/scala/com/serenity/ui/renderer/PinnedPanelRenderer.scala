package com.serenity.ui.renderer

import com.serenity.ui.theme.Theme

object PinnedPanelRenderer:

  def render(
    surface: RenderSurface,
    panel: TextPanelView,
    theme: Theme
  ): Unit =
    val rect = panel.rect

    surface.setAlpha(theme.panel.alpha.toFloat)
    surface.setForegroundColor(theme.panel.foreground)
    surface.setBackgroundColor(theme.panel.background)

    for y <- rect.y until rect.bottom do
      surface.putString(rect.x, y, " " * rect.width)

    drawBorder(surface, panel, theme)
    drawTitle(surface, panel)
    drawLines(surface, panel)

    surface.setAlpha(1.0f)
    surface.setForegroundColor(theme.foreground)
    surface.setBackgroundColor(theme.background)

  private def drawBorder(surface: RenderSurface, panel: TextPanelView, theme: Theme): Unit =
    val rect = panel.rect

    if rect.width >= 2 && rect.height >= 2 then
      surface.setForegroundColor(theme.border)
      surface.setBackgroundColor(theme.panel.background)
      surface.putString(rect.x, rect.y, "+" + "-" * (rect.width - 2) + "+")

      for y <- (rect.y + 1) until (rect.bottom - 1) do
        surface.putString(rect.x, y, "|")
        surface.putString(rect.right - 1, y, "|")

      surface.putString(rect.x, rect.bottom - 1, "+" + "-" * (rect.width - 2) + "+")

  private def drawTitle(surface: RenderSurface, panel: TextPanelView): Unit =
    val rect  = panel.rect
    val title = panel.title.take(math.max(0, rect.width - 2)).padTo(math.max(0, rect.width - 2), ' ')
    if rect.width >= 2 then
      CharacterRenderer.renderStringPlain(surface, rect.x + 1, rect.y, title)

  private def drawLines(surface: RenderSurface, panel: TextPanelView): Unit =
    val rect        = panel.rect
    val maxLineSize = math.max(0, rect.width - 2)
    val maxLines    = math.max(0, rect.height - 2)

    panel.lines.take(maxLines).zipWithIndex.foreach { case (line, index) =>
      val padded = line.take(maxLineSize).padTo(maxLineSize, ' ')
      CharacterRenderer.renderStringPlain(surface, rect.x + 1, rect.y + 1 + index, padded)
    }
