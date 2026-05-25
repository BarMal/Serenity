package com.serenity.ui.renderer

import com.googlecode.lanterna.TextColor
import com.googlecode.lanterna.graphics.TextGraphics
import com.serenity.ui.theme.Theme

object TextOverlayRenderer:

  def render(
    graphics: TextGraphics,
    overlay: TextOverlayView,
    theme: Theme
  ): Unit =
    val rect = overlay.rect

    graphics.setForegroundColor(theme.foregroundColor)
    graphics.setBackgroundColor(TextColor.ANSI.BLACK_BRIGHT)

    for y <- rect.y until rect.bottom do
      graphics.putString(rect.x, y, " " * rect.width)

    drawBorder(graphics, overlay)
    drawLines(graphics, overlay)

    graphics.setForegroundColor(theme.foregroundColor)
    graphics.setBackgroundColor(theme.backgroundColor)

  private def drawBorder(graphics: TextGraphics, overlay: TextOverlayView): Unit =
    val rect = overlay.rect

    if rect.width >= 2 && rect.height >= 2 then
      graphics.putString(rect.x, rect.y, "+" + "-" * (rect.width - 2) + "+")

      for y <- (rect.y + 1) until (rect.bottom - 1) do
        graphics.putString(rect.x, y, "|")
        graphics.putString(rect.right - 1, y, "|")

      graphics.putString(rect.x, rect.bottom - 1, "+" + "-" * (rect.width - 2) + "+")

  private def drawLines(graphics: TextGraphics, overlay: TextOverlayView): Unit =
    val rect        = overlay.rect
    val maxLineSize = math.max(0, rect.width - 2)
    val maxLines    = math.max(0, rect.height - 2)

    overlay.lines.take(maxLines).zipWithIndex.foreach { case (line, index) =>
      val padded = line.take(maxLineSize).padTo(maxLineSize, ' ')
      CharacterRenderer.renderStringPlain(graphics, rect.x + 1, rect.y + 1 + index, padded)
    }
