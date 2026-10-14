package com.serenity.ui.tui

import java.awt.Color

/** A terminal cannot blend a cell with what is behind it, so a translucent colour is mixed with `tui.backdrop` up
  * front. Alpha 0 is left alone: it means the terminal's own background, which only SGR 49 can ask for.
  */
object BackdropBlend:

  def flatten(color: Color, backdrop: Color): Color =
    val alpha = color.getAlpha
    if alpha == 0 || alpha == 255 then color
    else
      Color(
        mix(color.getRed, backdrop.getRed, alpha),
        mix(color.getGreen, backdrop.getGreen, alpha),
        mix(color.getBlue, backdrop.getBlue, alpha)
      )

  private def mix(channel: Int, behind: Int, alpha: Int): Int =
    (channel * alpha + behind * (255 - alpha) + 127) / 255
