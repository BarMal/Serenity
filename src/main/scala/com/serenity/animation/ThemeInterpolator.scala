package com.serenity.animation

import com.googlecode.lanterna.TextColor
import com.serenity.ui.theme.{SyntaxElement, Theme, ThemeColor}

object ThemeInterpolator:

  def blend(from: Theme, to: Theme, t: Double): Theme =
    Theme(
      name         = to.name,
      foreground   = blendColor(from.foreground, to.foreground, t),
      background   = blendColor(from.background, to.background, t),
      cursor       = blendColor(from.cursor, to.cursor, t),
      highlighted  = blendThemeColor(from.highlighted, to.highlighted, t),
      menuItem     = blendThemeColor(from.menuItem, to.menuItem, t),
      panel        = blendThemeColor(from.panel, to.panel, t),
      error        = blendThemeColor(from.error, to.error, t),
      border       = blendColor(from.border, to.border, t),
      muted        = blendColor(from.muted, to.muted, t),
      placeholder  = blendColor(from.placeholder, to.placeholder, t),
      textStyle    = to.textStyle,
      syntaxColors = blendSyntaxColors(from.syntaxColors, to.syntaxColors, t)
    )

  private def blendColor(from: TextColor, to: TextColor, t: Double): TextColor =
    val f = RgbInterpolator.toRgb(from)
    val tt = RgbInterpolator.toRgb(to)
    new TextColor.RGB(
      blendChannel(f.getRed,   tt.getRed,   t),
      blendChannel(f.getGreen, tt.getGreen, t),
      blendChannel(f.getBlue,  tt.getBlue,  t)
    )

  private def blendThemeColor(from: ThemeColor, to: ThemeColor, t: Double): ThemeColor =
    ThemeColor(
      foreground = blendColor(from.foreground, to.foreground, t),
      background = blendColor(from.background, to.background, t),
      style      = to.style,
      alpha      = from.alpha + (to.alpha - from.alpha) * t
    )

  private def blendSyntaxColors(
    from: Map[SyntaxElement, ThemeColor],
    to:   Map[SyntaxElement, ThemeColor],
    t:    Double
  ): Map[SyntaxElement, ThemeColor] =
    (from.keySet ++ to.keySet).map { el =>
      val f = from.getOrElse(el, to.getOrElse(el, ThemeColor(new TextColor.RGB(255, 255, 255), new TextColor.RGB(0, 0, 0))))
      val tt = to.getOrElse(el, from.getOrElse(el, ThemeColor(new TextColor.RGB(255, 255, 255), new TextColor.RGB(0, 0, 0))))
      el -> blendThemeColor(f, tt, t)
    }.toMap

  private def blendChannel(from: Int, to: Int, t: Double): Int =
    math.round(from + (to - from) * t).toInt.max(0).min(255)
