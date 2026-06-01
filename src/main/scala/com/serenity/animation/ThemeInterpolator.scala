package com.serenity.animation

import java.awt.Color

import com.serenity.ui.theme.{SyntaxElement, Theme, ThemeColor}

object ThemeInterpolator:

  def blend(from: Theme, to: Theme, t: Double): Theme =
    Theme(
      name = to.name,
      foreground = blendColor(from.foreground, to.foreground, t),
      background = blendColor(from.background, to.background, t),
      cursor = blendColor(from.cursor, to.cursor, t),
      highlighted = blendThemeColor(from.highlighted, to.highlighted, t),
      menuItem = blendThemeColor(from.menuItem, to.menuItem, t),
      panel = blendThemeColor(from.panel, to.panel, t),
      error = blendThemeColor(from.error, to.error, t),
      warning = blendThemeColor(from.warning, to.warning, t),
      border = blendColor(from.border, to.border, t),
      muted = blendColor(from.muted, to.muted, t),
      placeholder = blendColor(from.placeholder, to.placeholder, t),
      textStyle = to.textStyle,
      syntaxColors = blendSyntaxColors(from.syntaxColors, to.syntaxColors, t)
    )

  private def blendColor(from: Color, to: Color, t: Double): Color =
    new Color(
      blendChannel(from.getRed, to.getRed, t),
      blendChannel(from.getGreen, to.getGreen, t),
      blendChannel(from.getBlue, to.getBlue, t)
    )

  private def blendThemeColor(from: ThemeColor, to: ThemeColor, t: Double): ThemeColor =
    ThemeColor(
      foreground = blendColor(from.foreground, to.foreground, t),
      background = blendColor(from.background, to.background, t),
      style = to.style,
      alpha = from.alpha + (to.alpha - from.alpha) * t
    )

  private def blendSyntaxColors(
    from: Map[SyntaxElement, ThemeColor],
    to: Map[SyntaxElement, ThemeColor],
    t: Double
  ): Map[SyntaxElement, ThemeColor] =
    val fallback = ThemeColor(Color.WHITE, Color.BLACK)
    (from.keySet ++ to.keySet).map { el =>
      val f  = from.getOrElse(el, to.getOrElse(el, fallback))
      val tt = to.getOrElse(el, from.getOrElse(el, fallback))
      el -> blendThemeColor(f, tt, t)
    }.toMap

  private def blendChannel(from: Int, to: Int, t: Double): Int =
    math.round(from + (to - from) * t).toInt.max(0).min(255)
