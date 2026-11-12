package com.serenity.ui.renderer

import java.awt.Color

/** How a shadowed run's shadow looks, for every backend: the run's own glyphs, one logical pixel (never less than a
  * device pixel) down and to the right, in its foreground at 40% of the opacity it has.
  */
private[serenity] object TextShadow:

  private val Opacity = 0.4

  def colorOf(foreground: Color): Color =
    Color(foreground.getRed, foreground.getGreen, foreground.getBlue, math.round(foreground.getAlpha * Opacity).toInt)

  /** In logical pixels, for a surface whose device pixels are `1 / deviceScale` of one. */
  def offsetOf(deviceScale: Double): Double = math.max(1.0, 1.0 / deviceScale)
