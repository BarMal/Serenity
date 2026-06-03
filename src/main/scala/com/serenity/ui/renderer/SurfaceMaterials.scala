package com.serenity.ui.renderer

import java.awt.Color

import com.serenity.config.{AppConfig, BackgroundStyle}
import com.serenity.ui.theme.Theme

object SurfaceMaterials:

  def panelAlpha(config: AppConfig, theme: Theme): Float =
    config.backgroundStyle match
      case BackgroundStyle.Solid       => 1.0f
      case BackgroundStyle.Transparent => 0.28f
      case BackgroundStyle.Frosted     => theme.panel.alpha.toFloat
      case BackgroundStyle.GlassLike   => math.min(theme.panel.alpha.toFloat, 0.78f)

  def effectiveBlurRadius(config: AppConfig): Float =
    config.backgroundStyle match
      case BackgroundStyle.Solid       => 0.0f
      case BackgroundStyle.Transparent => 0.0f
      case BackgroundStyle.Frosted     => config.blurRadius
      case BackgroundStyle.GlassLike   => math.max(config.blurRadius, 0.6f)

  def glassSheenBackground(config: AppConfig, theme: Theme): Option[Color] =
    Option.when(config.backgroundStyle == BackgroundStyle.GlassLike) {
      blend(theme.panel.background, theme.panel.foreground, 0.18)
    }

  private def blend(from: Color, to: Color, factor: Double): Color =
    val t = factor.max(0.0).min(1.0)
    def component(start: Int, end: Int): Int =
      math.round(start + (end - start) * t).toInt.max(0).min(255)

    new Color(
      component(from.getRed, to.getRed),
      component(from.getGreen, to.getGreen),
      component(from.getBlue, to.getBlue),
      from.getAlpha
    )
