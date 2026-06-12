package com.serenity.ui.renderer

import java.awt.Color

import com.serenity.config.{AppConfig, BackgroundStyle, MaterialPreset}
import com.serenity.ui.theme.Theme

object SurfaceMaterials:

  def panelAlpha(config: AppConfig, theme: Theme): Float =
    config.materialPreset match
      case MaterialPreset.Solid   => 1.0f
      case MaterialPreset.Clear   => 0.28f
      case MaterialPreset.Frosted => theme.panel.alpha.toFloat
      case MaterialPreset.Crystal => 0.74f
      case MaterialPreset.Custom  => alphaForBackground(config.backgroundStyle, theme)

  def effectiveBlurRadius(config: AppConfig): Float =
    config.materialPreset match
      case MaterialPreset.Solid | MaterialPreset.Clear => 0.0f
      case MaterialPreset.Frosted                      => config.blurRadius
      case MaterialPreset.Crystal                      => math.max(config.blurRadius, 0.65f)
      case MaterialPreset.Custom                       => blurForBackground(config.backgroundStyle, config.blurRadius)

  def glassSheenBackground(config: AppConfig, theme: Theme): Option[Color] =
    Option.when(config.materialPreset == MaterialPreset.Crystal || isCustomGlass(config)) {
      blend(theme.panel.background, theme.panel.foreground, 0.18)
    }

  private def alphaForBackground(style: BackgroundStyle, theme: Theme): Float =
    style match
      case BackgroundStyle.Solid       => 1.0f
      case BackgroundStyle.Transparent => 0.28f
      case BackgroundStyle.Frosted     => theme.panel.alpha.toFloat
      case BackgroundStyle.GlassLike   => math.min(theme.panel.alpha.toFloat, 0.78f)

  private def blurForBackground(style: BackgroundStyle, blurRadius: Float): Float =
    style match
      case BackgroundStyle.Solid       => 0.0f
      case BackgroundStyle.Transparent => 0.0f
      case BackgroundStyle.Frosted     => blurRadius
      case BackgroundStyle.GlassLike   => math.max(blurRadius, 0.6f)

  private def isCustomGlass(config: AppConfig): Boolean =
    config.materialPreset == MaterialPreset.Custom && config.backgroundStyle == BackgroundStyle.GlassLike

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
