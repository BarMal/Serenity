package com.serenity.ui.renderer

import java.awt.Color

import com.serenity.config.{AppConfig, BackgroundStyle, MaterialPreset, VisualFlairLevel}
import com.serenity.ui.theme.ColorFormat.withAlpha
import com.serenity.ui.theme.Theme

object SurfaceMaterials:

  def panelAlpha(config: AppConfig, theme: Theme): Float =
    config.surfaceConfig.materialPreset match
      case MaterialPreset.Solid   => 1.0f
      case MaterialPreset.Clear   => 0.28f
      case MaterialPreset.Frosted => theme.panel.alpha.value.toFloat
      case MaterialPreset.Crystal => 0.78f
      case MaterialPreset.Custom  => alphaForBackground(config.surfaceConfig.backgroundStyle, theme)

  /** Background blur is real GPU/CPU work per frame -- a cost [[VisualFlairLevel]] exists to let a viewer on a slow
    * link or a battery-powered machine turn down independently of the material preset that would otherwise call for it:
    * `Reduced` halves whatever radius the preset picked, `Off` drops it to zero outright.
    */
  def effectiveBlurRadius(config: AppConfig): Float =
    val presetRadius = config.surfaceConfig.materialPreset match
      case MaterialPreset.Solid | MaterialPreset.Clear => 0.0f
      case MaterialPreset.Frosted                      => config.surfaceConfig.blurRadius
      case MaterialPreset.Crystal                      => math.max(config.surfaceConfig.blurRadius, 0.42f)
      case MaterialPreset.Custom =>
        blurForBackground(config.surfaceConfig.backgroundStyle, config.surfaceConfig.blurRadius)
    config.visualFlairLevel match
      case VisualFlairLevel.Full    => presetRadius
      case VisualFlairLevel.Reduced => presetRadius / 2.0f
      case VisualFlairLevel.Off     => 0.0f

  /** The theme's alpha-0 backdrop, when a panel on `surface` should show it rather than its own colour. A surface with
    * no [[Effects]] (the terminal's cell grid) cannot paint translucently, so a translucent material over a theme
    * background that is the terminal's own would otherwise paint as a solid block; the nearest it can express is that
    * backdrop itself. A `Solid` material, or an opaque theme background, keeps the panel colour.
    */
  def backdropShowingThrough(config: AppConfig, theme: Theme, surface: RenderSurface): Option[Color] =
    Option.when(surface.effects.isEmpty && theme.background.getAlpha == 0 && panelAlpha(config, theme) < 1.0f)(
      theme.background
    )

  /** `color` faded to the alpha of the background it sits on, so a panel's highlights and tones fade with the panel.
    * The theme's alpha-0 backdrop is not a fade but the terminal showing through (see [[backdropShowingThrough]]), so a
    * colour over it keeps its own alpha.
    */
  def fadedWith(color: Color, base: Color, theme: Theme): Color =
    if base.getAlpha == 0 && base == theme.background then color else color.withAlpha(base.getAlpha)

  def glassSheenBackground(config: AppConfig, theme: Theme): Option[Color] =
    Option.when(config.surfaceConfig.materialPreset == MaterialPreset.Crystal || isCustomGlass(config)) {
      blend(theme.panel.background, theme.panel.foreground, 0.10)
    }

  private def alphaForBackground(style: BackgroundStyle, theme: Theme): Float =
    style match
      case BackgroundStyle.Solid       => 1.0f
      case BackgroundStyle.Transparent => 0.28f
      case BackgroundStyle.Frosted     => theme.panel.alpha.value.toFloat
      case BackgroundStyle.GlassLike   => math.min(theme.panel.alpha.value.toFloat, 0.82f)

  private def blurForBackground(style: BackgroundStyle, blurRadius: Float): Float =
    style match
      case BackgroundStyle.Solid       => 0.0f
      case BackgroundStyle.Transparent => 0.0f
      case BackgroundStyle.Frosted     => blurRadius
      case BackgroundStyle.GlassLike   => math.max(blurRadius, 0.42f)

  private def isCustomGlass(config: AppConfig): Boolean =
    config.surfaceConfig.materialPreset == MaterialPreset.Custom && config.surfaceConfig.backgroundStyle == BackgroundStyle.GlassLike

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
