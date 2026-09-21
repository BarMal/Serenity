package com.serenity.ui.theme

import java.awt.Color

/** Semantic depth tiers a surface can name, strictly ordered from resting to topmost (issue #1090). */
enum ElevationLevel:
  case Base, Raised, Floating, Modal

object ElevationLevel:
  given Ordering[ElevationLevel] = Ordering.by(_.ordinal)

/** What a renderer needs to paint depth for one [[ElevationLevel]]: how strong its shadow reads, and, where the theme
  * calls for it, a tint that lifts the surface's own color instead.
  */
final case class ElevationTreatment(shadowOpacity: NormalizedAlpha, surfaceTint: Option[Color])

final case class ElevationLevels(
    base: ElevationTreatment,
    raised: ElevationTreatment,
    floating: ElevationTreatment,
    modal: ElevationTreatment
):

  def forLevel(level: ElevationLevel): ElevationTreatment = level match
    case ElevationLevel.Base     => base
    case ElevationLevel.Raised   => raised
    case ElevationLevel.Floating => floating
    case ElevationLevel.Modal    => modal

object ElevationLevels:

  /** Depth weight per level, strictly increasing -- shadow strength (and, on a dark theme, surface tint) comes out
    * strictly increasing too, regardless of the per-theme step sizes `derive` picks below.
    */
  private val DepthWeight: Map[ElevationLevel, Int] =
    Map(ElevationLevel.Base -> 0, ElevationLevel.Raised -> 1, ElevationLevel.Floating -> 2, ElevationLevel.Modal -> 3)

  /** Derives all four tiers from a theme's own foreground/background -- no theme file needs to declare elevation
    * explicitly for a sensible default to exist.
    *
    * A shadow reads clearly against a light backdrop, so a light theme leans on shadow strength alone. Against a dark
    * backdrop a black shadow barely shows, so a dark theme leans more on a surface tint that lifts toward the
    * foreground as depth increases instead -- the standard dark-theme "elevation overlay" technique.
    * `Theme.luminance`/`EqualContrastLuminanceThreshold` make that light-vs-dark call, exactly as they do for every
    * other such decision in this module.
    */
  def derive(foreground: Color, background: Color): ElevationLevels =
    val isDark = Theme.luminance(background) < Theme.EqualContrastLuminanceThreshold

    val baseShadowOpacity = if isDark then 0.04 else 0.06
    val shadowStep        = if isDark then 0.03 else 0.09
    val tintStep          = if isDark then 0.05 else 0.0

    def treatmentFor(level: ElevationLevel): ElevationTreatment =
      val depth         = DepthWeight(level)
      val shadowOpacity = NormalizedAlpha(baseShadowOpacity + shadowStep * depth)
      val tint = Option.when(tintStep > 0.0 && depth > 0)(Theme.blend(background, foreground, tintStep * depth))
      ElevationTreatment(shadowOpacity, tint)

    ElevationLevels(
      base = treatmentFor(ElevationLevel.Base),
      raised = treatmentFor(ElevationLevel.Raised),
      floating = treatmentFor(ElevationLevel.Floating),
      modal = treatmentFor(ElevationLevel.Modal)
    )
