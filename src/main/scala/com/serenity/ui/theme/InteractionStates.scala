package com.serenity.ui.theme

import java.awt.Color

/** Hover/pressed/disabled treatments for an interactive surface row or control (a list row, a menu item, a button).
  * Each state is a full [[ThemeColor]] so a renderer can later swap in the whole foreground/background/style/alpha pair
  * for that state without recomputing anything (issues #1093, #1612).
  */
final case class InteractionStates(hover: ThemeColor, pressed: ThemeColor, disabled: ThemeColor)

object InteractionStates:

  /** How far the background leans toward the foreground for hover/pressed. Blending toward the foreground -- rather
    * than a fixed lighten/darken -- works on both light and dark themes: a theme's foreground is already chosen to
    * contrast with its background in whichever direction that theme needs, so leaning the background toward it makes
    * the surface stand out a little regardless of which way that is. `SurfaceMaterials.blend` uses the same idiom for
    * painting; `pressed` reuses `hover`'s direction at roughly twice the strength, so pressed always reads as a deeper
    * version of hover rather than an unrelated treatment.
    */
  private val HoverBlend: Double   = 0.12
  private val PressedBlend: Double = 0.24

  /** WCAG 2.x non-text contrast minimum (SC 1.4.11) -- the floor a disabled treatment must still clear so it reads as
    * "dimmed", not "gone".
    */
  val DisabledContrastFloor: Double = 3.0

  /** Candidate blend-toward-background factors for the disabled foreground, tried from most to least washed out. The
    * first one that still clears [[DisabledContrastFloor]] wins, so disabled is muted as much as the theme's own
    * contrast budget allows.
    */
  private val DisabledBlendCandidates: List[Double] = List(0.6, 0.5, 0.4, 0.3, 0.2, 0.1, 0.0)

  /** Derives hover/pressed/disabled from a single base treatment -- the surface a theme already declares for the
    * interactive row/control the states apply to. No theme file needs to declare these explicitly for a sensible,
    * legible default to exist.
    */
  def derive(base: ThemeColor): InteractionStates =
    InteractionStates(
      hover = base.copy(background = Theme.blend(base.background, base.foreground, HoverBlend)),
      pressed = base.copy(background = Theme.blend(base.background, base.foreground, PressedBlend)),
      disabled = base.copy(foreground = mutedForeground(base))
    )

  /** The most washed-out foreground (closest to the background) that still clears [[DisabledContrastFloor]] against it.
    * Falls back to the base foreground unchanged if even that doesn't clear the floor, so a disabled treatment is never
    * less legible than the surface it is disabling.
    */
  private def mutedForeground(base: ThemeColor): Color =
    DisabledBlendCandidates
      .map(factor => Theme.blend(base.foreground, base.background, factor))
      .find(candidate => Theme.contrastRatio(candidate, base.background) >= DisabledContrastFloor)
      .getOrElse(base.foreground)
