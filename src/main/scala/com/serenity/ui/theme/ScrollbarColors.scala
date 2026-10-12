package com.serenity.ui.theme

import com.serenity.ui.color.RenderColor

/** What a scrollbar's thumb is painted in at rest and while it is held. */
final case class ScrollbarColors(thumb: RenderColor, thumbHeld: RenderColor)

object ScrollbarColors:

  /** How far each thumb colour leans from the background towards the text: faint enough to stay out of the way of
    * prose, strong enough that a held thumb reads as taken.
    */
  private val RestBlend: Double = 0.22
  private val HeldBlend: Double = 0.55

  /** Blending towards the foreground, as [[InteractionStates.derive]] does, keeps the thumb legible on a light theme
    * and a dark one alike: a theme's foreground is already chosen to contrast with its background.
    */
  def derive(theme: Theme): ScrollbarColors =
    ScrollbarColors(
      thumb = theme.background.blendToward(theme.foreground, RestBlend),
      thumbHeld = theme.background.blendToward(theme.foreground, HeldBlend)
    )
