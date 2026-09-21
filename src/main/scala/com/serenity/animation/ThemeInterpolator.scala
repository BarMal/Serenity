package com.serenity.animation

import java.awt.Color

import com.serenity.ui.theme.{
  ElevationLevels,
  ElevationTreatment,
  InteractionStates,
  NormalizedAlpha,
  SyntaxElement,
  Theme,
  ThemeColor
}

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
      panelBorder = blendColor(from.panelBorder, to.panelBorder, t),
      margin = blendColor(from.margin, to.margin, t),
      muted = blendColor(from.muted, to.muted, t),
      placeholder = blendColor(from.placeholder, to.placeholder, t),
      textStyle = to.textStyle,
      syntaxColors = blendSyntaxColors(from.syntaxColors, to.syntaxColors, t),
      interactionStates = blendInteractionStates(from.interactionStates, to.interactionStates, t),
      elevation = blendElevation(from.elevation, to.elevation, t)
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
      alpha = NormalizedAlpha(from.alpha.value + (to.alpha.value - from.alpha.value) * t)
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

  private def blendInteractionStates(from: InteractionStates, to: InteractionStates, t: Double): InteractionStates =
    InteractionStates(
      hover = blendThemeColor(from.hover, to.hover, t),
      pressed = blendThemeColor(from.pressed, to.pressed, t),
      disabled = blendThemeColor(from.disabled, to.disabled, t)
    )

  private def blendElevation(from: ElevationLevels, to: ElevationLevels, t: Double): ElevationLevels =
    ElevationLevels(
      base = blendElevationTreatment(from.base, to.base, t),
      raised = blendElevationTreatment(from.raised, to.raised, t),
      floating = blendElevationTreatment(from.floating, to.floating, t),
      modal = blendElevationTreatment(from.modal, to.modal, t)
    )

  private def blendElevationTreatment(
    from: ElevationTreatment,
    to: ElevationTreatment,
    t: Double
  ): ElevationTreatment =
    ElevationTreatment(
      shadowOpacity = NormalizedAlpha(
        from.shadowOpacity.value + (to.shadowOpacity.value - from.shadowOpacity.value) * t
      ),
      // Neither side's tint carries its own opacity to fade through, so a transition where only one side has a
      // tint just holds that side's color rather than popping it in/out partway through.
      surfaceTint = (from.surfaceTint, to.surfaceTint) match
        case (Some(fromTint), Some(toTint)) => Some(blendColor(fromTint, toTint, t))
        case (Some(fromTint), None)         => Some(fromTint)
        case (None, Some(toTint))           => Some(toTint)
        case (None, None)                   => None
    )
