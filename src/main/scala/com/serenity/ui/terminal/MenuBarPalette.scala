package com.serenity.ui.terminal

import com.serenity.ui.color.RenderColor
import com.serenity.ui.theme.{Theme, ThemeColor}

/** The colours of the Windows and Linux menu bar, derived from the editor theme so the bar does not sit as a grey strip
  * between a themed title bar and a themed canvas. Every text pair clears WCAG AA (4.5:1) wherever the theme's own pair
  * does, and a theme whose pair reaches AAA (7:1) keeps that through the accelerator text. Border and separator are
  * non-text and clear 3:1 against the popup, falling back to the foreground if the theme's border colour is too faint.
  */
final case class MenuBarPalette(
    barBackground: RenderColor,
    barForeground: RenderColor,
    popupBackground: RenderColor,
    popupForeground: RenderColor,
    selectionBackground: RenderColor,
    selectionForeground: RenderColor,
    hoverBackground: RenderColor,
    hoverForeground: RenderColor,
    disabledForeground: RenderColor,
    acceleratorForeground: RenderColor,
    separator: RenderColor,
    border: RenderColor
)

object MenuBarPalette:

  val TextContrastFloor: Double    = 4.5
  val EnhancedTextContrast: Double = 7.0
  val NonTextContrastFloor: Double = 3.0

  private val AcceleratorBlends: List[Double] = List(0.3, 0.2, 0.1, 0.0)

  def fromTheme(theme: Theme): MenuBarPalette =
    val bar   = theme.panel
    val popup = theme.menuItem
    val hover = theme.interactionStates.hover
    MenuBarPalette(
      barBackground = bar.background,
      barForeground = bar.foreground,
      popupBackground = popup.background,
      popupForeground = popup.foreground,
      selectionBackground = theme.highlighted.background,
      selectionForeground = theme.highlighted.foreground,
      hoverBackground = hover.background,
      hoverForeground = hover.foreground,
      disabledForeground = theme.interactionStates.disabled.foreground,
      acceleratorForeground = acceleratorOn(popup.foreground, popup.background),
      separator = nonText(theme.panelBorder, popup),
      border = nonText(theme.panelBorder, popup)
    )

  /** The quietest tint of `foreground` that still keeps the contrast level the pair itself has reached. */
  private def acceleratorOn(foreground: RenderColor, background: RenderColor): RenderColor =
    val required =
      if Theme.contrastRatio(foreground, background) >= EnhancedTextContrast then EnhancedTextContrast
      else TextContrastFloor
    AcceleratorBlends
      .map(factor => foreground.blendToward(background, factor))
      .find(candidate => Theme.contrastRatio(candidate, background) >= required)
      .getOrElse(foreground)

  private def nonText(candidate: RenderColor, against: ThemeColor): RenderColor =
    if Theme.contrastRatio(candidate, against.background) >= NonTextContrastFloor then candidate
    else against.foreground
