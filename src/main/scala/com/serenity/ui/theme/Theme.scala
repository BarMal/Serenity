package com.serenity.ui.theme

import java.awt.Color

case class Theme(
    name: String,
    foreground: Color,
    background: Color,
    cursor: Color,
    highlighted: ThemeColor,
    menuItem: ThemeColor,
    panel: ThemeColor,
    error: ThemeColor,
    warning: ThemeColor,
    border: Color,
    panelBorder: Color,
    margin: Color,
    muted: Color,
    placeholder: Color,
    textStyle: TextStyle,
    syntaxColors: Map[SyntaxElement, ThemeColor]
):
  /** Accent used for interactive affordances such as the caret and primary controls. */
  def accent: Color = cursor

  /** Quiet selected-row treatment; selection remains distinguishable from focus outlines. */
  def selection: ThemeColor = highlighted

  /** Focus outline colour. Focused controls also retain their border/outline treatment. */
  def focus: Color = border

  /** Non-colour focus cue used for selected actions and controls. */
  def focusStyle: TextStyle = TextStyle.bold

  /** Active-pane indication, separate from the general focus outline and row selection. */
  def activePane: Color = panelBorder

  /** Raised surface colour pair used by panels, overlays, and chrome. */
  def surface: ThemeColor = panel

  /** Status treatments for error and warning feedback. */
  def status: ThemeStatus = ThemeStatus(error, warning)

  /** WCAG relative contrast of normal readable text against the application background. */
  def normalTextContrast: Double = Theme.contrastRatio(foreground, background)

  /** WCAG relative contrast of control labels against their quiet menu surface. */
  def controlLabelContrast: Double = Theme.contrastRatio(menuItem.foreground, menuItem.background)

  /** WCAG relative contrast of text against a raised panel or overlay surface. */
  def surfaceTextContrast: Double = Theme.contrastRatio(surface.foreground, surface.background)

  /** WCAG relative contrast of placeholder text against the application background. */
  def placeholderTextContrast: Double = Theme.contrastRatio(placeholder, background)

  /** WCAG relative contrast of selected text against its selection surface. */
  def selectionTextContrast: Double = Theme.contrastRatio(selection.foreground, selection.background)

  def colorFor(element: SyntaxElement): ThemeColor =
    syntaxColors.getOrElse(element, ThemeColor(foreground, background, TextStyle.normal))

  def foregroundColor: Color = foreground
  def backgroundColor: Color = background
  def cursorColor: Color     = cursor

case class ThemeColor(
    foreground: Color,
    background: Color,
    style: TextStyle = TextStyle.normal,
    alpha: Double = 1.0
)

/** Semantic status treatments kept distinct from selection, focus, and regular text roles. */
case class ThemeStatus(error: ThemeColor, warning: ThemeColor)

object Theme:

  /** Relative luminance contrast ratio defined by WCAG 2.x. */
  def contrastRatio(foreground: Color, background: Color): Double =
    val foregroundLuminance = relativeLuminance(foreground)
    val backgroundLuminance = relativeLuminance(background)
    (foregroundLuminance.max(backgroundLuminance) + 0.05) / (foregroundLuminance.min(backgroundLuminance) + 0.05)

  private def relativeLuminance(color: Color): Double =
    def linear(channel: Int): Double =
      val normalized = channel / 255.0
      if normalized <= 0.04045 then normalized / 12.92 else math.pow((normalized + 0.055) / 1.055, 2.4)

    (0.2126 * linear(color.getRed)) + (0.7152 * linear(color.getGreen)) + (0.0722 * linear(color.getBlue))

  def dark: Theme = DefaultThemes.defaultDark.copy(name = "dark")

  def light: Theme = DefaultThemes.defaultLight.copy(name = "light")

  def default: Theme = dark
