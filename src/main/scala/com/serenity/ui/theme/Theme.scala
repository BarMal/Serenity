package com.serenity.ui.theme

import com.googlecode.lanterna.TextColor

case class Theme(
    name: String,
    foreground: TextColor,
    background: TextColor,
    cursor: TextColor,
    highlighted: ThemeColor,
    menuItem: ThemeColor,
    panel: ThemeColor,
    error: ThemeColor,
    border: TextColor,
    muted: TextColor,
    placeholder: TextColor,
    textStyle: TextStyle,
    syntaxColors: Map[SyntaxElement, ThemeColor]
):
  /** Get the color configuration for a specific syntax element */
  def colorFor(element: SyntaxElement): ThemeColor =
    syntaxColors.getOrElse(element, ThemeColor(foreground, background, TextStyle.normal))

  /** Backward-compatible accessors while callers are migrated to semantic names. */
  def foregroundColor: TextColor = foreground
  def backgroundColor: TextColor = background
  def cursorColor: TextColor     = cursor

case class ThemeColor(
    foreground: TextColor,
    background: TextColor,
    style: TextStyle = TextStyle.normal
)

object Theme:

  def dark: Theme = DefaultThemes.defaultDark.copy(name = "dark")

  def light: Theme = DefaultThemes.defaultLight.copy(name = "light")

  def default: Theme = dark
