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
    border: Color,
    muted: Color,
    placeholder: Color,
    textStyle: TextStyle,
    syntaxColors: Map[SyntaxElement, ThemeColor]
):
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

object Theme:

  def dark: Theme = DefaultThemes.defaultDark.copy(name = "dark")

  def light: Theme = DefaultThemes.defaultLight.copy(name = "light")

  def default: Theme = dark
