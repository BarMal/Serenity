package com.serenity.ui.theme

import com.googlecode.lanterna.TextColor

case class Theme(
    name: String,
    foregroundColor: TextColor,
    backgroundColor: TextColor,
    cursorColor: TextColor,
    textStyle: TextStyle,
    syntaxColors: Map[SyntaxElement, ThemeColor]
):
  /** Get the color configuration for a specific syntax element */
  def colorFor(element: SyntaxElement): ThemeColor =
    syntaxColors.getOrElse(element, ThemeColor(foregroundColor, backgroundColor, TextStyle.normal))

case class ThemeColor(
    foreground: TextColor,
    background: TextColor = TextColor.ANSI.BLACK,
    style: TextStyle = TextStyle.normal
)

object Theme:

  def dark: Theme =
    Theme(
      name = "dark",
      foregroundColor = TextColor.ANSI.WHITE,
      backgroundColor = TextColor.ANSI.BLACK,
      cursorColor = TextColor.ANSI.WHITE,
      textStyle = TextStyle.normal,
      syntaxColors = Map(
        SyntaxElement.Keyword    -> ThemeColor(TextColor.ANSI.BLUE_BRIGHT, TextColor.ANSI.BLACK, TextStyle.bold),
        SyntaxElement.String     -> ThemeColor(TextColor.ANSI.GREEN, TextColor.ANSI.BLACK),
        SyntaxElement.Comment    -> ThemeColor(TextColor.ANSI.BLACK_BRIGHT, TextColor.ANSI.BLACK, TextStyle.italic),
        SyntaxElement.Number     -> ThemeColor(TextColor.ANSI.CYAN, TextColor.ANSI.BLACK),
        SyntaxElement.Operator   -> ThemeColor(TextColor.ANSI.YELLOW, TextColor.ANSI.BLACK),
        SyntaxElement.Identifier -> ThemeColor(TextColor.ANSI.WHITE, TextColor.ANSI.BLACK),
        SyntaxElement.Type       -> ThemeColor(TextColor.ANSI.MAGENTA, TextColor.ANSI.BLACK, TextStyle.bold),
        SyntaxElement.Delimiter  -> ThemeColor(TextColor.ANSI.WHITE_BRIGHT, TextColor.ANSI.BLACK),
        SyntaxElement.Whitespace -> ThemeColor(TextColor.ANSI.BLACK, TextColor.ANSI.BLACK),
        SyntaxElement.Error      -> ThemeColor(TextColor.ANSI.RED_BRIGHT, TextColor.ANSI.BLACK, TextStyle.underlined),
        SyntaxElement.Normal     -> ThemeColor(TextColor.ANSI.WHITE, TextColor.ANSI.BLACK)
      )
    )

  def light: Theme =
    Theme(
      name = "light",
      foregroundColor = TextColor.ANSI.BLACK,
      backgroundColor = TextColor.ANSI.WHITE,
      cursorColor = TextColor.ANSI.BLACK,
      textStyle = TextStyle.normal,
      syntaxColors = Map(
        SyntaxElement.Keyword    -> ThemeColor(TextColor.ANSI.BLUE, TextColor.ANSI.WHITE, TextStyle.bold),
        SyntaxElement.String     -> ThemeColor(TextColor.ANSI.GREEN, TextColor.ANSI.WHITE),
        SyntaxElement.Comment    -> ThemeColor(TextColor.ANSI.BLACK_BRIGHT, TextColor.ANSI.WHITE, TextStyle.italic),
        SyntaxElement.Number     -> ThemeColor(TextColor.ANSI.CYAN_BRIGHT, TextColor.ANSI.WHITE),
        SyntaxElement.Operator   -> ThemeColor(TextColor.ANSI.MAGENTA, TextColor.ANSI.WHITE),
        SyntaxElement.Identifier -> ThemeColor(TextColor.ANSI.BLACK, TextColor.ANSI.WHITE),
        SyntaxElement.Type       -> ThemeColor(TextColor.ANSI.BLUE_BRIGHT, TextColor.ANSI.WHITE, TextStyle.bold),
        SyntaxElement.Delimiter  -> ThemeColor(TextColor.ANSI.BLACK_BRIGHT, TextColor.ANSI.WHITE),
        SyntaxElement.Whitespace -> ThemeColor(TextColor.ANSI.WHITE, TextColor.ANSI.WHITE),
        SyntaxElement.Error      -> ThemeColor(TextColor.ANSI.RED, TextColor.ANSI.WHITE, TextStyle.underlined),
        SyntaxElement.Normal     -> ThemeColor(TextColor.ANSI.BLACK, TextColor.ANSI.WHITE)
      )
    )

  def default: Theme = dark
