package com.serenity.ui.theme

import com.googlecode.lanterna.TextColor

object DefaultThemes:

  /** Internal default dark theme - always available, no config files required */
  val defaultDark: Theme = 
    Theme(
      name = "default-dark",
      foregroundColor = TextColor.ANSI.WHITE,
      backgroundColor = TextColor.ANSI.BLACK,
      cursorColor = TextColor.ANSI.YELLOW,
      textStyle = TextStyle.normal,
      syntaxColors = Map(
        SyntaxElement.Keyword -> ThemeColor(TextColor.ANSI.BLUE_BRIGHT, TextColor.ANSI.BLACK, TextStyle.bold),
        SyntaxElement.String -> ThemeColor(TextColor.ANSI.GREEN, TextColor.ANSI.BLACK),
        SyntaxElement.Comment -> ThemeColor(TextColor.ANSI.BLACK_BRIGHT, TextColor.ANSI.BLACK, TextStyle.italic),
        SyntaxElement.Number -> ThemeColor(TextColor.ANSI.CYAN, TextColor.ANSI.BLACK),
        SyntaxElement.Operator -> ThemeColor(TextColor.ANSI.YELLOW, TextColor.ANSI.BLACK),
        SyntaxElement.Identifier -> ThemeColor(TextColor.ANSI.WHITE, TextColor.ANSI.BLACK),
        SyntaxElement.Type -> ThemeColor(TextColor.ANSI.MAGENTA, TextColor.ANSI.BLACK, TextStyle.bold),
        SyntaxElement.Delimiter -> ThemeColor(TextColor.ANSI.WHITE_BRIGHT, TextColor.ANSI.BLACK),
        SyntaxElement.Whitespace -> ThemeColor(TextColor.ANSI.BLACK, TextColor.ANSI.BLACK),
        SyntaxElement.Error -> ThemeColor(TextColor.ANSI.RED_BRIGHT, TextColor.ANSI.BLACK, TextStyle.underlined),
        SyntaxElement.Normal -> ThemeColor(TextColor.ANSI.WHITE, TextColor.ANSI.BLACK)
      )
    )

  /** Internal default light theme - always available, no config files required */
  val defaultLight: Theme = 
    Theme(
      name = "default-light",
      foregroundColor = TextColor.ANSI.BLACK,
      backgroundColor = TextColor.ANSI.WHITE,
      cursorColor = TextColor.ANSI.BLUE,
      textStyle = TextStyle.normal,
      syntaxColors = Map(
        SyntaxElement.Keyword -> ThemeColor(TextColor.ANSI.BLUE, TextColor.ANSI.WHITE, TextStyle.bold),
        SyntaxElement.String -> ThemeColor(TextColor.ANSI.GREEN, TextColor.ANSI.WHITE),
        SyntaxElement.Comment -> ThemeColor(TextColor.ANSI.BLACK_BRIGHT, TextColor.ANSI.WHITE, TextStyle.italic),
        SyntaxElement.Number -> ThemeColor(TextColor.ANSI.CYAN_BRIGHT, TextColor.ANSI.WHITE),
        SyntaxElement.Operator -> ThemeColor(TextColor.ANSI.MAGENTA, TextColor.ANSI.WHITE),
        SyntaxElement.Identifier -> ThemeColor(TextColor.ANSI.BLACK, TextColor.ANSI.WHITE),
        SyntaxElement.Type -> ThemeColor(TextColor.ANSI.BLUE_BRIGHT, TextColor.ANSI.WHITE, TextStyle.bold),
        SyntaxElement.Delimiter -> ThemeColor(TextColor.ANSI.BLACK_BRIGHT, TextColor.ANSI.WHITE),
        SyntaxElement.Whitespace -> ThemeColor(TextColor.ANSI.WHITE, TextColor.ANSI.WHITE),
        SyntaxElement.Error -> ThemeColor(TextColor.ANSI.RED, TextColor.ANSI.WHITE, TextStyle.underlined),
        SyntaxElement.Normal -> ThemeColor(TextColor.ANSI.BLACK, TextColor.ANSI.WHITE)
      )
    )

  /** Get all internal themes */
  val allInternal: Map[String, Theme] = Map(
    "default-dark" -> defaultDark,
    "default-light" -> defaultLight
  )

  /** Get default theme (fallback) */
  val default: Theme = defaultDark