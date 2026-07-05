package com.serenity.ui.theme.config

import java.awt.Color
import java.nio.file.Path

import cats.effect.IO
import com.serenity.ui.theme.*

object ConfigurableThemeManager:

  def configToTheme(config: ThemeConfig): Either[String, Theme] =
    for
      foreground  <- ColorParser.parseColor(config.ui.foreground)
      background  <- ColorParser.parseColor(config.ui.background)
      cursor      <- ColorParser.parseColor(config.ui.cursor)
      highlighted <- convertUiToken(config.ui.highlighted)
      menuItem    <- convertUiToken(config.ui.menuItem)
      panel       <- convertUiToken(config.ui.panel)
      error       <- convertUiToken(config.ui.error)
      warning <- config.ui.warning match
        case Some(w) => convertUiToken(w)
        case None    => Right(ThemeColor(new java.awt.Color(0xf0b429), new java.awt.Color(0x2b2000)))
      border       <- ColorParser.parseColor(config.ui.border)
      panelBorder  <- parseOptionalColor(config.ui.panelBorder, border)
      margin       <- parseOptionalColor(config.ui.margin, background)
      muted        <- ColorParser.parseColor(config.ui.muted)
      placeholder  <- ColorParser.parseColor(config.ui.placeholder)
      syntaxColors <- convertSyntaxColors(config.syntax, background)
    yield Theme(
      name = config.name,
      foreground = foreground,
      background = background,
      cursor = cursor,
      highlighted = highlighted,
      menuItem = menuItem,
      panel = panel,
      error = error,
      warning = warning,
      border = border,
      panelBorder = panelBorder,
      margin = margin,
      muted = muted,
      placeholder = placeholder,
      textStyle = TextStyle.normal,
      syntaxColors = syntaxColors
    )

  private def parseOptionalColor(value: Option[String], default: Color): Either[String, Color] =
    value match
      case Some(colorStr) => ColorParser.parseColor(colorStr)
      case None           => Right(default)

  private def convertUiToken(config: UiTokenConfig): Either[String, ThemeColor] =
    for
      foreground <- ColorParser.parseColor(config.foreground)
      background <- ColorParser.parseColor(config.background)
    yield ThemeColor(
      foreground = foreground,
      background = background,
      style = TextStyle(
        isBold = config.style.bold,
        isItalic = config.style.italic,
        isUnderlined = config.style.underline
      ),
      alpha = config.alpha.getOrElse(1.0)
    )

  private def convertSyntaxColors(
    syntax: SyntaxColors,
    defaultBackground: Color
  ): Either[String, Map[SyntaxElement, ThemeColor]] =
    val defaultFg = SyntaxElementConfig("#F5F7FA", None, StyleConfig())
    val conversions = List(
      (SyntaxElement.Keyword, syntax.keyword),
      (SyntaxElement.String, syntax.string),
      (SyntaxElement.Comment, syntax.comment),
      (SyntaxElement.Number, syntax.number),
      (SyntaxElement.Operator, syntax.operator),
      (SyntaxElement.Identifier, syntax.identifier),
      (SyntaxElement.Type, syntax.typ.getOrElse(SyntaxElementConfig("#AF7AC5", None, StyleConfig(bold = true)))),
      (SyntaxElement.Delimiter, syntax.delimiter.getOrElse(defaultFg)),
      (SyntaxElement.Whitespace, syntax.whitespace.getOrElse(SyntaxElementConfig("#000000", None, StyleConfig()))),
      (
        SyntaxElement.Error,
        syntax.error.getOrElse(SyntaxElementConfig("#FF6B6B", None, StyleConfig(underline = true)))
      ),
      (SyntaxElement.Normal, syntax.normal.getOrElse(defaultFg))
    )

    val results = conversions.map {
      case (element, config) =>
        convertSyntaxElementConfig(config, defaultBackground).map(element -> _)
    }

    results.foldLeft(Right(Map.empty[SyntaxElement, ThemeColor]): Either[String, Map[SyntaxElement, ThemeColor]]) {
      case (Right(acc), Right((element, color))) => Right(acc + (element -> color))
      case (Left(error), _)                      => Left(error)
      case (_, Left(error))                      => Left(error)
    }

  private def convertSyntaxElementConfig(
    config: SyntaxElementConfig,
    defaultBackground: Color
  ): Either[String, ThemeColor] =
    for
      foreground <- ColorParser.parseColor(config.foreground)
      background <- config.background match
        case None           => Right(defaultBackground)
        case Some(colorStr) => ColorParser.parseColor(colorStr)
    yield ThemeColor(
      foreground = foreground,
      background = background,
      style = TextStyle(
        isBold = config.style.bold,
        isItalic = config.style.italic,
        isUnderlined = config.style.underline
      )
    )

class ConfigurableThemeManager(loader: ThemeConfigLoader):

  /** Load and convert theme from file */
  def loadThemeFromFile(path: Path): IO[Theme] =
    for
      config <- loader.loadThemeFromFile(path)
      theme  <- IO.fromEither(ConfigurableThemeManager.configToTheme(config).left.map(new RuntimeException(_)))
    yield theme

  /** Load and convert theme from resource */
  def loadThemeFromResource(resourcePath: String): IO[Theme] =
    for
      config <- loader.loadThemeFromResource(resourcePath)
      theme  <- IO.fromEither(ConfigurableThemeManager.configToTheme(config).left.map(new RuntimeException(_)))
    yield theme

  /** Load theme by name, checking internal themes first, then user directory, then resources */
  def loadThemeByName(themeName: String): IO[Theme] =
    // Check internal themes first
    com.serenity.ui.theme.DefaultThemes.allInternal.get(themeName) match
      case Some(theme) => IO.pure(theme)
      case None =>
        val userFile     = loader.getUserThemesDirectory.resolve(s"$themeName.conf")
        val resourcePath = s"${loader.getDefaultThemesResourcePath}/$themeName.conf"

        loadThemeFromFile(userFile)
          .handleErrorWith(_ => loadThemeFromResource(resourcePath))
          .handleErrorWith(error =>
            IO.raiseError(
              new RuntimeException(s"Theme '$themeName' not found in internal, user directory, or resources", error)
            )
          )

  /** List all available themes (internal + user + bundled) */
  def listAvailableThemes: IO[List[String]] =
    for
      userThemes <- loader
        .listAvailableThemes(loader.getUserThemesDirectory)
        .map(_.map(_.getFileName.toString.stripSuffix(".conf")))
      bundledThemes <- loader.listBundledThemes
      internalThemes = com.serenity.ui.theme.DefaultThemes.allInternal.keys.toList
    yield (internalThemes ++ userThemes ++ bundledThemes).distinct.sorted
