package com.serenity.ui.theme.config

import java.awt.Color
import java.nio.file.Path

import cats.effect.IO
import com.serenity.ui.theme.*

object ConfigurableThemeManager:

  def configToTheme(config: ThemeConfig): Either[String, Theme] =
    for
      foreground   <- ColorParser.parseColor(ThemeFieldSchema.uiForegroundField.select(config.ui))
      background   <- ColorParser.parseColor(ThemeFieldSchema.uiBackgroundField.select(config.ui))
      cursor       <- ColorParser.parseColor(ThemeFieldSchema.uiCursorField.select(config.ui))
      highlighted  <- convertUiToken(ThemeFieldSchema.uiHighlightedField.select(config.ui))
      menuItem     <- convertUiToken(ThemeFieldSchema.uiMenuItemField.select(config.ui))
      panel        <- convertUiToken(ThemeFieldSchema.uiPanelField.select(config.ui))
      error        <- convertUiToken(ThemeFieldSchema.uiErrorField.select(config.ui))
      warning      <- convertUiToken(config.ui.warning.getOrElse(ThemeFieldSchema.warningDefault))
      border       <- ColorParser.parseColor(ThemeFieldSchema.uiBorderField.select(config.ui))
      panelBorder  <- parseOptionalColor(config.ui.panelBorder, border)
      margin       <- parseOptionalColor(config.ui.margin, background)
      muted        <- ColorParser.parseColor(ThemeFieldSchema.uiMutedField.select(config.ui))
      placeholder  <- ColorParser.parseColor(ThemeFieldSchema.uiPlaceholderField.select(config.ui))
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
    val mandatory = ThemeFieldSchema.mandatorySyntaxFields.map(field => (field.element, field.select(syntax)))
    val optional =
      ThemeFieldSchema.syntaxFields.map(field => (field.element, field.select(syntax).getOrElse(field.default)))
    val conversions = mandatory ++ optional

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
