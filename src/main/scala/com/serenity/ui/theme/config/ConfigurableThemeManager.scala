package com.serenity.ui.theme.config

import java.nio.file.Path

import cats.effect.IO
import com.googlecode.lanterna.TextColor
import com.serenity.ui.theme.*

object ConfigurableThemeManager:

  /** Convert ThemeConfig to Theme object */
  def configToTheme(config: ThemeConfig): Either[String, Theme] =
    for
      foreground   <- ColorParser.parseColor(config.colors.foreground)
      background   <- ColorParser.parseColor(config.colors.background)
      cursor       <- ColorParser.parseColor(config.colors.cursor)
      syntaxColors <- convertSyntaxColors(config.syntax, background)
    yield Theme(
      name = config.name,
      foregroundColor = foreground,
      backgroundColor = background,
      cursorColor = cursor,
      textStyle = TextStyle.normal,
      syntaxColors = syntaxColors
    )

  /** Convert syntax colors configuration to map */
  private def convertSyntaxColors(
    syntax: SyntaxColors,
    defaultBackground: TextColor
  ): Either[String, Map[SyntaxElement, ThemeColor]] =
    val conversions = List(
      (SyntaxElement.Keyword, syntax.keyword),
      (SyntaxElement.String, syntax.string),
      (SyntaxElement.Comment, syntax.comment),
      (SyntaxElement.Number, syntax.number),
      (SyntaxElement.Operator, syntax.operator),
      (SyntaxElement.Identifier, syntax.identifier),
      (SyntaxElement.Type, syntax.typ),
      (SyntaxElement.Delimiter, syntax.delimiter),
      (SyntaxElement.Whitespace, syntax.whitespace),
      (SyntaxElement.Error, syntax.error),
      (SyntaxElement.Normal, syntax.normal)
    )

    val results = conversions.map {
      case (element, config) =>
        convertSyntaxElementConfig(config, defaultBackground).map(element -> _)
    }

    // Collect all errors or return success
    results.foldLeft(Right(Map.empty[SyntaxElement, ThemeColor]): Either[String, Map[SyntaxElement, ThemeColor]]) {
      case (Right(acc), Right((element, color))) => Right(acc + (element -> color))
      case (Left(error), _)                      => Left(error)
      case (_, Left(error))                      => Left(error)
    }

  /** Convert a single syntax element configuration */
  private def convertSyntaxElementConfig(
    config: SyntaxElementConfig,
    defaultBackground: TextColor
  ): Either[String, ThemeColor] =
    for
      foreground <- ColorParser.parseColor(config.foreground)
      background <-
        if config.background == "default" then Right(defaultBackground)
        else ColorParser.parseColor(config.background)
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
