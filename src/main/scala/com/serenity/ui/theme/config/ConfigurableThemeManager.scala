package com.serenity.ui.theme.config

import java.awt.Color
import java.nio.file.Path

import cats.effect.IO
import com.serenity.ui.theme.*

object ConfigurableThemeManager:

  def configToTheme(config: ThemeConfig): Either[String, Theme] =
    for
      foreground        <- ColorParser.parseColor(ThemeFieldSchema.uiForegroundField.select(config.ui))
      background        <- ColorParser.parseColor(ThemeFieldSchema.uiBackgroundField.select(config.ui))
      cursor            <- ColorParser.parseColor(ThemeFieldSchema.uiCursorField.select(config.ui))
      highlighted       <- convertUiToken(ThemeFieldSchema.uiHighlightedField.select(config.ui))
      menuItem          <- convertUiToken(ThemeFieldSchema.uiMenuItemField.select(config.ui))
      panel             <- convertUiToken(ThemeFieldSchema.uiPanelField.select(config.ui))
      error             <- convertUiToken(ThemeFieldSchema.uiErrorField.select(config.ui))
      warning           <- convertUiToken(config.ui.warning.getOrElse(ThemeFieldSchema.warningDefault))
      border            <- ColorParser.parseColor(ThemeFieldSchema.uiBorderField.select(config.ui))
      panelBorder       <- parseOptionalColor(config.ui.panelBorder, border)
      margin            <- parseOptionalColor(config.ui.margin, background)
      muted             <- ColorParser.parseColor(ThemeFieldSchema.uiMutedField.select(config.ui))
      placeholder       <- ColorParser.parseColor(ThemeFieldSchema.uiPlaceholderField.select(config.ui))
      syntaxColors      <- convertSyntaxColors(config.syntax, background)
      interactionStates <- convertInteractionStates(config.interactionStates, InteractionStates.derive(menuItem))
      elevation         <- convertElevation(config.elevation, ElevationLevels.derive(foreground, background))
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
      syntaxColors = syntaxColors,
      interactionStates = interactionStates,
      elevation = elevation
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
      alpha = NormalizedAlpha(config.alpha.getOrElse(1.0))
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

  private def convertInteractionStates(
    configOpt: Option[InteractionStatesConfig],
    default: InteractionStates
  ): Either[String, InteractionStates] =
    val fields = configOpt.getOrElse(InteractionStatesConfig())

    def resolve(overrideConfig: Option[UiTokenConfig], fallback: ThemeColor): Either[String, ThemeColor] =
      overrideConfig match
        case Some(tokenConfig) => convertUiToken(tokenConfig)
        case None              => Right(fallback)

    for
      hover    <- resolve(fields.hover, default.hover)
      pressed  <- resolve(fields.pressed, default.pressed)
      disabled <- resolve(fields.disabled, default.disabled)
    yield InteractionStates(hover, pressed, disabled)

  private def convertElevation(
    configOpt: Option[ElevationConfig],
    default: ElevationLevels
  ): Either[String, ElevationLevels] =
    val fields = configOpt.getOrElse(ElevationConfig())
    for
      base     <- convertElevationTreatment(fields.base, default.base)
      raised   <- convertElevationTreatment(fields.raised, default.raised)
      floating <- convertElevationTreatment(fields.floating, default.floating)
      modal    <- convertElevationTreatment(fields.modal, default.modal)
    yield ElevationLevels(base, raised, floating, modal)

  private def convertElevationTreatment(
    configOpt: Option[ElevationTreatmentConfig],
    default: ElevationTreatment
  ): Either[String, ElevationTreatment] =
    configOpt match
      case None => Right(default)
      case Some(fields) =>
        val shadowOpacity = fields.shadowOpacity.map(NormalizedAlpha(_)).getOrElse(default.shadowOpacity)
        fields.surfaceTint match
          case None => Right(ElevationTreatment(shadowOpacity, default.surfaceTint))
          case Some(colorStr) =>
            ColorParser.parseColor(colorStr).map(color => ElevationTreatment(shadowOpacity, Some(color)))

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
