package com.serenity.config

/** `theme.*`: following the operating system's appearance. */
private[config] object ConfigFieldsTheme:

  import ConfigFieldSyntax.*
  import FieldCodec.*

  private val themeName: FieldCodec[String] = string.filtered(_.nonEmpty)

  val fields: List[ConfigField[?]] = List(
    field("theme.follow_system")(boolean)(
      _.themeFollowConfig.followSystem,
      (config, value) => config.withThemeFollowConfig(config.themeFollowConfig.copy(followSystem = value))
    ),
    field("theme.light")(themeName)(
      _.themeFollowConfig.lightTheme,
      (config, value) => config.withThemeFollowConfig(config.themeFollowConfig.copy(lightTheme = value))
    ),
    field("theme.dark")(themeName)(
      _.themeFollowConfig.darkTheme,
      (config, value) => config.withThemeFollowConfig(config.themeFollowConfig.copy(darkTheme = value))
    ),
    field("theme.high_contrast")(themeName)(
      _.themeFollowConfig.highContrastTheme,
      (config, value) => config.withThemeFollowConfig(config.themeFollowConfig.copy(highContrastTheme = value))
    )
  )
