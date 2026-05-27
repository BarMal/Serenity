package com.serenity.ui.theme

import com.serenity.ui.theme.config.{ConfigurableThemeManager, ThemeConfig}

object DefaultThemes:

  private def load(config: ThemeConfig): Theme =
    ConfigurableThemeManager.configToTheme(config) match
      case Right(theme) => theme
      case Left(error)  => throw new IllegalStateException(s"Invalid bundled theme config: $error")

  /** Internal default dark theme - always available, no external config files required */
  val defaultDark: Theme = load(ThemeConfig.defaultDark)

  /** Internal default light theme - always available, no external config files required */
  val defaultLight: Theme = load(ThemeConfig.defaultLight)

  /** Get all internal themes */
  val allInternal: Map[String, Theme] = Map(
    "default-dark"  -> defaultDark,
    "default-light" -> defaultLight
  )

  /** Get default theme (fallback) */
  val default: Theme = defaultDark
