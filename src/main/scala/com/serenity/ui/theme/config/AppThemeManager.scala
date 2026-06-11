package com.serenity.ui.theme.config

import cats.effect.IO
import com.serenity.state.models.AppState
import com.serenity.ui.theme.Theme

/** Application-level theme manager that integrates with AppState */
class AppThemeManager:

  private val themeManager        = new StatefulThemeManager()
  private val configurableManager = new ConfigurableThemeManager(new ThemeConfigLoader())

  /** Initialize the application with a default theme */
  def initializeWithTheme(themeName: String = "dark"): IO[Theme] =
    themeManager
      .loadAndSetTheme(themeName)
      .handleErrorWith(_ =>
        // Fallback to internal default if theme loading fails
        val defaultTheme = com.serenity.ui.theme.DefaultThemes.default
        themeManager.setCurrentTheme(defaultTheme, defaultTheme.name).as(defaultTheme)
      )

  /** Get the current active theme */
  def getCurrentTheme: IO[Option[Theme]] =
    themeManager.getCurrentTheme

  /** Switch to a different theme by name and update app state */
  def switchTheme(themeName: String): IO[(Theme, AppState => AppState)] =
    for
      newTheme <- themeManager.loadAndSetTheme(themeName)
      stateUpdate = (state: AppState) => state.copy(theme = newTheme)
    yield (newTheme, stateUpdate)

  /** Reload the current theme (useful for config file changes) */
  def reloadCurrentTheme: IO[Option[(Theme, AppState => AppState)]] =
    for reloadedTheme <- themeManager.reloadCurrentTheme
    yield reloadedTheme.map { theme =>
      val stateUpdate = (state: AppState) => state.copy(theme = theme)
      (theme, stateUpdate)
    }

  /** List all available themes */
  def listAvailableThemes: IO[List[String]] =
    configurableManager.listAvailableThemes

  /** Load a specific theme without setting it as current */
  def loadTheme(themeName: String): IO[Theme] =
    configurableManager.loadThemeByName(themeName)

  /** Create an AppState update function for a given theme */
  def createThemeUpdate(theme: Theme): AppState => AppState =
    state => state.copy(theme = theme)

object AppThemeManager:
  /** Create a new instance with default configuration */
  def create: AppThemeManager = new AppThemeManager()
