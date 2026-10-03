package com.serenity.ui.theme.config

import cats.effect.IO
import cats.syntax.all.*
import com.serenity.state.models.AppState
import com.serenity.ui.theme.{DefaultThemes, Theme}
import org.slf4j.LoggerFactory

/** Application-level theme manager that integrates with AppState */
class AppThemeManager:

  private val themeManager        = new StatefulThemeManager()
  private val configurableManager = new ConfigurableThemeManager(new ThemeConfigLoader())

  /** Initialize the application with a default theme */
  def initializeWithTheme(themeName: String = "dark"): IO[Theme] =
    themeManager
      .loadAndSetTheme(themeName)
      .handleErrorWith(_ =>
        val defaultTheme = DefaultThemes.default
        AppThemeManager
          .warnRemovedTheme(themeName, defaultTheme.name)
          .whenA(DefaultThemes.removedInternal.contains(themeName)) >>
          themeManager.setCurrentTheme(defaultTheme, defaultTheme.name).as(defaultTheme)
      )

  /** Get the current active theme */
  def getCurrentTheme: IO[Option[Theme]] =
    themeManager.getCurrentTheme

  /** Switch to a different theme by name and update app state */
  def switchTheme(themeName: String): IO[(Theme, AppState => AppState)] =
    for
      newTheme <- themeManager.loadAndSetTheme(themeName)
      stateUpdate = (state: AppState) => state.copy(persisted = state.persisted.copy(theme = newTheme))
    yield (newTheme, stateUpdate)

  /** Reload the current theme (useful for config file changes) */
  def reloadCurrentTheme: IO[Option[(Theme, AppState => AppState)]] =
    for reloadedTheme <- themeManager.reloadCurrentTheme
    yield reloadedTheme.map { theme =>
      val stateUpdate = (state: AppState) => state.copy(persisted = state.persisted.copy(theme = theme))
      (theme, stateUpdate)
    }

  /** List all available themes */
  def listAvailableThemes: IO[List[String]] =
    configurableManager.listAvailableThemes

  /** Load a specific theme without setting it as current */
  def loadTheme(themeName: String): IO[Theme] =
    configurableManager.loadThemeByName(themeName)

  def writeUserTheme(config: ThemeConfig): IO[java.nio.file.Path] =
    ThemeConfigWriter.writeUserTheme(config)

  /** Create an AppState update function for a given theme */
  def createThemeUpdate(theme: Theme): AppState => AppState =
    state => state.copy(persisted = state.persisted.copy(theme = theme))

object AppThemeManager:
  private val logger = LoggerFactory.getLogger(classOf[AppThemeManager])

  private def warnRemovedTheme(themeName: String, fallbackName: String): IO[Unit] =
    IO(logger.warn(s"Theme '$themeName' was removed; using the default theme '$fallbackName' instead"))

  /** Create a new instance with default configuration */
  def create: AppThemeManager = new AppThemeManager()
