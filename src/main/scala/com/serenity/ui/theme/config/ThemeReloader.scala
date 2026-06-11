package com.serenity.ui.theme.config

import java.nio.file.Path

import cats.effect.{IO, Ref}
import com.serenity.ui.theme.Theme

class ThemeReloader(
    private val loader: ThemeConfigLoader = new ThemeConfigLoader(),
    private val manager: ConfigurableThemeManager = new ConfigurableThemeManager(new ThemeConfigLoader())
):

  /** Load and convert theme configuration from file (for testing) */
  def loadAndConvertTheme(path: Path): IO[ThemeConfig] =
    loader.loadThemeFromFile(path)

  /** Reload theme by name and return the new Theme object */
  def reloadTheme(themeName: String): IO[Theme] =
    manager.loadThemeByName(themeName)

  /** Reload theme from specific file path */
  def reloadThemeFromFile(path: Path): IO[Theme] =
    manager.loadThemeFromFile(path)

  /** Reload theme from resource */
  def reloadThemeFromResource(resourcePath: String): IO[Theme] =
    manager.loadThemeFromResource(resourcePath)

/** Theme manager that maintains current theme state and supports reloading */
class StatefulThemeManager(
    private val reloader: ThemeReloader = new ThemeReloader()
):
  private val currentThemeRef: Ref[IO, Option[Theme]]      = Ref.unsafe(None)
  private val currentThemeNameRef: Ref[IO, Option[String]] = Ref.unsafe(None)

  /** Get the currently active theme */
  def getCurrentTheme: IO[Option[Theme]] =
    currentThemeRef.get

  /** Set the active theme */
  def setCurrentTheme(theme: Theme, themeName: String): IO[Unit] =
    for
      _ <- currentThemeRef.set(Some(theme))
      _ <- currentThemeNameRef.set(Some(themeName))
    yield ()

  /** Load and set theme by name */
  def loadAndSetTheme(themeName: String): IO[Theme] =
    for
      theme <- reloader.reloadTheme(themeName)
      _     <- setCurrentTheme(theme, themeName)
    yield theme

  /** Reload the currently active theme (if any) */
  def reloadCurrentTheme: IO[Option[Theme]] =
    for
      currentName <- currentThemeNameRef.get
      result <- currentName match
        case Some(name) =>
          reloader
            .reloadTheme(name)
            .flatMap(theme => setCurrentTheme(theme, name).as(Some(theme)))
            .handleErrorWith(error => IO.println(s"Failed to reload theme '$name': ${error.getMessage}").as(None))
        case None => IO.pure(None)
    yield result

  /** Get the name of the currently active theme */
  def getCurrentThemeName: IO[Option[String]] =
    currentThemeNameRef.get
