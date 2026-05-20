package com.serenity.ui.theme.config

import cats.effect.IO

/** Registry for discovering and accessing available themes at runtime */
object ThemeRegistry:

  private val manager = new ConfigurableThemeManager(new ThemeConfigLoader())

  /** Get all available theme names (internal + user + bundled) without file extensions */
  def getAvailableThemeNames: IO[List[String]] =
    manager.listAvailableThemes

  /** Check if a theme exists by name */
  def themeExists(themeName: String): IO[Boolean] =
    getAvailableThemeNames.map(_.contains(themeName))

  /** Get theme names grouped by source */
  def getThemesBySource: IO[ThemesBySource] =
    for
      userThemes <- new ThemeConfigLoader().listAvailableThemes(new ThemeConfigLoader().getUserThemesDirectory)
        .map(_.map(_.getFileName.toString.stripSuffix(".conf")))
      bundledThemes <- new ThemeConfigLoader().listBundledThemes  
      internalThemes = com.serenity.ui.theme.DefaultThemes.allInternal.keys.toList
    yield ThemesBySource(
      internal = internalThemes.sorted,
      user = userThemes.sorted,
      bundled = bundledThemes.sorted
    )

  /** Print available themes to console (useful for debugging/CLI) */
  def printAvailableThemes: IO[Unit] =
    for
      themes <- getThemesBySource
      _ <- IO.println("Available themes:")
      _ <- if themes.internal.nonEmpty then
        IO.println(s"  Internal: ${themes.internal.mkString(", ")}")
      else IO.unit
      _ <- if themes.user.nonEmpty then
        IO.println(s"  User: ${themes.user.mkString(", ")}")
      else IO.unit
      _ <- if themes.bundled.nonEmpty then
        IO.println(s"  Bundled: ${themes.bundled.mkString(", ")}")
      else IO.unit
    yield ()

case class ThemesBySource(
  internal: List[String],
  user: List[String], 
  bundled: List[String]
):
  def all: List[String] = (internal ++ user ++ bundled).distinct.sorted