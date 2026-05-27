package com.serenity.ui.theme.config

import java.nio.file.{Files, Path, Paths}

import cats.effect.IO
import pureconfig.*

class ThemeConfigLoader:

  /** Load theme configuration from a file path */
  def loadThemeFromFile(path: Path): IO[ThemeConfig] =
    IO.blocking {
      if !Files.exists(path) then throw new RuntimeException(s"Theme file not found: $path")

      val configSource = ConfigSource.file(path).at("theme")
      configSource.load[ThemeConfig] match
        case Right(config)  => config
        case Left(failures) => throw new RuntimeException(s"Failed to load theme config: ${failures.prettyPrint()}")
    }

  /** Load theme configuration from classpath resource */
  def loadThemeFromResource(resourcePath: String): IO[ThemeConfig] =
    IO.blocking {
      val configSource = ConfigSource.resources(resourcePath).at("theme")
      configSource.load[ThemeConfig] match
        case Right(config) => config
        case Left(failures) =>
          throw new RuntimeException(s"Failed to load theme config from resource: ${failures.prettyPrint()}")
    }

  /** Load theme configuration from string (useful for testing) */
  def loadThemeFromString(configString: String): IO[ThemeConfig] =
    IO.blocking {
      val configSource = ConfigSource.string(configString).at("theme")
      configSource.load[ThemeConfig] match
        case Right(config)  => config
        case Left(failures) => throw new RuntimeException(s"Failed to parse theme config: ${failures.prettyPrint()}")
    }

  /** List available theme files in a directory */
  def listAvailableThemes(themesDir: Path): IO[List[Path]] =
    IO.blocking {
      if !Files.exists(themesDir) || !Files.isDirectory(themesDir) then List.empty
      else
        import scala.jdk.CollectionConverters.*
        Files
          .list(themesDir)
          .filter(path => path.toString.endsWith(".conf") || path.toString.endsWith(".hocon"))
          .toList
          .asScala
          .toList
          .sorted
    }

  /** Get default themes directory in resources */
  def getDefaultThemesResourcePath: String = "themes"

  /** Get user themes directory */
  def getUserThemesDirectory: Path =
    val homeDir = System.getProperty("user.home")
    Paths.get(homeDir, ".serenity", "themes")

  /** Ensure user themes directory exists */
  def ensureUserThemesDirectory: IO[Path] =
    IO.blocking {
      val userThemesDir = getUserThemesDirectory
      if !Files.exists(userThemesDir) then Files.createDirectories(userThemesDir)
      userThemesDir
    }

  /** List bundled theme files from resources */
  def listBundledThemes: IO[List[String]] =
    IO.blocking {
      // Try to read resource directory - this is a simple implementation
      // In production, you might want to use a more robust resource scanning approach
      val resourceUrl = getClass.getClassLoader.getResource(getDefaultThemesResourcePath)
      if resourceUrl != null then
        try
          val resourcePath = Paths.get(resourceUrl.toURI)
          if Files.exists(resourcePath) && Files.isDirectory(resourcePath) then
            import scala.jdk.CollectionConverters.*
            Files
              .list(resourcePath)
              .filter(path => path.toString.endsWith(".conf"))
              .map(_.getFileName.toString.stripSuffix(".conf"))
              .toList
              .asScala
              .toList
              .sorted
          else List.empty
        catch
          case _: Exception =>
            // If we can't read from resources (e.g., in JAR), return known themes
            List("dark", "light")
      else List.empty
    }
