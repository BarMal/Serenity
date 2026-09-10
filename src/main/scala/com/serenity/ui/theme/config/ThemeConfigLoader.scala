package com.serenity.ui.theme.config

import java.nio.file.{Files, Path, Paths}

import cats.effect.IO
import pureconfig.*
import pureconfig.error.ConfigReaderFailures

object ThemeConfigLoader:

  /** Fallback returned by `listBundledThemes` when the bundled `themes/` resource directory can't be read as a
    * filesystem path (e.g. running from a packaged JAR). Derived from `DefaultThemes.allInternal` so it can never
    * name a theme that isn't actually registered internally -- it previously hardcoded `List("dark", "light")`,
    * names that no longer match any internal theme key (issue #1429).
    */
  def bundledThemesFallback: List[String] =
    com.serenity.ui.theme.DefaultThemes.allInternal.keys.toList.sorted

class ThemeConfigLoader:

  /** Load theme configuration from a file path */
  def loadThemeFromFile(path: Path): IO[ThemeConfig] =
    for
      exists <- IO.blocking(Files.exists(path))
      _      <- IO.unlessA(exists)(IO.raiseError(new RuntimeException(s"Theme file not found: $path")))
      config <- loadThemeConfig(
        ConfigSource.file(path),
        failures => s"Failed to load theme config: ${failures.prettyPrint()}"
      )
    yield config

  /** Load theme configuration from classpath resource */
  def loadThemeFromResource(resourcePath: String): IO[ThemeConfig] =
    loadThemeConfig(
      ConfigSource.resources(resourcePath),
      failures => s"Failed to load theme config from resource: ${failures.prettyPrint()}"
    )

  /** Load theme configuration from string (useful for testing) */
  def loadThemeFromString(configString: String): IO[ThemeConfig] =
    loadThemeConfig(
      ConfigSource.string(configString),
      failures => s"Failed to parse theme config: ${failures.prettyPrint()}"
    )

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
      if !Files.exists(userThemesDir) then Files.createDirectories(userThemesDir): Unit
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
            // Can't read from resources (e.g., in JAR) -- fall back to the actual internal theme keys.
            ThemeConfigLoader.bundledThemesFallback
      else List.empty
    }

  private def loadThemeConfig(
    configSource: ConfigSource,
    errorMessage: ConfigReaderFailures => String
  ): IO[ThemeConfig] =
    IO.blocking(configSource.at("theme").load[ThemeConfig]).flatMap {
      case Right(config)  => IO.pure(config)
      case Left(failures) => IO.raiseError(new RuntimeException(errorMessage(failures)))
    }
