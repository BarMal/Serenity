package com.serenity.config

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path, Paths}

import scala.util.control.NonFatal

import com.serenity.io.AtomicFileWriter
import org.slf4j.LoggerFactory

/** Blocking load/save entry points kept for test convenience only.
  *
  * Production code exclusively drives config I/O through `ConfigManager.loadConfigIO`/
  * `loadConfigResultIO`/`saveConfigIO` on the Cats Effect blocking pool. These synchronous equivalents used to live on
  * `ConfigManager` itself but had no production callers (#1462); they are kept here, test-only, so the large existing
  * body of specs can drive load/save without threading `IO` through every call site. Behaviour is unchanged from the
  * removed originals.
  */
object ConfigManagerTestSupport:
  private val logger = LoggerFactory.getLogger("com.serenity.config.ConfigManagerTestSupport")

  /** Load configuration from file or return default */
  def loadConfig(configPath: Option[String] = None): AppConfig =
    loadConfigResult(configPath).config

  /** Load configuration from file with migration/deprecation report or return defaults. */
  def loadConfigResult(configPath: Option[String] = None): ConfigLoadResult =
    val path = configPath.map(Paths.get(_)).getOrElse(ConfigManager.defaultConfigPath)
    if Files.exists(path) then
      try ConfigManager.parseConfigResult(path)
      catch
        case NonFatal(error) =>
          logger.error(s"[CONFIG] Failed to load config from $path, using defaults", error)
          ConfigLoadResult(AppConfig.default, ConfigMigrationReport.empty)
    else ConfigLoadResult(AppConfig.default, ConfigMigrationReport.empty)

  /** Save configuration to file */
  def saveConfig(config: AppConfig, configPath: String): Boolean =
    saveConfig(config, Paths.get(configPath))

  def saveConfig(config: AppConfig, configPath: Path): Boolean =
    ConfigManager.renderedConfig(config) match
      case Left(problem) =>
        logger.error(s"[CONFIG] Failed to save config to $configPath: $problem")
        false
      case Right(text) =>
        try
          AtomicFileWriter.writeBytesBlocking(configPath, text.getBytes(StandardCharsets.UTF_8))
          true
        catch
          case NonFatal(error) =>
            logger.error(s"[CONFIG] Failed to save config to $configPath", error)
            false
