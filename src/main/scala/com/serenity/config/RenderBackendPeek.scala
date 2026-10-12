package com.serenity.config

import java.nio.file.{Files, Path}
import java.util.Locale

import scala.jdk.CollectionConverters.*
import scala.util.control.NonFatal

import cats.effect.IO

/** Reads `ui.render.backend` alone, because the renderer is chosen before the full config is loaded: the choice can
  * decide the toolkit, which is fixed before AWT starts. It logs nothing, so it can run ahead of log routing.
  */
object RenderBackendPeek:

  private val SettingKey = "ui.render.backend"

  def read(path: Path = ConfigManager.defaultConfigPath): IO[RenderBackendSetting] =
    IO.blocking {
      if Files.exists(path) then
        try settingIn(path)
        catch case NonFatal(_) => RenderBackendSetting.Auto
      else RenderBackendSetting.Auto
    }

  private def settingIn(path: Path): RenderBackendSetting =
    ConfigManager
      .parseHoconFile(path)
      .entrySet()
      .asScala
      .toList
      .flatMap { entry =>
        val key = entry.getKey.stripPrefix("\"").stripSuffix("\"").toLowerCase(Locale.ROOT)
        ConfigRegistry
          .find(key)
          .filter(_.key == SettingKey)
          .flatMap(_.readValue(AppConfig.default, entry.getValue))
          .map(_.surfaceConfig.renderBackend)
      }
      .headOption
      .getOrElse(RenderBackendSetting.Auto)
