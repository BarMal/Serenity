package com.serenity.config

import java.nio.file.{Files, Path}
import java.util.Locale

import scala.jdk.CollectionConverters.*
import scala.util.control.NonFatal

import cats.effect.IO

/** The render settings that decide the toolkit and the renderer before any window exists. */
final case class RenderStartupSettings(backend: RenderBackendSetting, wayland: WaylandRender)

/** Reads the render startup settings alone, because they are needed before the full config is loaded: they can decide
  * the toolkit, which is fixed before AWT starts. It logs nothing, so it can run ahead of log routing.
  */
object RenderBackendPeek:

  private val SettingKeys = Set("ui.render.backend", "ui.render.wayland")

  private val Defaults = RenderStartupSettings(RenderBackendSetting.Auto, WaylandRender.SkiaRaster)

  def read(path: Path = ConfigManager.defaultConfigPath): IO[RenderStartupSettings] =
    IO.blocking {
      if Files.exists(path) then
        try settingsIn(path)
        catch case NonFatal(_) => Defaults
      else Defaults
    }

  private def settingsIn(path: Path): RenderStartupSettings =
    val applied = ConfigManager
      .parseHoconFile(path)
      .entrySet()
      .asScala
      .foldLeft(AppConfig.default) { (config, entry) =>
        val key = entry.getKey.stripPrefix("\"").stripSuffix("\"").toLowerCase(Locale.ROOT)
        ConfigRegistry
          .find(key)
          .filter(field => SettingKeys.contains(field.key))
          .flatMap(_.readValue(config, entry.getValue))
          .getOrElse(config)
      }
    RenderStartupSettings(applied.surfaceConfig.renderBackend, applied.surfaceConfig.renderWayland)
