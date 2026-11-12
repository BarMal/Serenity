package com.serenity.config

import java.util.Locale

import scala.jdk.CollectionConverters.*

import com.typesafe.config.{Config, ConfigValueFactory}

/** Version 2 files could choose Java2D as the renderer. Skia is the only renderer now, so the choice becomes the
  * setting's default: `auto` for the renderer, `skia-raster` for the Wayland render.
  */
private[config] object RenderBackendMigration:

  final private case class Setting(spellings: Set[String], replacement: String)

  private val settings = List(
    Setting(Set("ui.render.backend", "renderbackend", "render.backend", "render_backend"), "auto"),
    Setting(Set("ui.render.wayland", "renderwayland", "render.wayland", "render_wayland"), "skia-raster")
  )

  private val retiredValues = Set("java2d", "java")

  final private case class Retired(path: String, name: String, value: String, replacement: String)

  def withoutJava2D(config: Config): Config =
    retired(config).foldLeft(config)((kept, found) =>
      kept.withValue(found.path, ConfigValueFactory.fromAnyRef(found.replacement))
    )

  def notes(config: Config): List[String] =
    retired(config).map(found =>
      s"${found.name} = ${found.value} is no longer a choice, since Serenity paints with Skia only; using ${found.replacement}."
    )

  private def retired(config: Config): List[Retired] =
    for
      entry <- config.entrySet().asScala.toList
      name = entry.getKey.stripPrefix("\"").stripSuffix("\"")
      setting <- settings.find(_.spellings.contains(name.toLowerCase(Locale.ROOT)))
      value = entry.getValue.unwrapped().toString.trim
      if retiredValues.contains(value.toLowerCase(Locale.ROOT))
    yield Retired(entry.getKey, name, value, setting.replacement)
