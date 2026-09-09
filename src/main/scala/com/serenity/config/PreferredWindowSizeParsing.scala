package com.serenity.config

import scala.jdk.CollectionConverters.*
import scala.util.Try

import com.typesafe.config.Config

/** `window.preferred.width`/`.height` are a pair read one key at a time: applied through the registry alone, a config
  * file that states both would have the second key overwrite whichever placeholder the first fabricated (the #1316 bug)
  * or, once that fabrication is removed, never establish a size at all, since neither key alone may invent the other. A
  * file that states both means exactly that -- so this reads both raw values directly and sets them together, the way
  * `ConfigManager.applyHoconLists` already does for the settings a single key can't carry alone. A file that states
  * only one still falls through to the registry's own (now side-effect-free) per-key handling.
  */
private[config] object PreferredWindowSizeParsing:

  def applied(config: AppConfig, source: Config): AppConfig =
    def intAt(spellings: Set[String]): Option[Int] =
      source
        .entrySet()
        .asScala
        .find(entry => spellings.contains(entry.getKey.stripPrefix("\"").stripSuffix("\"")))
        .flatMap(entry => Try(source.getInt(entry.getKey)).toOption)

    val widthSpellings  = ConfigRegistry.find("window.preferred.width").map(_.spellings).getOrElse(Set.empty)
    val heightSpellings = ConfigRegistry.find("window.preferred.height").map(_.spellings).getOrElse(Set.empty)

    (intAt(widthSpellings), intAt(heightSpellings)) match
      case (Some(width), Some(height)) => config.withPreferredWindowSize(PreferredWindowSize(width, height))
      case _                           => config
