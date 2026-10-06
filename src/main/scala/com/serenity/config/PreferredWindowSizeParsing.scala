package com.serenity.config

import scala.jdk.CollectionConverters.*

import com.typesafe.config.Config

/** `window.preferred.width`/`.height` are a pair read one key at a time: applied through the registry alone, a config
  * file that states both would have the second key overwrite whichever placeholder the first fabricated (the #1316 bug)
  * or, once that fabrication is removed, never establish a size at all, since neither key alone may invent the other. A
  * file that states both means exactly that -- so this reads both raw values directly and sets them together, the way
  * `ConfigManager.applyHoconLists` already does for the settings a single key can't carry alone. A file that states
  * only one still falls through to the registry's own (now side-effect-free) per-key handling.
  */
private[config] object PreferredWindowSizeParsing:

  /** What one of the two keys says: nothing, something unusable, or a size. */
  private enum Component:
    case Absent
    case Invalid
    case Valid(pixels: Int)

  /** A component that is stated but unusable (including an empty value or a block) falls back to the default window's,
    * so the other, valid one is kept. One that is not stated at all still means no preferred size, as above.
    */
  def applied(config: AppConfig, source: Config): AppConfig =
    def componentAt(spellings: Set[String]): Component =
      def keyOf(entry: java.util.Map.Entry[String, ?]): String = entry.getKey.stripPrefix("\"").stripSuffix("\"")
      val entries                                              = source.entrySet().asScala.toList
      entries.find(entry => spellings.contains(keyOf(entry))) match
        case Some(entry) =>
          FieldCodec.flatten(entry.getValue).trim.toIntOption.fold(Component.Invalid)(Component.Valid.apply)
        case None =>
          if entries.exists(entry => spellings.exists(spelling => keyOf(entry).startsWith(s"$spelling."))) then
            Component.Invalid
          else Component.Absent

    val widthSpellings  = ConfigRegistry.find("window.preferred.width").map(_.spellings).getOrElse(Set.empty)
    val heightSpellings = ConfigRegistry.find("window.preferred.height").map(_.spellings).getOrElse(Set.empty)

    (componentAt(widthSpellings), componentAt(heightSpellings)) match
      case (Component.Valid(width), Component.Valid(height)) =>
        config.withPreferredWindowSize(PreferredWindowSize(width, height))
      case (Component.Valid(width), Component.Invalid) =>
        config.withPreferredWindowSize(PreferredWindowSize(width, PreferredWindowSize.Default.height))
      case (Component.Invalid, Component.Valid(height)) =>
        config.withPreferredWindowSize(PreferredWindowSize(PreferredWindowSize.Default.width, height))
      case _ => config
