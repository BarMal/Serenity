package com.serenity.config

/** Spellings that are read but never written, because they do not correspond to one setting.
  *
  * `font.size` sets the code and the text size together; `font.ligatures` does the same for both ligature switches.
  * They stay readable so a config written before the settings were split apart still means what it meant, but nothing
  * can write them back -- which is why they are here rather than in [[ConfigRegistry]].
  */
object ConfigLegacyKeys:

  private def bothFontSizes(config: AppConfig, text: String): Option[AppConfig] =
    text.trim.toFloatOption.map { size =>
      val clamped = size.max(8.0f).min(48.0f)
      config.withFontConfig(config.editorConfig.fontConfig.copy(fontSize = clamped, textFontSize = clamped))
    }

  private def bothLigatures(config: AppConfig, text: String): Option[AppConfig] =
    FieldCodec
      .parseBoolean(text)
      .map(enabled =>
        config.withFontConfig(config.editorConfig.fontConfig.copy(enableLigatures = enabled, textLigatures = enabled))
      )

  val keys: List[LegacyConfigKey] = List(
    LegacyConfigKey(Set("font.size", "font_size"), bothFontSizes),
    LegacyConfigKey(Set("font.ligatures", "font_ligatures"), bothLigatures)
  )

  /** What to tell someone still using one of these. There is no single key to point at, so both are named. */
  val replacements: Map[String, String] = Map(
    "font.size"      -> "font.code.size and font.text.size",
    "font_size"      -> "font.code.size and font.text.size",
    "font.ligatures" -> "font.code.ligatures and font.text.ligatures",
    "font_ligatures" -> "font.code.ligatures and font.text.ligatures"
  )

  private val byKey: Map[String, LegacyConfigKey] =
    keys.flatMap(legacy => legacy.spellings.map(_ -> legacy)).toMap

  def find(key: String): Option[LegacyConfigKey] = byKey.get(key)

  def handles(key: String): Boolean = byKey.contains(key)
