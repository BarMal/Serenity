package com.serenity.config

/** Every setting the config file persists, declared once.
  *
  * The file writer, the file parser, the key schema and session state all read this list rather than each carrying
  * their own copy of what a setting is called and how it converts. That is the point: a setting used to be written down
  * in three or four places, and the bugs were always a place that had been missed -- ten settings the parser knew and
  * the writer did not, sixteen the config file kept and session state dropped.
  *
  * The declarations themselves live in the per-domain `ConfigFields*` objects (one per config namespace family),
  * spelled with [[ConfigFieldSyntax]]; this object is the assembled list plus the lookups over it. Concatenation order
  * here is the order the config file is written in.
  *
  * Composite settings whose shape is not one key to one value -- the motion families, the animation presets, the
  * LSP/hotkey/keymap groups -- are declared in [[ConfigGroups]] instead, and the coverage tests treat both alike.
  */
object ConfigRegistry:

  val fields: List[ConfigField[?]] =
    ConfigFieldsLanguageAndTypography.fields ++
      ConfigFieldsCursorAndWindow.fields ++
      ConfigFieldsStatusLine.fields ++
      ConfigFieldsDocumentsAndCommandRunner.fields ++
      ConfigFieldsDisplay.fields ++
      ConfigFieldsSurface.fields

  private val byKey: Map[String, ConfigField[?]] =
    fields.flatMap(configField => configField.spellings.map(_ -> configField)).toMap

  def find(key: String): Option[ConfigField[?]] = byKey.get(key)

  /** Apply one key's value, as reading a config file does. `None` when the key is unknown or the value unusable. */
  def read(config: AppConfig, key: String, value: String): Option[AppConfig] =
    find(key).flatMap(_.read(config, value))

  /** Whether a registered key would reject this value. An unknown key is not this function's business, so it says no.
    */
  def rejects(key: String, value: String): Boolean =
    find(key).exists(_.codec.parse(value).isEmpty)

  val writtenKeys: List[String] = fields.map(_.key)

  val allKeys: Set[String] = byKey.keySet

  /** The order settings are applied in when a whole config is read at once: broader paths before narrower ones, then
    * alphabetically. It is the order the config file is folded in, and it matters because a handful of setters
    * deliberately adjust a neighbouring setting -- a custom blur radius switches the material preset to custom, and the
    * preset's own saved value has to come after that to have the last word.
    */
  /** Every setting's default value, in the order the file writes them.
    *
    * The literals still sit in the config case classes' constructors, but this is where to read them: one list, keyed
    * the way the config file is, rather than spread over parameter lists and an override block that restates some of
    * them. `docs/default-config.conf` is generated from it, so a change to any default shows up as a diff there.
    */
  lazy val defaults: List[(String, HoconValue)] = fields.map(_.setting(AppConfig.default))

  def defaultFor(key: String): Option[HoconValue] = find(key).map(_.setting(AppConfig.default)._2)

  /** Put one setting back the way it ships, leaving every other setting alone. */
  def resetToDefault(config: AppConfig, key: String): Option[AppConfig] =
    find(key).map(_.restoreDefault(config, AppConfig.default))

  val readOrder: List[ConfigField[?]] =
    fields.sortBy(field => (field.key.count(_ == '.'), field.key))
