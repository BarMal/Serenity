package com.serenity.config

import com.serenity.config.FieldCodec.parseBoolean

/** `typography.prose.measure` and `editor.column_target_width`, folded into `editor.wrap_column`.
  *
  * A file that still names either is read as what it showed before: the measure (always in ch) wins unless column mode
  * is on, where the target (always in cells) is the column's width; each key is reported as renamed, and the one whose
  * value was not carried over says why. The target did nothing outside column mode -- the old default config names `80`
  * -- so carrying it over would cap every text area at 80 cells. An explicit `editor.wrap_column` wins over both.
  */
object LegacyWrapColumnKeys:

  private val MeasureKeys = Set("typography.prose.measure", "prose.measure")

  private val TargetKeys = Set(
    "editor.column_target_width",
    "display.column_target_width",
    "display.column.target.width",
    "display_column_target_width"
  )

  /** Every old key, and where its setting lives now. */
  val replacements: Map[String, String] = (MeasureKeys ++ TargetKeys).map(_ -> WrapColumnKey).toMap

  private val WrapColumnKey = "editor.wrap_column"

  /** The session's own spellings, from before the wrap column. */
  private val SessionMeasureKey = "typography.prose.measure"
  private val SessionTargetKey  = "columnTargetWidthCells"
  private val TargetKey         = "editor.column_target_width"

  def handles(key: String): Boolean = replacements.contains(key)

  /** Whether an old key's value is one the old setting would have refused, or the wrap column now does. */
  def rejects(key: String, value: String): Boolean = read(key, value).isEmpty

  private def read(key: String, value: String): Option[WrapColumn] =
    if MeasureKeys.contains(key) then
      if parseBoolean(value).contains(false) then Some(WrapColumn.Edge)
      else
        val text = value.trim.toLowerCase(java.util.Locale.ROOT)
        WrapColumn.parse(if text.forall(_.isDigit) then s"${text}ch" else text)
    else WrapColumn.parse(value)

  private case class Resolved(chosen: Option[(String, WrapColumn)], notCarried: Map[String, String])

  private def resolve(old: Map[String, String], columnMode: Boolean): Resolved =
    def valueOf(keys: Set[String]): Option[(String, WrapColumn)] =
      keys.toList.sorted.flatMap(key => old.get(key).flatMap(read(key, _)).map(key -> _)).headOption
    val measure = valueOf(MeasureKeys)
    val target  = valueOf(TargetKeys)
    val chosen  = if columnMode then target.orElse(measure) else measure
    val notCarried =
      (measure.toList ++ target.toList).collect {
        case (key, _) if !chosen.exists(_._1 == key) =>
          key -> (if TargetKeys.contains(key) && !columnMode then "it only applied in column mode"
                  else "the other key was used")
      }.toMap
    Resolved(chosen, notCarried)

  private def oldEntries(entries: List[(String, String)]): Map[String, String] =
    entries.filter((key, _) => handles(key)).toMap

  private val columnModeSpellings: Set[String] =
    ConfigRegistry.find("editor.column_mode").map(_.spellings).getOrElse(Set("editor.column_mode"))

  private val wrapColumnSpellings: Set[String] =
    ConfigRegistry.find(WrapColumnKey).map(_.spellings).getOrElse(Set(WrapColumnKey))

  private def columnModeIn(entries: List[(String, String)]): Boolean =
    entries
      .collectFirst { case (key, value) if columnModeSpellings.contains(key) => parseBoolean(value) }
      .flatten
      .getOrElse(false)

  private def setExplicitly(entries: List[(String, String)]): Boolean =
    entries.exists((key, _) => wrapColumnSpellings.contains(key))

  /** The config with the old keys' meaning in it, unless `editor.wrap_column` was given. */
  def applied(config: AppConfig, entries: List[(String, String)]): AppConfig =
    val old = oldEntries(entries)
    if old.isEmpty || setExplicitly(entries) then config
    else resolve(old, columnModeIn(entries)).chosen.fold(config)((_, wrapColumn) => config.withWrapColumn(wrapColumn))

  /** What to tell the user an old key became: the new key, and why its value was not carried over when it was not. */
  def replacementFor(key: String, entries: List[(String, String)]): Option[String] =
    Option.when(handles(key)) {
      val old = oldEntries(entries)
      if setExplicitly(entries) then s"$WrapColumnKey (not carried over: $WrapColumnKey is set)"
      else
        resolve(old, columnModeIn(entries)).notCarried
          .get(key)
          .fold(WrapColumnKey)(reason => s"$WrapColumnKey (not carried over: $reason)")
    }

  /** A session written before the wrap column carries the old settings under their own keys. */
  def appliedToSession(cursor: io.circe.HCursor, config: AppConfig): AppConfig =
    if cursor.downField(WrapColumnKey).succeeded then config
    else
      def stored(sessionKey: String, key: String, spell: Int => String, none: String): Option[(String, String)] =
        val field = cursor.downField(sessionKey)
        Option.when(field.succeeded) {
          key -> field.as[Option[Int]].toOption.flatten.fold(none)(spell)
        }
      val old = List(
        stored(SessionMeasureKey, SessionMeasureKey, count => s"${count}ch", "off"),
        stored(SessionTargetKey, TargetKey, _.toString, "unlimited")
      ).flatten.toMap
      resolve(old, config.surfaceConfig.columnModeEnabled).chosen
        .fold(config)((_, wrapColumn) => config.withWrapColumn(wrapColumn))
