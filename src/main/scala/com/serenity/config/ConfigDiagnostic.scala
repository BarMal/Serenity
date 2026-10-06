package com.serenity.config

/** A trigger two settings both claimed: [[keptBy]] holds it, and it was taken away from [[droppedFrom]]. */
final case class HotkeyConflict(trigger: String, keptBy: String, droppedFrom: List[String])

object HotkeyConflict:

  /** The config with every shared trigger left to its first holder -- actions before commands, each in key order -- and
    * the conflicts that were settled that way. One clashing pair in a hand-edited file would otherwise cost the user
    * every hotkey they had set.
    */
  def settle(config: HotkeyConfig): (HotkeyConfig, List[HotkeyConflict]) =
    val targets = HotkeyConfig.actionTargets(config.bindings).sortBy(_._1) ++
      config.commandBindings.toList.sortBy(_._1).map((commandId, triggers) => s"command.$commandId" -> triggers)
    val conflicts = targets
      .flatMap((target, triggers) => triggers.distinct.map(_ -> target))
      .groupMap(_._1)(_._2)
      .collect { case (trigger, keeper :: others) if others.nonEmpty => HotkeyConflict(trigger.render, keeper, others) }
      .toList
      .sortBy(_.trigger)
    val lostBy = conflicts
      .flatMap(conflict => conflict.droppedFrom.map(_ -> conflict.trigger))
      .groupMap(_._1)(_._2)
      .view
      .mapValues(_.toSet)
      .toMap
    def settled(target: String, triggers: List[HotkeyTrigger]): List[HotkeyTrigger] =
      triggers.filterNot(trigger => lostBy.getOrElse(target, Set.empty).contains(trigger.render))
    val resolved = config.copy(
      bindings = config.bindings.map((action, triggers) => action -> settled(action.configKey, triggers)),
      commandBindings =
        config.commandBindings.map((commandId, triggers) => commandId -> settled(s"command.$commandId", triggers))
    )
    (resolved, conflicts)

/** One thing worth telling the user about the config file they loaded.
  *
  * Typed rather than pre-formatted so that whatever surfaces them -- the start page notice today, a corner notice later
  * -- can choose its own wording and decide which are worth interrupting for ([[needsAttention]]).
  */
enum ConfigDiagnostic:
  case InvalidValue(key: String, value: String, reason: String)
  case ConflictingHotkey(conflict: HotkeyConflict)
  case NewerFileVersion(found: ConfigVersion, supported: ConfigVersion)
  case Migrated(from: ConfigVersion, to: ConfigVersion)
  case DeprecatedKey(key: String, replacement: String)
  case RemovedKey(key: String)
  case UnknownKey(key: String)

  /** Whether the user lost something they asked for, as opposed to housekeeping the log is enough for. */
  def needsAttention: Boolean = this match
    case _: InvalidValue | _: ConflictingHotkey | _: NewerFileVersion => true
    case _                                                            => false

  def message: String = this match
    case InvalidValue(key, value, reason) => s"$key = $value: $reason"
    case ConflictingHotkey(conflict) =>
      s"hotkey ${conflict.trigger} is bound to ${conflict.keptBy} and ${conflict.droppedFrom.mkString(", ")}; " +
        s"${conflict.keptBy} keeps it"
    case NewerFileVersion(found, supported) =>
      s"config.version = ${found.value} is newer than this Serenity understands (${supported.value}); " +
        "settings it does not recognise are kept as they are"
    case Migrated(from, to)              => s"config upgraded from version ${from.value} to ${to.value}"
    case DeprecatedKey(key, replacement) => s"$key is deprecated, use $replacement"
    case RemovedKey(key)                 => s"$key is no longer a setting and is ignored"
    case UnknownKey(key)                 => s"$key is not a setting Serenity recognises"

object ConfigNotice:

  private val listed = 3

  /** What the start page says about a load, if anything in it cost the user a setting. */
  def forLoad(path: java.nio.file.Path, report: ConfigMigrationReport): Option[String] =
    val attention = report.diagnostics.filter(_.needsAttention)
    Option.when(attention.nonEmpty) {
      val shown = attention.take(listed).map(_.message).mkString("; ")
      val more  = Option.when(attention.size > listed)(s"; and ${attention.size - listed} more").getOrElse("")
      s"${path.getFileName}: ${attention.size} problem(s), the rest of your settings were kept. $shown$more. " +
        "Your file is untouched; Serenity makes a timestamped backup before it next saves."
    }

  /** The load to run the session on, and the notice to show for it: defaults when the file could not be parsed. */
  def forOutcome(
    path: java.nio.file.Path,
    outcome: Either[ConfigError, ConfigLoadResult]
  ): (ConfigLoadResult, Option[String]) =
    outcome.fold(
      error => (ConfigLoadResult(AppConfig.default, ConfigMigrationReport.empty), Some(unparseable(path, error))),
      load => (load, forLoad(path, load.report))
    )

  private def unparseable(path: java.nio.file.Path, error: ConfigError): String =
    s"${path.getFileName} could not be parsed (${error.cause.map(_.getMessage).getOrElse(error.message)}), so this " +
      "session is using defaults. Your file is untouched and settings changes will not be saved until it is fixed."
