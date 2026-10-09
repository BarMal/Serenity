package com.serenity.config

import java.nio.file.Path

import scala.util.Try

import com.typesafe.config.{Config, ConfigValueFactory}

/** The on-disk config file format's schema version (`config.version`), distinct from
  * [[com.serenity.session.SessionState.SchemaVersion]] -- a different persisted format with its own version history.
  */
opaque type ConfigVersion = Int

object ConfigVersion:
  val Current: ConfigVersion = ConfigVersion(2)

  def apply(value: Int): ConfigVersion = value

  extension (version: ConfigVersion) def value: Int = version

/** How a config file's `config.version` compares with the version this Serenity writes. */
enum ConfigVersionStatus:
  case Missing
  case Current
  case Older(found: ConfigVersion)
  case Newer(found: ConfigVersion)

object ConfigVersionStatus:

  /** A file with no usable `config.version` predates versioning, which is the same format as version 1. */
  val legacy: ConfigVersion = ConfigVersion(1)

  def declared(source: Config): Option[ConfigVersion] =
    List("config.version", "\"config.version\"")
      .flatMap(path => Try(source.getInt(path)).toOption)
      .find(_ > 0)
      .map(ConfigVersion.apply)

  def classify(source: Config, supported: ConfigVersion = ConfigVersion.Current): ConfigVersionStatus =
    declared(source).fold(Missing) { found =>
      if found.value < supported.value then Older(found)
      else if found.value > supported.value then Newer(found)
      else Current
    }

/** Something a migration step noticed in the file it was upgrading, worth telling the user. */
final case class MigrationNote(from: ConfigVersion, message: String)

final case class DeprecatedConfigEntry(
    key: String,
    replacement: String
)

final case class InvalidConfigEntry(
    key: String,
    value: String,
    reason: String
)

final case class ConfigMigrationReport(
    version: ConfigVersion,
    deprecatedEntries: List[DeprecatedConfigEntry] = Nil,
    unknownKeys: List[String] = Nil,
    invalidEntries: List[InvalidConfigEntry] = Nil,
    removedKeys: List[String] = Nil,
    hotkeyConflicts: List[HotkeyConflict] = Nil,
    migratedFrom: Option[ConfigVersion] = None,
    versionStatus: ConfigVersionStatus = ConfigVersionStatus.Current,
    supportedVersion: ConfigVersion = ConfigVersion.Current,
    migrationNotes: List[MigrationNote] = Nil
):
  def newerThanSupported: Boolean = version.value > supportedVersion.value

  def hasWarnings: Boolean =
    deprecatedEntries.nonEmpty || unknownKeys.nonEmpty || invalidEntries.nonEmpty || removedKeys.nonEmpty ||
      hotkeyConflicts.nonEmpty || newerThanSupported || migrationNotes.nonEmpty

  /** Whether rewriting the file would drop something the user wrote that is still in it. */
  def needsBackupBeforeRewrite: Boolean =
    invalidEntries.nonEmpty || hotkeyConflicts.nonEmpty || newerThanSupported

  def diagnostics: List[ConfigDiagnostic] =
    invalidEntries.map(entry => ConfigDiagnostic.InvalidValue(entry.key, entry.value, entry.reason)) ++
      hotkeyConflicts.map(ConfigDiagnostic.ConflictingHotkey.apply) ++
      Option.when(newerThanSupported)(ConfigDiagnostic.NewerFileVersion(version, supportedVersion)) ++
      migrationNotes.map(note => ConfigDiagnostic.MigrationWarning(note.from, note.message)) ++
      migratedFrom.map(from => ConfigDiagnostic.Migrated(from, supportedVersion)) ++
      deprecatedEntries.map(entry => ConfigDiagnostic.DeprecatedKey(entry.key, entry.replacement)) ++
      removedKeys.map(ConfigDiagnostic.RemovedKey.apply) ++
      unknownKeys.map(ConfigDiagnostic.UnknownKey.apply)

object ConfigMigrationReport:
  val empty: ConfigMigrationReport = ConfigMigrationReport(ConfigVersion.Current)

final case class ConfigLoadResult(
    config: AppConfig,
    report: ConfigMigrationReport
)

/** A structured configuration failure surfaced at the effectful IO boundary. */
final case class ConfigError(
    operation: String,
    path: Path,
    message: String,
    cause: Option[Throwable] = None
)

object ConfigMigrationWarning:

  def message(path: Path, report: ConfigMigrationReport): Option[String] =
    Option.when(report.hasWarnings) {
      val deprecated =
        section(
          "Deprecated entries:",
          report.deprecatedEntries.map(entry => s"- ${entry.key} -> use ${entry.replacement}")
        )
      val unknown =
        section(
          "Unknown entries:",
          report.unknownKeys.map(key => s"- $key")
        )
      val removed =
        section(
          "Removed entries:",
          report.removedKeys.map(key => s"- $key (setting removed; ignored)")
        )
      val invalid =
        section(
          "Invalid entries:",
          report.invalidEntries.map(entry => s"- ${entry.key} = ${entry.value} (${entry.reason})")
        )

      val migration =
        section(
          "Migration notes:",
          report.migrationNotes.map(note => s"- from version ${note.from.value}: ${note.message}")
        )

      List(
        Some(s"[CONFIG] Deprecated config format detected in $path."),
        deprecated,
        unknown,
        removed,
        invalid,
        migration,
        Some("Suggested upgraded config can be generated by saving settings from Serenity.")
      ).flatten.mkString("\n")
    }

  private def section(title: String, lines: List[String]): Option[String] =
    Option.when(lines.nonEmpty)((title :: lines).mkString("\n"))

/** Upgrades a config file written by an older format version to the current one, before it is read.
  *
  * Each [[Step]] carries a file from `from` to `from + 1`. A file of nothing but `key = value` lines is read with each
  * dotted key as one quoted key (`"old.wrap"`), not as a path, so a step should look for both spellings. A change to
  * the format adds a step here rather than teaching the parser a second spelling.
  */
object ConfigMigrations:

  final case class Step(from: ConfigVersion, migrate: Config => Config, warnings: Config => List[String] = _ => Nil)

  final case class Outcome(
      config: Config,
      found: ConfigVersion,
      applied: List[ConfigVersion],
      notes: List[MigrationNote] = Nil
  )

  /** `osName` is the platform whose hotkey defaults the file is written against. */
  final case class Plan(steps: List[Step], target: ConfigVersion, osName: String = HotkeyOverrides.runningOs)

  val steps: List[Step] = List(droppingDefaultHotkeys(HotkeyOverrides.runningOs))

  val installed: Plan = Plan(steps, ConfigVersion.Current)

  /** Version 1 to 2: `config.conf` records only the hotkeys the user changed (see [[HotkeyMigration]]). */
  def droppingDefaultHotkeys(osName: String): Step =
    Step(ConfigVersion(1), HotkeyMigration.withoutDefaultBindings(osName))

  def versionOf(source: Config): ConfigVersion =
    ConfigVersionStatus.declared(source).getOrElse(ConfigVersionStatus.legacy)

  def migrate(source: Config, plan: Plan): Outcome = migrate(source, plan.steps, plan.target)

  def migrate(
    source: Config,
    available: List[Step] = steps,
    target: ConfigVersion = ConfigVersion.Current
  ): Outcome =
    val found = versionOf(source)
    val due = available
      .filter(step => step.from.value >= found.value && step.from.value < target.value)
      .sortBy(_.from.value)
    val (migrated, notes) = due.foldLeft((source, List.empty[MigrationNote])) {
      case ((current, noted), step) =>
        (step.migrate(current), noted ++ step.warnings(current).map(MigrationNote(step.from, _)))
    }
    val stamped =
      if due.isEmpty then migrated
      else migrated.withValue("config.version", ConfigValueFactory.fromAnyRef(target.value))
    Outcome(stamped, found, due.map(_.from), notes)
