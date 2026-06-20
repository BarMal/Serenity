package com.serenity.config

case class ConfigVersion(value: Int) extends AnyVal:
  override def toString: String = value.toString

object ConfigVersion:
  val Current: ConfigVersion = ConfigVersion(1)

case class DeprecatedConfigEntry(
    key: String,
    replacement: String
)

case class InvalidConfigEntry(
    key: String,
    value: String,
    reason: String
)

case class ConfigMigrationReport(
    version: ConfigVersion,
    deprecatedEntries: List[DeprecatedConfigEntry] = Nil,
    unknownKeys: List[String] = Nil,
    invalidEntries: List[InvalidConfigEntry] = Nil,
    defaultedKeys: List[String] = Nil
):
  def hasWarnings: Boolean =
    deprecatedEntries.nonEmpty || unknownKeys.nonEmpty || invalidEntries.nonEmpty

object ConfigMigrationReport:
  val empty: ConfigMigrationReport = ConfigMigrationReport(ConfigVersion.Current)

case class ConfigLoadResult(
    config: AppConfig,
    report: ConfigMigrationReport
)
