package com.serenity.config

import scala.util.matching.Regex

private[config] object ConfigVersioning:

  private val versionLine: Regex = """(?m)^(\s*config\.version\s*[=:]\s*)\d+""".r

  /** The version a rewrite of the file leaves it at: never below what it was, so a newer file is not downgraded. */
  def writtenVersion(report: ConfigMigrationReport, target: ConfigVersion): ConfigVersion =
    if report.newerThanSupported then report.version else target

  /** The text with its `config.version` line set to `version`, which is how a whole-file rewrite keeps a newer file's
    * version rather than writing this Serenity's own over it.
    */
  def stamp(text: String, version: ConfigVersion): String =
    versionLine.replaceAllIn(text, found => Regex.quoteReplacement(found.group(1) + version.value))

  /** The line edit that brings an older (or unversioned) file up to `target` on the next save; none otherwise. */
  def stampChange(report: ConfigMigrationReport, target: ConfigVersion): List[ConfigTextPatch.Change] =
    List(
      ConfigTextPatch.Change("config.version", Set("config.version"), Some(target.value.toString))
    ).filter(_ => report.version.value < target.value)
