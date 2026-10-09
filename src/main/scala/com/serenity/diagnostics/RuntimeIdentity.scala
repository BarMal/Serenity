package com.serenity.diagnostics

import com.serenity.BuildInfo

/** What a report must say about the build and machine it came from, so a log or crash file stands on its own. */
final case class RuntimeIdentity(version: String, commit: String, os: String, jvm: String, toolkit: String):

  def summary: String =
    s"Serenity $version ($commit) on $os, $jvm, toolkit $toolkit"

  def lines: List[String] =
    List(s"Version: $version", s"Commit: $commit", s"OS: $os", s"JVM: $jvm", s"Toolkit: $toolkit")

object RuntimeIdentity:

  /** Set at launch, once the toolkit has been chosen, so later reports can name it without being handed it. */
  val ToolkitProperty: String = "serenity.toolkit"

  private val Unknown = "unknown"

  def of(version: String, commit: String, property: String => Option[String]): RuntimeIdentity =
    def value(key: String): String = property(key).filter(_.nonEmpty).getOrElse(Unknown)
    RuntimeIdentity(
      version = version,
      commit = commit,
      os = s"${value("os.name")} ${value("os.version")} (${value("os.arch")})",
      jvm = s"${value("java.vm.name")} ${value("java.runtime.version")} (${value("java.vendor")})",
      toolkit = value(ToolkitProperty)
    )

  def current: RuntimeIdentity =
    of(BuildInfo.version, BuildInfo.commit, key => Option(System.getProperty(key)))
