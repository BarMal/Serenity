package com.serenity.diagnostics

import java.nio.file.Path

import scala.util.Try

enum Platform:
  case Windows, MacOs, Other

object Platform:

  def fromOsName(name: String): Platform =
    val lowered = name.toLowerCase
    if lowered.startsWith("windows") then Windows
    else if lowered.startsWith("mac") || lowered.startsWith("darwin") then MacOs
    else Other

  def current: Platform = fromOsName(Option(System.getProperty("os.name")).getOrElse(""))

/** Where logs and crash files live: the platform's conventional place for them, not a dotfile in the home directory. */
object LogLocation:

  /** Lets a launch, or a test, put the logs somewhere of its own choosing. */
  val OverrideProperty: String = "serenity.log.dir"

  def directory(platform: Platform, env: Map[String, String], home: Path): Path =
    platform match
      case Platform.Windows =>
        nonBlank(env, "LOCALAPPDATA")
          .flatMap(absolutePath)
          .getOrElse(home.resolve("AppData").resolve("Local"))
          .resolve("Serenity")
          .resolve("logs")
      case Platform.MacOs => home.resolve("Library").resolve("Logs").resolve("Serenity")
      case Platform.Other =>
        nonBlank(env, "XDG_STATE_HOME")
          .flatMap(absolutePath)
          .getOrElse(home.resolve(".local").resolve("state"))
          .resolve("serenity")

  def legacyDirectory(home: Path): Path = home.resolve(".serenity")

  def resolve(platform: Platform, env: Map[String, String], home: Path, property: String => Option[String]): Path =
    property(OverrideProperty).filter(_.trim.nonEmpty).flatMap(absolutePath).getOrElse(directory(platform, env, home))

  def current: Path =
    resolve(Platform.current, sys.env, userHome, key => Option(System.getProperty(key)))

  def currentLegacy: Path = legacyDirectory(userHome)

  private def userHome: Path = Path.of(System.getProperty("user.home"))

  private def nonBlank(env: Map[String, String], key: String): Option[String] =
    env.get(key).map(_.trim).filter(_.nonEmpty)

  private def absolutePath(text: String): Option[Path] =
    Try(Path.of(text)).toOption.filter(_.isAbsolute)
