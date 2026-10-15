package com.serenity.config

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}
import java.time.Instant

import scala.util.Try
import scala.util.control.NonFatal

import com.serenity.io.AtomicFileWriter

/** Removes chrome settings from the config file by deleting their lines, so the layout supplies them again.
  *
  * A line edit can only reach a setting written as a whole line of its own. Anything else -- a key inside a block, a
  * value spread over several lines -- stays where it is and is reported, rather than guessed at.
  */
private[config] object ConfigKeyRelease:

  def apply(path: Path, keys: Set[String], at: Instant): Either[ConfigError, Set[String]] =
    if keys.isEmpty || !Files.isRegularFile(path) then Right(Set.empty)
    else
      try
        val text    = Files.readString(path, StandardCharsets.UTF_8)
        val removal = keys.toList.sorted.map(key => ConfigTextPatch.Change(key, LayoutPreset.chromeKeys(key), None))
        val patched = ConfigTextPatch(text, removal).text
        remaining(path, patched) match
          case None => Left(failure(path, "the file would not parse once the settings were removed", None))
          case Some(stillSet) =>
            val stuck = keys.filter(key => LayoutPreset.chromeKeys(key).exists(stillSet.contains))
            if patched == text then Right(stuck)
            else replaced(path, patched, at).map(_ => stuck)
      catch case NonFatal(error) => Left(failure(path, Option(error.getMessage).getOrElse("unreadable"), Some(error)))

  private def replaced(path: Path, patched: String, at: Instant): Either[ConfigError, Unit] =
    ConfigManager.backUpConfig(path, at) match
      case None => Left(failure(path, s"could not back up $path first, so it was left untouched", None))
      case Some(_) =>
        Try(AtomicFileWriter.writeBytesBlocking(path, patched.getBytes(StandardCharsets.UTF_8))).toEither.left
          .map(error => failure(path, Option(error.getMessage).getOrElse("not written"), Some(error)))

  /** The keys a file with `text` would set, read from a sibling file so relative includes work as they will. */
  private def remaining(path: Path, text: String): Option[Set[String]] =
    Option(path.toAbsolutePath.getParent).flatMap { directory =>
      val probe = Files.createTempFile(directory, ".config-release", ".conf")
      try
        Files.writeString(probe, text, StandardCharsets.UTF_8)
        Try(
          ConfigManager.explicitKeys(
            ConfigMigrations.migrate(ConfigManager.parseHoconFile(probe), ConfigMigrations.installed).config
          )
        ).toOption
      finally Files.deleteIfExists(probe): Unit
    }

  private def failure(path: Path, problem: String, cause: Option[Throwable]): ConfigError =
    ConfigError("release", path, s"Failed to update configuration: $problem", cause)
