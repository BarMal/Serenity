package com.serenity.app

import java.nio.file.{Files, LinkOption, Path}
import java.time.LocalDateTime

import cats.effect.IO
import com.serenity.io.TimestampedBackup

/** `--reset-config` and `--reset-session`: the files are moved to timestamped backups, never deleted, so a reset can
  * always be undone by hand.
  */
object LaunchReset:

  final case class Moved(from: Path, to: Path)

  val ConfigFileName: String = "config.conf"

  /** Everything under the session root that makes up the saved session. Settings, presets and themes live beside it and
    * are left alone.
    */
  private val SessionEntries: List[String] = List("session-index.json", "session-write.pending.json", "sessions")

  def backupSuffix(at: LocalDateTime): String =
    TimestampedBackup.suffix(at)

  def backUpConfig(config: Path, at: LocalDateTime): IO[List[Moved]] =
    IO.blocking {
      if Files.exists(config, LinkOption.NOFOLLOW_LINKS) then
        val backup = TimestampedBackup.siblingOf(config, at)
        List(Moved(config, TimestampedBackup.moveAside(config, backup)))
      else Nil
    }

  def backUpSession(sessionRoot: Path, at: LocalDateTime): IO[List[Moved]] =
    IO.blocking {
      val present = SessionEntries.map(sessionRoot.resolve).filter(Files.exists(_, LinkOption.NOFOLLOW_LINKS))
      if present.isEmpty then Nil
      else
        val folder = Files.createDirectory(sessionRoot.resolve(s"session-${backupSuffix(at)}"))
        present.map(entry => Moved(entry, TimestampedBackup.moveAside(entry, folder.resolve(entry.getFileName))))
    }
