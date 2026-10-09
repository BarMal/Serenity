package com.serenity.session

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path, StandardOpenOption}

import scala.util.Try

import cats.effect.IO
import cats.syntax.all.*
import org.typelevel.log4cats.Logger

/** Moves a session that could not be restored out of the way of the next save, unchanged, and writes the unsaved text
  * it still holds next to it as plain-text files.
  */
object SessionSetAside:

  /** The error a set-aside is logged with when the file is intact but written by a newer build. */
  case object NewerSchemaFile extends RuntimeException("session file was written by a newer build"):
    override def fillInStackTrace(): Throwable = this

  /** An unreadable file counts as not newer: it is the corrupt case, which startup alone deals with. */
  def isNewerSchema(sessionFile: Path): IO[Boolean] =
    IO.blocking(new String(Files.readAllBytes(sessionFile), StandardCharsets.UTF_8))
      .map(json => SessionSalvage.reason(json) != UnreadableReason.Corrupt)
      .handleError(_ => false)

  /** Text that fails to export is still in the backup, so that failure is logged rather than losing the notice. */
  def setAside(sessionFile: Path, epochMillis: Long, logger: Logger[IO]): IO[Option[UnreadableSession]] =
    IO.blocking(Files.exists(sessionFile)).flatMap {
      case false => IO.none
      case true =>
        for
          json <- IO.blocking(new String(Files.readAllBytes(sessionFile), StandardCharsets.UTF_8))
          reason = SessionSalvage.reason(json)
          backup = sessionFile.resolveSibling(
            SessionSalvage.backupFileName(sessionFile.getFileName.toString, reason, epochMillis)
          )
          _       <- IO.blocking(Files.move(sessionFile, backup))
          content <- keepContentWith(sessionFile, backup, logger)
          recovered <- exportTexts(backup, SessionSalvage.salvage(json, contentIn(content))).handleErrorWith(error =>
            logger.error(error)(s"[SESSION] Could not export unsaved text from $backup").as(Nil)
          )
        yield Some(UnreadableSession(reason, backup, recovered))
    }

  /** The session's content files (#1912) go with its backup: the next save would otherwise prune them from beside the
    * new session file, leaving the backup naming text that no longer exists.
    */
  private def keepContentWith(sessionFile: Path, backup: Path, logger: Logger[IO]): IO[Option[Path]] =
    val original = sessionFile.resolveSibling(SessionContentStore.directoryName(sessionFile.getFileName.toString))
    val kept     = backup.resolveSibling(SessionContentStore.directoryName(backup.getFileName.toString))
    IO.blocking(Files.isDirectory(original)).flatMap {
      case false => IO.none
      case true =>
        IO.blocking(Files.move(original, kept))
          .map(Option(_))
          .handleErrorWith(error =>
            logger.error(error)(s"[SESSION] Could not move content files $original beside $backup").as(None)
          )
    }

  private def contentIn(directory: Option[Path])(ref: String): Option[String] =
    directory.flatMap(dir =>
      Try(Files.readString(dir.resolve(SessionContentStore.contentFileName(ref)), StandardCharsets.UTF_8)).toOption
    )

  private def exportTexts(backup: Path, texts: List[SalvagedText]): IO[List[Path]] =
    if texts.isEmpty then IO.pure(Nil)
    else
      IO.blocking(Files.createDirectories(backup.resolveSibling(s"${backup.getFileName}.recovered"))).flatMap {
        directory =>
          texts.zipWithIndex.traverse { (salvaged, index) =>
            val target = directory.resolve(SessionSalvage.recoveredFileName(index, salvaged.label))
            IO.blocking(Files.writeString(target, salvaged.text, StandardCharsets.UTF_8, StandardOpenOption.CREATE_NEW))
          }
      }
