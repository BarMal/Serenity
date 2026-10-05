package com.serenity.session

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path, StandardOpenOption}

import cats.effect.IO
import cats.syntax.all.*
import org.typelevel.log4cats.Logger

/** Moves a session that could not be restored out of the way of the next save, unchanged, and writes the unsaved text
  * it still holds next to it as plain-text files.
  */
object SessionSetAside:

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
          _ <- IO.blocking(Files.move(sessionFile, backup))
          recovered <- exportTexts(backup, SessionSalvage.salvage(json)).handleErrorWith(error =>
            logger.error(error)(s"[SESSION] Could not export unsaved text from $backup").as(Nil)
          )
        yield Some(UnreadableSession(reason, backup, recovered))
    }

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
