package com.serenity.session

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path, StandardCopyOption}

import cats.effect.IO
import cats.syntax.all.*
import com.serenity.io.AtomicFileWriter
import org.typelevel.log4cats.Logger

/** Applies one session-file-and-index change as a unit, writing each session payload once (#1912).
  *
  * Each payload is written durably to a `.staged` sibling of its session file. Only then is a small
  * [[PendingSessionWrite]] marker recorded, naming the staged files rather than embedding them; applying renames each
  * staged file over its target, removes the deletes and writes the index. A crash before the marker leaves the old,
  * consistent state (and at most a stray staged file); a crash after it leaves a marker that [[recover]] replays.
  * Renames, deletes and the index write are all idempotent, so replaying a partly applied change is safe.
  *
  * Write targets are validated up front and the whole commit is refused if any is unsafe; delete targets that resolve
  * to no safe path are silently skipped.
  */
final private[session] class SessionWriteJournal(
    pendingFile: Path,
    indexFile: Path,
    safeSessionPath: String => Option[Path],
    logger: Logger[IO]
):

  def commit(writes: Map[String, String], deletes: List[String], indexJson: String): IO[Unit] =
    for
      staged          <- resolveStagedWrites(writes)
      resolvedDeletes <- resolveSafePaths(deletes)
      _               <- staged.traverse_((stagedPath, _, json) => AtomicFileWriter.writeString(stagedPath, json))
      marker = PendingSessionWrite(deletes = deletes, indexJson = indexJson, staged = writes.keys.toList.sorted)
      _ <- AtomicFileWriter.writeString(pendingFile, SessionWriteJournal.compact(marker))
      _ <- applyChange(staged.map((stagedPath, target, _) => stagedPath -> target), Nil, resolvedDeletes, indexJson)
      _ <- IO.blocking(Files.deleteIfExists(pendingFile)).void
    yield ()

  /** Finish a change left behind by a crash between recording it and completing it.
    *
    * Best-effort: a marker that fails to decode is quarantined rather than raised, so a corrupt marker can never block
    * every future session operation.
    */
  def recover: IO[Unit] =
    IO.blocking(Files.exists(pendingFile)).flatMap {
      case false => IO.unit
      case true =>
        IO.blocking(new String(Files.readAllBytes(pendingFile), StandardCharsets.UTF_8))
          .flatMap(json => IO.fromEither(_root_.io.circe.parser.decode[PendingSessionWrite](json)))
          .flatMap(replay)
          .handleErrorWith(quarantine)
    }

  private def replay(pending: PendingSessionWrite): IO[Unit] =
    for
      renames <- pending.staged.traverse(name => IO.blocking((stagedPath(name), safeSessionPath(name)).tupled))
      // Markers written before #1912 embed the payloads themselves.
      legacyWrites <- pending.writes.toList.traverse((name, json) => IO.blocking(safeSessionPath(name).map(_ -> json)))
      deletes      <- resolveSafePaths(pending.deletes)
      _            <- applyChange(renames.flatten, legacyWrites.flatten, deletes, pending.indexJson)
      _            <- IO.blocking(Files.deleteIfExists(pendingFile)).void
      _            <- logger.info(s"[SESSION] Replayed an interrupted session write from $pendingFile")
    yield ()

  // A staged file already gone was renamed into place before the interruption.
  private def applyChange(
    renames: List[(Path, Path)],
    writes: List[(Path, String)],
    deletes: List[Path],
    indexJson: String
  ): IO[Unit] =
    for
      _ <- renames.traverse_((staged, target) =>
        IO.blocking(Files.exists(staged)).ifM(AtomicFileWriter.replaceWith(staged, target), IO.unit)
      )
      _ <- writes.traverse_((path, json) => AtomicFileWriter.writeString(path, json))
      _ <- deletes.traverse_(deletePathQuietly)
      _ <- AtomicFileWriter.writeString(indexFile, indexJson)
    yield ()

  private def stagedPath(sessionFileName: String): Option[Path] =
    safeSessionPath(s"$sessionFileName.staged")

  private def resolveStagedWrites(writes: Map[String, String]): IO[List[(Path, Path, String)]] =
    writes.toList.traverse { (sessionFileName, json) =>
      IO.blocking((stagedPath(sessionFileName), safeSessionPath(sessionFileName)).tupled).flatMap {
        case Some((staged, target)) => IO.pure((staged, target, json))
        case None => IO.raiseError(new IllegalArgumentException(s"Unsafe session path: $sessionFileName"))
      }
    }

  private def resolveSafePaths(sessionFileNames: List[String]): IO[List[Path]] =
    sessionFileNames.traverse(name => IO.blocking(safeSessionPath(name))).map(_.flatten)

  private def deletePathQuietly(path: Path): IO[Unit] =
    IO.blocking(Files.deleteIfExists(path))
      .void
      .handleErrorWith(error => logger.error(error)(s"[SESSION] Failed to delete session file $path"))

  private def quarantine(error: Throwable): IO[Unit] =
    quarantinePendingFile.attempt.flatMap {
      case Right(Some(quarantineFile)) =>
        logger.error(error)(
          s"[SESSION] Failed to replay pending session write at $pendingFile; copied to $quarantineFile"
        )
      case Right(None) =>
        logger.error(error)(
          s"[SESSION] Failed to replay pending session write at $pendingFile; no file was available to copy"
        )
      case Left(quarantineError) =>
        logger.error(quarantineError)(s"[SESSION] Failed to copy corrupt pending session write at $pendingFile") >>
          logger.error(error)(s"[SESSION] Failed to replay pending session write at $pendingFile")
    }

  private def quarantinePendingFile: IO[Option[Path]] =
    IO.blocking(Files.exists(pendingFile)).flatMap {
      case false => IO.pure(None)
      case true =>
        IO.realTime.flatMap { now =>
          val quarantineFile = pendingFile.resolveSibling(s"${pendingFile.getFileName}.corrupt-${now.toMillis}")
          IO.blocking(Files.copy(pendingFile, quarantineFile, StandardCopyOption.REPLACE_EXISTING)) >>
            IO.blocking(Files.deleteIfExists(pendingFile)).as(Some(quarantineFile))
        }
    }

private[session] object SessionWriteJournal:

  def compact[A : _root_.io.circe.Encoder](value: A): String =
    _root_.io.circe.syntax.EncoderOps(value).asJson.noSpaces
