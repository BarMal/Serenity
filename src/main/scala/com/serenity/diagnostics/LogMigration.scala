package com.serenity.diagnostics

import java.nio.file.{Files, Path, StandardCopyOption}

import scala.jdk.CollectionConverters.*
import scala.util.Using

import cats.effect.IO
import cats.syntax.all.*

/** Brings the logs of earlier versions across from `~/.serenity` to the platform's log directory. Only log files move:
  * the settings and session in the old folder stay where they are.
  */
object LogMigration:

  /** The old `serenity.log` cannot keep its name: the new directory's own `serenity.log` is being written. */
  val MigratedLogName: String = "serenity.previous.log"

  private val ArchiveName = """serenity\.\d{4}-\d{2}-\d{2}\.\d+\.log\.gz""".r

  /** Moves, never overwrites, and never fails: a log that cannot be moved is left where it was. */
  def migrate(legacy: Path, target: Path): IO[List[Path]] =
    IO.blocking(Option.when(Files.isDirectory(legacy) && !isSame(legacy, target))(candidates(legacy)))
      .flatMap {
        case None => IO.pure(Nil)
        case Some(files) =>
          IO.blocking(Files.createDirectories(target)).attempt.flatMap {
            case Left(_) => IO.pure(Nil)
            case Right(_) =>
              files.flatTraverse {
                case (source, name) =>
                  moveIfAbsent(source, target.resolve(name)).map(_.toList)
              }
          }
      }
      .handleError(_ => Nil)

  private def isSame(first: Path, second: Path): Boolean =
    first.toAbsolutePath.normalize == second.toAbsolutePath.normalize

  private def candidates(legacy: Path): List[(Path, String)] =
    Using.resource(Files.list(legacy))(_.iterator().asScala.toList).flatMap { file =>
      val name = file.getFileName.toString
      if Files.isRegularFile(file) && name == "serenity.log" then Some(file -> MigratedLogName)
      else if Files.isRegularFile(file) && ArchiveName.matches(name) then Some(file -> name)
      else None
    }

  private def moveIfAbsent(source: Path, destination: Path): IO[Option[Path]] =
    IO.blocking {
      if Files.exists(destination) then None
      else Some(Files.move(source, destination, StandardCopyOption.ATOMIC_MOVE))
    }.handleError(_ => None)
