package com.serenity.diagnostics

import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, LinkOption, Path, StandardCopyOption}
import java.time.Instant

import scala.util.Try

import cats.effect.IO

/** How the previous run ended, as far as the files it left show. */
enum PreviousRun:
  case Clean

  /** @param report
    *   The crash file's text, or one built from what is known when nothing was recorded.
    */
  case Abnormal(report: String)

object PreviousRun:

  def decide(
    runningMarker: Option[String],
    crashFile: Option[String],
    identity: RuntimeIdentity,
    at: Instant,
    logDirectory: Path
  ): PreviousRun =
    crashFile
      .map(Abnormal(_))
      .orElse {
        runningMarker.map(build =>
          Abnormal(CrashReport.unrecordedExit(Some(build).filter(_.nonEmpty), identity, at, logDirectory))
        )
      }
      .getOrElse(Clean)

/** The two files that tell the next launch how this one ended. A running marker is written at launch and removed on a
  * clean exit, so one left behind means the process was killed or lost. A crash file is written when a failure is
  * caught, and outlives the shutdown that follows it. Every operation tolerates an unwritable directory: reporting a
  * crash must never be what stops the editor starting.
  */
final case class CrashRecord(directory: Path):

  val runningMarker: Path         = directory.resolve("running")
  val crashFile: Path             = directory.resolve("last-crash.txt")
  val acknowledgedCrashFile: Path = directory.resolve("previous-crash.txt")

  def markRunning(identity: RuntimeIdentity, at: Instant): IO[Unit] =
    IO.blocking {
      Files.createDirectories(directory)
      val _ = Files.writeString(runningMarker, s"${identity.summary} since $at")
    }.handleError(_ => ())

  def markCleanExit: IO[Unit] =
    IO.blocking(Files.deleteIfExists(runningMarker)).void.handleError(_ => ())

  def recordCrash(report: String): IO[Option[Path]] =
    IO.blocking(writeCrash(report))

  /** For an uncaught-exception handler, which has no `IO` to run in. */
  def writeCrash(report: String): Option[Path] =
    Try {
      Files.createDirectories(directory)
      Files.writeString(crashFile, report)
    }.toOption

  def previousRun(identity: RuntimeIdentity, at: Instant): IO[PreviousRun] =
    IO.blocking(
      PreviousRun.decide(read(runningMarker), read(crashFile), identity, at, directory)
    ).handleError(_ => PreviousRun.Clean)

  /** Called once the previous run's outcome is in hand: keeps the crash file for the record, under a name that is not
    * reported again, and clears the marker.
    */
  def acknowledge: IO[Unit] =
    IO.blocking {
      if Files.isRegularFile(crashFile, LinkOption.NOFOLLOW_LINKS) then
        val _ = Files.move(crashFile, acknowledgedCrashFile, StandardCopyOption.REPLACE_EXISTING)
      val _ = Files.deleteIfExists(runningMarker)
    }.handleError(_ => ())

  private def read(file: Path): Option[String] =
    Option.when(Files.isRegularFile(file, LinkOption.NOFOLLOW_LINKS))(new String(Files.readAllBytes(file), UTF_8))
