package com.serenity.app

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, LinkOption, Path, Paths}

import cats.effect.IO

/** Notices a start that never reached its first frame. A launch leaves a marker holding how many starts in a row have
  * not finished; painting the first frame clears it. A crash before that leaves the marker behind, so the next launch
  * can count and, from [[StartThreshold]] on, start in safe mode before reading any config or session. Safe-mode
  * launches do not count themselves, and one that reaches its first frame clears the marker, so safe mode cannot trap.
  */
object StartupCrashGuard:

  val StartThreshold: Int = 2

  enum Decision:
    case Proceed
    case StartSafeMode(unfinishedStarts: Int)

  def defaultMarker: Path =
    Paths.get(System.getProperty("user.home"), ".serenity", "startup-in-progress")

  /** The marker's presence means at least one start did not finish, whatever it holds. */
  def unfinishedStarts(markerContents: Option[String]): Int =
    markerContents.fold(0)(text => text.trim.toIntOption.filter(_ > 0).getOrElse(1))

  def decide(unfinishedStarts: Int, safeModeRequested: Boolean, threshold: Int = StartThreshold): Decision =
    if safeModeRequested || unfinishedStarts < threshold then Decision.Proceed
    else Decision.StartSafeMode(unfinishedStarts)

  /** The unfinished starts so far, without counting this one: for a launch already in safe mode. */
  def peekUnfinishedStarts(marker: Path): IO[Int] =
    IO.blocking(
      unfinishedStarts(
        Option.when(Files.isRegularFile(marker, LinkOption.NOFOLLOW_LINKS))(
          new String(Files.readAllBytes(marker), StandardCharsets.UTF_8)
        )
      )
    ).handleError(_ => 0)

  /** A launch already in safe mode only reads the marker: a safe start that dies must not push the next one further. */
  def countStart(safeMode: Boolean): IO[Int] =
    if safeMode then peekUnfinishedStarts(defaultMarker) else recordStartAttempt(defaultMarker)

  /** Returns the unfinished starts before this one and leaves the marker one higher. A marker that cannot be read or
    * written must never stop the editor starting, so failure counts as no history.
    */
  def recordStartAttempt(marker: Path): IO[Int] =
    IO.blocking {
      val previous = unfinishedStarts(
        Option.when(Files.isRegularFile(marker, LinkOption.NOFOLLOW_LINKS))(
          new String(Files.readAllBytes(marker), StandardCharsets.UTF_8)
        )
      )
      Files.createDirectories(marker.getParent)
      val _ = Files.write(marker, (previous + 1).toString.getBytes(StandardCharsets.UTF_8))
      previous
    }.handleError(_ => 0)

  def markStarted(marker: Path): IO[Unit] =
    IO.blocking(Files.deleteIfExists(marker)).void.handleError(_ => ())
