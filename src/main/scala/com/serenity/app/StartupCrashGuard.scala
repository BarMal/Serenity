package com.serenity.app

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, LinkOption, Path, Paths}

import cats.effect.IO

/** Notices a start that never reached its first frame. A launch leaves a marker holding how many starts in a row have
  * not finished; painting the first frame clears it. A crash before that leaves the marker behind, so the next launch
  * can count and, from [[OfferThreshold]] on, offer safe mode.
  */
object StartupCrashGuard:

  val OfferThreshold: Int = 2

  enum Decision:
    case Proceed
    case OfferSafeMode(unfinishedStarts: Int)

  def defaultMarker: Path =
    Paths.get(System.getProperty("user.home"), ".serenity", "startup-in-progress")

  /** The marker's presence means at least one start did not finish, whatever it holds. */
  def unfinishedStarts(markerContents: Option[String]): Int =
    markerContents.fold(0)(text => text.trim.toIntOption.filter(_ > 0).getOrElse(1))

  def decide(unfinishedStarts: Int, safeModeRequested: Boolean, threshold: Int = OfferThreshold): Decision =
    if safeModeRequested || unfinishedStarts < threshold then Decision.Proceed
    else Decision.OfferSafeMode(unfinishedStarts)

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
