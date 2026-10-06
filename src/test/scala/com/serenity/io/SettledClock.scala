package com.serenity.io

import scala.concurrent.duration.{DurationInt, FiniteDuration}

import cats.effect.IO
import com.serenity.rope.Balance

/** A clock a minute ahead of the real one, so a file written moments ago already counts as old enough for its stamp to
  * vouch for its content (see [[FileStamp.vouchesForContent]]) without the test sleeping.
  */
object SettledClock:
  val aMinuteAhead: IO[FiniteDuration] = IO.realTime.map(_ + 1.minute)

  def fileManager(using Balance): FileManager =
    new FileManager(LocalDocumentStorageProvider(clock = aMinuteAhead))
