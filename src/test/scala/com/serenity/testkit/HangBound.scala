package com.serenity.testkit

import java.lang.management.ManagementFactory
import java.util.concurrent.TimeoutException

import scala.concurrent.duration.*
import scala.jdk.CollectionConverters.*

import cats.effect.IO
import cats.effect.unsafe.IORuntime

/** How long a spec lets a program run before it calls it hung.
  *
  * The bound is measured in time the JVM was running: a program is given [[Default]] plus however long the garbage
  * collectors stopped it, so a long pause on a loaded runner cannot fail a program that was about to finish. A program
  * that never finishes still fails once its un-paused time reaches the bound.
  */
object HangBound:

  val Default: FiniteDuration = 10.seconds

  /** Milliseconds the JVM has spent paused for collection since it started; never decreases. */
  trait PausedTime:
    def millis(): Long

  object PausedTime:

    /** The stop-the-world collectors' accumulated time. A concurrent collector's beans ("G1 Concurrent GC", "ZGC
      * Cycles") count work done beside the program, which is not a pause.
      */
    val jvm: PausedTime = () =>
      ManagementFactory.getGarbageCollectorMXBeans.asScala
        .filterNot(bean => bean.getName.contains("Concurrent") || bean.getName.contains("Cycles"))
        .map(_.getCollectionTime)
        .filter(_ >= 0L)
        .sum

  /** `program`'s result, or a `TimeoutException` once it has run for `bound` of un-paused time; the program is
    * cancelled then. An exception the program raises itself reaches the caller as it is.
    */
  def within[A](
    program: IO[A],
    bound: FiniteDuration = Default,
    paused: PausedTime = PausedTime.jvm,
    poll: FiniteDuration = 25.millis
  ): IO[A] =
    finished(program, bound, paused, poll).flatMap {
      case Some(result) => IO.pure(result)
      case None => IO.raiseError(TimeoutException(s"still running after ${bound.toMillis} ms of un-paused time"))
    }

  /** The same, as `Some(result)` or `None` for a program that did not finish. */
  def unsafeRunTimed[A](
    program: IO[A],
    bound: FiniteDuration = Default,
    paused: PausedTime = PausedTime.jvm,
    poll: FiniteDuration = 25.millis
  )(using IORuntime): Option[A] =
    finished(program, bound, paused, poll).unsafeRunSync()

  private def finished[A](
    program: IO[A],
    bound: FiniteDuration,
    paused: PausedTime,
    poll: FiniteDuration
  ): IO[Option[A]] =
    IO.monotonic.flatMap { started =>
      val pausedAtStart = paused.millis()
      val expired: IO[Boolean] = IO.monotonic.map { now =>
        val elapsed = (now - started).toMillis
        elapsed - (paused.millis() - pausedAtStart) >= bound.toMillis
      }
      val watchdog: IO[Unit] = (IO.sleep(poll) >> expired).iterateUntil(identity).void
      IO.race(program, watchdog).map(_.left.toOption)
    }
