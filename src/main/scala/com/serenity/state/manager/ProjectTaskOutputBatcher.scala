package com.serenity.state.manager

import scala.concurrent.duration.{DurationLong, FiniteDuration}

import cats.effect.std.Queue
import cats.effect.{IO, Ref}
import com.serenity.project.ProjectTaskRunner

/** Collects a running project task's output and hands it to `publish` at most once per `interval`, however fast the
  * process writes. Each hand-off waits for `publish`, so a busy dispatcher only makes the next batch bigger. The buffer
  * keeps the same bounded tail the terminal panel does.
  *
  * Event-driven: the first chunk into an empty buffer wakes [[run]], which then waits out the interval so the rest of a
  * burst joins the batch, at the next refresh boundary (a multiple of `interval` after [[run]] started, the cadence the
  * panel has always had). A silent task leaves [[run]] parked on the wake-up rather than polling.
  */
final private[manager] class ProjectTaskOutputBatcher private (
    unpublished: Ref[IO, String],
    wakeups: Queue[IO, Unit],
    interval: FiniteDuration,
    publish: String => IO[Unit]
):

  def append(chunk: String): IO[Unit] =
    unpublished
      .modify { existing =>
        val updated = ProjectTaskRunner.appendOutputTail(existing, chunk)
        (updated, existing.isEmpty && updated.nonEmpty)
      }
      .flatMap(wasEmpty => IO.whenA(wasEmpty)(wakeups.tryOffer(()).void))

  val run: IO[Nothing] =
    IO.monotonic.flatMap(started => (wakeups.take >> untilNextRefresh(started) >> flush).foreverM)

  private def untilNextRefresh(started: FiniteDuration): IO[Unit] =
    IO.monotonic.flatMap { now =>
      val intoInterval = (now - started).toNanos % interval.toNanos
      IO.sleep(interval - intoInterval.nanos)
    }

  private def flush: IO[Unit] =
    unpublished.getAndSet("").flatMap(batch => IO.whenA(batch.nonEmpty)(publish(batch)))

private[manager] object ProjectTaskOutputBatcher:

  def create(interval: FiniteDuration, publish: String => IO[Unit]): IO[ProjectTaskOutputBatcher] =
    for
      unpublished <- IO.ref("")
      wakeups     <- Queue.bounded[IO, Unit](1)
    yield new ProjectTaskOutputBatcher(unpublished, wakeups, interval, publish)
