package com.serenity.ui.sdl

import scala.concurrent.duration.FiniteDuration

import cats.effect.std.Queue
import cats.effect.{Deferred, IO, Ref}
import cats.syntax.all.*
import fs2.Stream

/** Where the SDL thread's events go: each one is folded into the window's state, which the window host asks about, and
  * published on [[events]] for whoever reads input.
  *
  * The published queue is a circular buffer of [[SdlEventHub.Backlog]] events. Publishing never waits, so a reader that
  * is slow, or that does not exist yet, cannot stop the pump or keep a resize or close request from being seen; when it
  * falls further behind than the buffer holds, it loses the oldest events, not the newest. [[events]] is for one
  * reader: two would split the events between them.
  */
final private[sdl] class SdlEventHub private (
    current: Ref[IO, GlWindowState],
    closed: Deferred[IO, Unit],
    queue: Queue[IO, SdlEvent.Stamped]
):

  def state: IO[GlWindowState] = current.get

  /** The pixel size to lay out for after a resize or a scale change, once, until the next one. */
  def takeResize: IO[Option[(Int, Int)]] = current.modify(_.takeResize)

  /** Completes once a close request has been published. */
  def awaitClose: IO[Unit] = closed.get

  def events: Stream[IO, SdlEvent.Stamped] = Stream.fromQueueUnterminated(queue)

  /** `scale` is the display scale read when the event arrived; it is used only if the event changed it. */
  def publish(stamped: SdlEvent.Stamped, scale: Double): IO[Unit] =
    current
      .updateAndGet(_.after(stamped.event, scale))
      .flatMap(now => closed.complete(()).void.whenA(now.closeRequested))
      .productR(queue.offer(stamped))

private[sdl] object SdlEventHub:

  val Backlog: Int = 1024

  def create(start: GlWindowState): IO[SdlEventHub] =
    (Ref.of[IO, GlWindowState](start), Deferred[IO, Unit], Queue.circularBuffer[IO, SdlEvent.Stamped](Backlog))
      .mapN(new SdlEventHub(_, _, _))

  /** Polls for events, publishes what it gets, and sleeps `interval`, until it is cancelled. */
  def pump(poll: IO[(List[SdlEvent.Stamped], Double)], hub: SdlEventHub, interval: FiniteDuration): IO[Nothing] =
    poll
      .flatMap((events, scale) => events.traverse_(hub.publish(_, scale)))
      .productR(IO.sleep(interval))
      .foreverM
