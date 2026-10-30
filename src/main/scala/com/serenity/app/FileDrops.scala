package com.serenity.app

import java.nio.file.Path

import cats.effect.std.{Dispatcher, Queue}
import cats.effect.{IO, Resource}
import fs2.Stream

/** Paths dropped onto the window, and what drags offered, handed from the AWT thread to the editor's side. The
  * callbacks only enqueue, so the event thread never waits on an open; `opened` is single-consumer, like
  * [[DesktopHooks]]'s streams.
  */
final case class FileDrops(opened: Stream[IO, List[Path]], dropped: List[Path] => Unit, offered: String => Unit)

object FileDrops:

  def create(log: String => IO[Unit]): Resource[IO, FileDrops] =
    for
      dispatcher <- Dispatcher.sequential[IO]
      queue      <- Resource.eval(Queue.unbounded[IO, List[Path]])
    yield FileDrops(
      Stream.fromQueueUnterminated(queue),
      paths => dispatcher.unsafeRunAndForget(queue.offer(paths)),
      description => dispatcher.unsafeRunAndForget(log(description))
    )
