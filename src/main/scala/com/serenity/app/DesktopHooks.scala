package com.serenity.app

import java.nio.file.Path

import scala.concurrent.duration.FiniteDuration

import cats.effect.std.{Dispatcher, Queue}
import cats.effect.{IO, Resource}
import com.serenity.app.instance.{Delivery, LaunchRole}
import com.serenity.state.manager.QuitOutcome
import fs2.Stream

/** How the desktop answers a quit request it raised: it waits for exactly one of these. */
final case class QuitResponse(performQuit: IO[Unit], cancelQuit: IO[Unit])

/** Where the desktop's events go while one launch is running. */
final case class DesktopTarget(onOpen: List[Path] => Unit, onQuit: QuitResponse => Unit)

/** The AWT `Desktop`, behind `IO` so specs can raise its events without AWT. Events reach `target` for as long as the
  * returned resource is held.
  */
trait DesktopEvents:
  def attach(target: DesktopTarget): Resource[IO, Unit]

/** What the desktop raised, as streams. Both are single-consumer: whoever takes them owns them. */
final case class DesktopHooks(openedFiles: Stream[IO, List[Path]], quitRequests: Stream[IO, QuitResponse]):

  /** The launch-time opens a role serves: a primary alongside the launches forwarded to it, an isolated launch alone. A
    * launch that stepped aside serves none -- see [[DesktopHooks.forwardQueued]].
    */
  def opensFor(role: LaunchRole): Stream[IO, List[Path]] =
    role match
      case LaunchRole.Primary(forwardedOpens) => forwardedOpens.merge(openedFiles)
      case LaunchRole.Isolated(_)             => openedFiles
      case LaunchRole.Forwarded               => Stream.empty

object DesktopHooks:

  val none: DesktopHooks = DesktopHooks(Stream.empty, Stream.empty)

  /** Windowed launches only: a terminal launch never touches `java.awt.Desktop`, which would start the AWT toolkit
    * under a terminal UI.
    */
  def install(events: DesktopEvents, gui: Boolean): Resource[IO, DesktopHooks] =
    if !gui then Resource.pure(none)
    else
      for
        dispatcher <- Dispatcher.sequential[IO]
        opened     <- Resource.eval(Queue.unbounded[IO, List[Path]])
        quits      <- Resource.eval(Queue.unbounded[IO, QuitResponse])
        _ <- events.attach(
          DesktopTarget(
            paths => dispatcher.unsafeRunAndForget(opened.offer(paths)),
            request => dispatcher.unsafeRunAndForget(quits.offer(request))
          )
        )
      yield DesktopHooks(Stream.fromQueueUnterminated(opened), Stream.fromQueueUnterminated(quits))

  /** A launch that stepped aside for the running instance: the desktop may still be about to deliver the file it was
    * launched for, so queued opens get `grace` to arrive, then go to the running instance.
    */
  def forwardQueued(hooks: DesktopHooks, forward: List[Path] => IO[Delivery], grace: FiniteDuration): IO[Unit] =
    hooks.openedFiles.interruptAfter(grace).compile.toList.map(_.flatten).flatMap { paths =>
      IO.whenA(paths.nonEmpty)(forward(paths).void)
    }

  /** The desktop waits for an answer: perform the quit only once the orderly quit has finished, and cancel it when that
    * was called off or failed, so a failed save never leaves a Cmd-Q request unanswered.
    */
  def answer(response: QuitResponse, orderlyQuit: IO[QuitOutcome]): IO[Unit] =
    orderlyQuit.attempt.flatMap { outcome =>
      // The editor tears down as soon as the quit completes, which interrupts this loop; the answer still goes out.
      IO.uncancelable { _ =>
        outcome match
          case Right(QuitOutcome.Completed) => response.performQuit
          case _                            => response.cancelQuit
      }
    }

  def serveQuitRequests(requests: Stream[IO, QuitResponse], orderlyQuit: IO[QuitOutcome]): IO[Unit] =
    requests.evalMap(answer(_, orderlyQuit)).compile.drain
