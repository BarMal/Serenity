package com.serenity.app

import scala.concurrent.duration.*

import cats.effect.std.{Queue, Supervisor}
import cats.effect.{IO, Ref}
import cats.syntax.all.*
import com.serenity.state.manager.StateUpdater
import com.serenity.state.models.AppState

/** Ends the typing quiet window on its own timer once the last typed character is
  * [[com.serenity.state.models.TypingActivity.QuietWindow]] old, rather than waiting for a render-loop tick to notice
  * it has run out -- so nothing wakes while the editor is idle.
  *
  * One fiber serves every window: a commit that moves the window's end only records the new end, and the fiber, asleep
  * until the end it last saw, finds the window has moved on and sleeps again. A burst of keystrokes therefore starts no
  * fiber and leaves none sleeping (#1928), and the expiry never runs inside the commit dispatcher it would otherwise
  * have to be cancelled from.
  */
final class TypingQuietTimer private (windowEnd: Ref[IO, Option[Long]], armed: Queue[IO, Unit], expire: IO[Unit]):

  def onCommit(before: AppState, after: AppState): IO[Unit] =
    val end = after.runtime.typingActivity.quietUntilNanos
    if end == before.runtime.typingActivity.quietUntilNanos then IO.unit
    else end.traverse_(arm)

  private def arm(end: Long): IO[Unit] =
    windowEnd.getAndSet(Some(end)).flatMap(previous => IO.whenA(previous.isEmpty)(armed.offer(())))

  private val run: IO[Nothing] = (armed.take >> expireOnceQuiet).foreverM

  private def expireOnceQuiet: IO[Unit] =
    IO.monotonic.flatMap { now =>
      windowEnd.modify {
        case Some(end) if end <= now.toNanos => (None, expire)
        case Some(end)                       => (Some(end), IO.sleep((end - now.toNanos).nanos) >> expireOnceQuiet)
        case None                            => (None, IO.unit)
      }.flatten
    }

object TypingQuietTimer:

  def create(supervisor: Supervisor[IO], expire: IO[Unit]): IO[TypingQuietTimer] =
    for
      windowEnd <- Ref.of[IO, Option[Long]](None)
      armed     <- Queue.dropping[IO, Unit](1)
      timer = new TypingQuietTimer(windowEnd, armed, expire)
      _ <- supervisor.supervise(timer.run)
    yield timer

  /** Expires the window through an ordinary validated commit, which the render loop's commit observer turns into the
    * status-row repaint like any other state change.
    */
  def expireIn(stateManager: StateUpdater): IO[Unit] =
    IO.monotonic.flatMap { now =>
      stateManager.updateStateValidated(state =>
        state.copy(runtime = state.runtime.copy(typingActivity = state.runtime.typingActivity.advance(now.toNanos)))
      )
    }
