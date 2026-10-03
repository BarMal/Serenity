package com.serenity.app

import scala.concurrent.duration.*

import cats.effect.std.Supervisor
import cats.effect.{IO, Ref}
import cats.syntax.all.*
import com.serenity.state.manager.StateUpdater
import com.serenity.state.models.AppState

/** Ends the typing quiet window on its own timer once the last typed character is
  * [[com.serenity.state.models.TypingActivity.QuietWindow]] old, rather than waiting for a render-loop tick to notice
  * it has run out -- so nothing wakes while the editor is idle.
  *
  * A commit that moves the window's end arms a timer; one that arrives while another is pending supersedes it, and the
  * superseded timer wakes to find it is no longer current. The timers are never cancelled: this runs inside the commit
  * dispatcher, which a cancelled expiry mid-commit would wait on.
  */
final class TypingQuietTimer private (supervisor: Supervisor[IO], latest: Ref[IO, Long], expire: IO[Unit]):

  def onCommit(before: AppState, after: AppState): IO[Unit] =
    val windowEnd = after.runtime.typingActivity.quietUntilNanos
    if windowEnd == before.runtime.typingActivity.quietUntilNanos then IO.unit
    else windowEnd.traverse_(arm)

  private def arm(windowEnd: Long): IO[Unit] =
    for
      generation <- latest.updateAndGet(_ + 1)
      now        <- IO.monotonic
      _          <- supervisor.supervise(await(generation, (windowEnd - now.toNanos).nanos)).void
    yield ()

  private def await(generation: Long, remaining: FiniteDuration): IO[Unit] =
    IO.sleep(remaining) >> latest.get.flatMap(current => IO.whenA(current == generation)(expire))

object TypingQuietTimer:

  def create(supervisor: Supervisor[IO], expire: IO[Unit]): IO[TypingQuietTimer] =
    Ref.of[IO, Long](0L).map(new TypingQuietTimer(supervisor, _, expire))

  /** Expires the window through an ordinary validated commit, which the render loop's commit observer turns into the
    * status-row repaint like any other state change.
    */
  def expireIn(stateManager: StateUpdater): IO[Unit] =
    IO.monotonic.flatMap { now =>
      stateManager.updateStateValidated(state =>
        state.copy(runtime = state.runtime.copy(typingActivity = state.runtime.typingActivity.advance(now.toNanos)))
      )
    }
