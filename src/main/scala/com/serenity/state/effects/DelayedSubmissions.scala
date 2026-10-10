package com.serenity.state.effects

import scala.concurrent.duration.*

import cats.effect.std.Queue
import cats.effect.{Deferred, IO, Ref}
import cats.syntax.all.*

/** Holds jobs that are due after a quiet delay and hands each to its lane when the delay has passed, from one
  * long-lived fiber: a later job for the same lane replaces the waiting one by overwriting a map entry, so a burst of
  * keystrokes starts no fiber per keystroke (#1928). The fiber parks, waking nothing, while no job is waiting.
  */
final private[effects] class DelayedSubmissions private (
    state: Ref[IO, DelayedSubmissions.State],
    wakeups: Queue[IO, Unit],
    submit: (Lane.Keyed, IO[Unit]) => IO[Unit]
):
  import DelayedSubmissions.{Entry, Step}

  def schedule(lane: Lane.Keyed, delay: FiniteDuration, job: IO[Unit]): IO[Unit] =
    IO.monotonic.flatMap { now =>
      state.modify(_.scheduled(lane, now + delay, job)).flatMap(IO.whenA(_)(wakeups.offer(())))
    }

  /** Completes once every job scheduled before the call has been handed to its lane or replaced by a later one. */
  def handedOver: IO[Unit] =
    IO.deferred[Unit].flatMap { handed =>
      state.modify(_.awaiting(handed)).flatMap(waiting => IO.whenA(waiting)(handed.get))
    }

  /** Forgets every waiting job and releases whoever waits for them, for when the lanes are released. */
  def discard: IO[Unit] =
    state.getAndUpdate(_.emptied).flatMap(_.waiters.traverse_(_._2.complete(()).void))

  val run: IO[Nothing] = (wakeups.take >> serve).foreverM

  private def serve: IO[Unit] =
    IO.monotonic.flatMap(now => state.modify(_.advance(now))).flatMap {
      case Step.Fire(due) => due.traverse_((lane, entry) => submit(lane, entry.job)) >> forget(due) >> serve
      case Step.Wait(until, handed) =>
        handed.traverse_(_.complete(())) >> until
          .traverse_(deadline => IO.monotonic.flatMap(now => IO.race(IO.sleep(deadline - now), wakeups.take)) >> serve)
    }

  private def forget(due: Vector[(Lane.Keyed, Entry)]): IO[Unit] =
    state.update(_.without(due))

object DelayedSubmissions:

  def create(submit: (Lane.Keyed, IO[Unit]) => IO[Unit]): IO[DelayedSubmissions] =
    (Ref.of[IO, State](State.empty), Queue.dropping[IO, Unit](1)).mapN(new DelayedSubmissions(_, _, submit))

  final private[effects] case class Entry(seq: Long, due: FiniteDuration, job: IO[Unit])

  private[effects] enum Step:
    case Fire(due: Vector[(Lane.Keyed, Entry)])
    case Wait(until: Option[FiniteDuration], handed: Vector[Deferred[IO, Unit]])

  /** `sleepingUntil` is the deadline the fiber is asleep for, so only an earlier one needs to wake it. */
  final private[effects] case class State(
      nextSeq: Long,
      pending: Map[Lane.Keyed, Entry],
      sleepingUntil: Option[FiniteDuration],
      waiters: Vector[(Long, Deferred[IO, Unit])]
  ):

    def scheduled(lane: Lane.Keyed, due: FiniteDuration, job: IO[Unit]): (State, Boolean) =
      val next = copy(nextSeq = nextSeq + 1, pending = pending.updated(lane, Entry(nextSeq, due, job)))
      (next, sleepingUntil.forall(_ > due))

    def awaiting(handed: Deferred[IO, Unit]): (State, Boolean) =
      val target = nextSeq - 1
      if pendingUpTo(target) then (copy(waiters = waiters :+ (target -> handed)), true) else (this, false)

    def advance(now: FiniteDuration): (State, Step) =
      val due = pending.filter((_, entry) => entry.due <= now).toVector
      if due.nonEmpty then (copy(sleepingUntil = None), Step.Fire(due))
      else
        val (released, kept) = waiters.partition((target, _) => !pendingUpTo(target))
        val until            = pending.valuesIterator.map(_.due).minOption
        (copy(sleepingUntil = until, waiters = kept), Step.Wait(until, released.map(_._2)))

    def without(fired: Vector[(Lane.Keyed, Entry)]): State =
      val firedSeqs = fired.map((lane, entry) => lane -> entry.seq).toSet
      copy(pending = pending.filterNot((lane, entry) => firedSeqs.contains(lane -> entry.seq)))

    def emptied: State = State.empty

    private def pendingUpTo(target: Long): Boolean = pending.valuesIterator.exists(_.seq <= target)

  private[effects] object State:
    val empty: State = State(0L, Map.empty, None, Vector.empty)
