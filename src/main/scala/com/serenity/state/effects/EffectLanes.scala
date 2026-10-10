package com.serenity.state.effects

import scala.concurrent.duration.FiniteDuration

import cats.effect.std.Supervisor
import cats.effect.{Deferred, IO, Outcome, Ref, Resource}
import cats.syntax.all.*

/** Runs effects off the dispatcher fiber, one lane at a time per [[Lane]] (see docs/state-architecture-target.md).
  *
  * Jobs on one Sequential lane run FIFO; jobs on different lanes run in parallel. A failed job is reported through the
  * `onFailure` callback given to [[EffectLanes.resource]] and its lane carries on with the next job.
  */
trait EffectLanes:

  /** Accepts `job` and returns without waiting for it to start or finish -- including an Exclusive job, which is
    * admitted later, so the dispatcher is never blocked. [[Lane.Inline]] is excluded by type: it has no lane to run on.
    * Fails with [[EffectLanes.Released]] once the resource has been released.
    */
  def submit(lane: Lane.Scheduled, job: IO[Unit]): IO[Unit]

  /** Accepts `job` to be submitted to `lane` once `delay` passes with no later `submitAfter` for the same lane: each
    * call replaces the one still waiting, so a burst of calls runs `job` once, after the burst, and costs no fiber per
    * call. Unlike [[submit]], a job already running on the lane is cancelled when the replacement is handed over, not
    * when it is requested. With `maxWait`, a burst that never goes quiet still runs `job` that long after its first
    * request. Fails with [[EffectLanes.Released]] once the resource has been released.
    */
  def submitAfter(
    lane: Lane.Keyed,
    delay: FiniteDuration,
    job: IO[Unit],
    maxWait: Option[FiniteDuration] = None
  ): IO[Unit]

  /** Completes once every job accepted before the call has finished, been cancelled, been superseded or been dropped.
    * Work accepted afterwards is not waited for, so a steady stream of submissions cannot starve a drain.
    */
  def drain: IO[Unit]

object EffectLanes:

  final class Released extends IllegalStateException("EffectLanes used after its resource was released")

  /** `onFailure` should not fail itself; if it does, that error goes to the runtime's failure reporter. Releasing the
    * resource cancels every running job and discards every waiting one.
    */
  def resource(onFailure: (Lane.Scheduled, Throwable) => IO[Unit]): Resource[IO, EffectLanes] =
    for
      supervisor <- Supervisor[IO](await = false)
      table      <- Resource.make(IO.ref(LaneTable.empty))(table => table.modify(_.release).flatMap(signalOnly))
      lanes      <- Resource.eval(IO.deferred[EffectLanes])
      delayed    <- Resource.make(DelayedSubmissions.create(handOver(lanes)))(_.discard)
      _          <- Resource.eval(supervisor.supervise(delayed.run))
      supervised = SupervisedLanes(supervisor, table, delayed, onFailure)
      _ <- Resource.eval(lanes.complete(supervised))
    yield supervised

  // The lanes may be released while a delayed job is still due; it then has nowhere to run and is dropped.
  private def handOver(lanes: Deferred[IO, EffectLanes])(lane: Lane.Keyed, job: IO[Unit]): IO[Unit] =
    lanes.get.flatMap(_.submit(lane, job)).recover { case _: Released => () }

  private def signalOnly(actions: Actions): IO[Unit] =
    actions.cancel.traverse_(_.complete(())) >> actions.release.traverse_(_.complete(()))

  final private class SupervisedLanes(
      supervisor: Supervisor[IO],
      table: Ref[IO, LaneTable],
      delayed: DelayedSubmissions,
      onFailure: (Lane.Scheduled, Throwable) => IO[Unit]
  ) extends EffectLanes:

    def submit(lane: Lane.Scheduled, job: IO[Unit]): IO[Unit] =
      IO.deferred[Unit].flatMap { cancelSignal =>
        IO.uncancelable { _ =>
          table.modify { current =>
            if current.released then (current, IO.raiseError(Released()))
            else
              val (next, actions) = current.accept(lane, job, cancelSignal)
              (next, perform(actions))
          }.flatten
        }
      }

    def submitAfter(
      lane: Lane.Keyed,
      delay: FiniteDuration,
      job: IO[Unit],
      maxWait: Option[FiniteDuration]
    ): IO[Unit] =
      table.get.flatMap(current =>
        if current.released then IO.raiseError(Released()) else delayed.schedule(lane, delay, job, maxWait)
      )

    def drain: IO[Unit] =
      delayed.handedOver >> IO.deferred[Unit].flatMap(drained => transition(_.awaitDrain(drained)) >> drained.get)

    private def transition(step: LaneTable => (LaneTable, Actions)): IO[Unit] =
      IO.uncancelable(_ => table.modify(step).flatMap(perform))

    private def perform(actions: Actions): IO[Unit] =
      signalOnly(actions) >> actions.start.traverse_(started => supervisor.supervise(execute(started)).void)

    private def execute(started: Started): IO[Unit] =
      runUntilCancelled(started.job)
        .flatMap(reportFailure(started.lane, _))
        .guarantee(transition(_.finish(started.lane, started.job.ticket)))

    // A race against the signal alone would wait forever on a job that cancels itself; racePair sees that outcome.
    private def runUntilCancelled(job: Job): IO[Outcome[IO, Throwable, Unit]] =
      IO.racePair(job.run, job.cancelSignal.get).flatMap {
        case Left((outcome, signalWatch)) => signalWatch.cancel.as(outcome)
        case Right((running, _))          => running.cancel >> running.join
      }

    private def reportFailure(lane: Lane.Scheduled, outcome: Outcome[IO, Throwable, Unit]): IO[Unit] =
      outcome match
        case Outcome.Errored(error) =>
          onFailure(lane, error).handleErrorWith(unreported =>
            IO.executionContext.flatMap(ec => IO(ec.reportFailure(unreported)))
          )
        case Outcome.Succeeded(_) | Outcome.Canceled() => IO.unit
