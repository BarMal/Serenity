package com.serenity.state.manager

import cats.effect.std.{Queue, Semaphore}
import cats.effect.{Deferred, IO, Outcome}
import org.typelevel.log4cats.Logger

/** The state inbox and its single consumer (#1697): requests run one at a time, in arrival order, so state writes routed
  * through here never interleave with an event dispatch.
  *
  * The consumer is started unsupervised: `StateManager` is built as a plain `IO` by the app and by many specs, and an
  * idle consumer is parked on `take` without holding a thread.
  */
final private[manager] class StateManagerDispatcher private (
    inbox: Queue[IO, StateManagerDispatcher.Request],
    // Held by the consumer while a request runs, and by a render tick that finds the consumer idle.
    exclusive: Semaphore[IO],
    logger: Logger[IO]
):
  import StateManagerDispatcher.Request

  /** Runs `request` on the dispatcher and waits for its result, re-raising its failure.
    *
    * Never call this from code already running on the dispatcher (an event dispatch, a posted update, or a request
    * submitted here): the request would queue behind the one waiting for it and deadlock. Such code replays follow-up
    * work directly instead -- see `StateManagerEventPipeline.drainPendingOperations`.
    */
  def submit[A](request: IO[A]): IO[A] =
    for
      value   <- Deferred[IO, A]
      settled <- Deferred[IO, Outcome[IO, Throwable, Unit]]
      _       <- inbox.offer(Request(request.flatMap(value.complete).void, outcome => settled.complete(outcome).void))
      outcome <- settled.get
      result <- outcome match
        case Outcome.Succeeded(_) => value.get
        case Outcome.Errored(error) => IO.raiseError(error)
        case Outcome.Canceled()     => IO.raiseError(new IllegalStateException("dispatched request was canceled"))
    yield result

  /** Queues `update` for the dispatcher without waiting for it -- how background work hands over its result. */
  def post(update: IO[Unit]): IO[Unit] =
    inbox.offer(
      Request(update.handleErrorWith(logger.error(_)("[DISPATCH] Posted state update failed")), _ => IO.unit)
    )

  /** Runs `request` on the caller's fiber, excluded from dispatcher work, only if the dispatcher is idle right now;
    * `None` means it was busy and `request` did not run. `request` must not itself submit to the dispatcher.
    */
  def runIfIdle[A](request: IO[A]): IO[Option[A]] =
    exclusive.tryPermit.use(idle => if idle then request.map(Some(_)) else IO.none)

  // Each request runs on its own child fiber so one that cancels itself cannot take the consumer down with it. The
  // submitter is only released once the permit is, so its next `runIfIdle` finds the dispatcher idle.
  private def consume: IO[Nothing] =
    inbox.take.flatMap { request =>
      exclusive.permit.surround(request.work.start.flatMap(_.join)).flatMap(request.settle)
    }.foreverM

private[manager] object StateManagerDispatcher:

  final private case class Request(work: IO[Unit], settle: Outcome[IO, Throwable, Unit] => IO[Unit])

  def start(logger: Logger[IO]): IO[StateManagerDispatcher] =
    for
      inbox     <- Queue.unbounded[IO, Request]
      exclusive <- Semaphore[IO](1)
      dispatcher = new StateManagerDispatcher(inbox, exclusive, logger)
      _ <- dispatcher.consume.start
    yield dispatcher
