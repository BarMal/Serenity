package com.serenity.state.manager

import cats.effect.std.{Queue, Semaphore}
import cats.effect.{Deferred, IO, Outcome, Ref}
import org.typelevel.log4cats.Logger

/** The state inbox and its single consumer (#1697): requests run one at a time, in arrival order, so state writes
  * routed through here never interleave with an event dispatch.
  *
  * The consumer fiber exists only while the inbox has work: the first offer into an idle inbox starts it, and it exits
  * once the inbox is empty. So it needs no owning `Resource` (`StateManager` is built as a plain `IO` by the app and by
  * many specs), and it runs on the runtime of whoever offered -- a spec may build the manager on one runtime and drive
  * it under `TestControl`.
  */
final private[manager] class StateManagerDispatcher private (
    inbox: Queue[IO, StateManagerDispatcher.Request],
    consuming: Ref[IO, Boolean],
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
      _       <- offer(Request(request.flatMap(value.complete).void, outcome => settled.complete(outcome).void))
      outcome <- settled.get
      result <- outcome match
        case Outcome.Succeeded(_)   => value.get
        case Outcome.Errored(error) => IO.raiseError(error)
        case Outcome.Canceled()     => IO.raiseError(new IllegalStateException("dispatched request was canceled"))
    yield result

  /** Queues `update` for the dispatcher without waiting for it -- how background work hands over its result. */
  def post(update: IO[Unit]): IO[Unit] =
    offer(Request(update.handleErrorWith(logger.error(_)("[DISPATCH] Posted state update failed")), _ => IO.unit))

  /** Runs `request` on the caller's fiber, excluded from dispatcher work, only if the dispatcher is idle right now;
    * `None` means it was busy and `request` did not run. `request` must not itself submit to the dispatcher.
    */
  def runIfIdle[A](request: IO[A]): IO[Option[A]] =
    exclusive.tryPermit.use(idle => if idle then request.map(Some(_)) else IO.none)

  private def offer(request: Request): IO[Unit] =
    (inbox.offer(request) >> ensureConsuming).uncancelable

  private def ensureConsuming: IO[Unit] =
    consuming.getAndSet(true).flatMap(alreadyConsuming => if alreadyConsuming then IO.unit else consume.start.void)

  // Each request runs on its own child fiber so one that cancels itself cannot take the consumer down with it. The
  // submitter is only released once the permit is, so its next `runIfIdle` finds the dispatcher idle. The size check
  // after standing down catches an offer that saw this consumer still running just before it stopped.
  private def consume: IO[Unit] =
    inbox.tryTake.flatMap {
      case Some(request) =>
        exclusive.permit.surround(request.work.start.flatMap(_.join)).flatMap(request.settle) >> consume
      case None =>
        consuming.set(false) >> inbox.size.flatMap(pending => if pending > 0 then ensureConsuming else IO.unit)
    }

private[manager] object StateManagerDispatcher:

  final private case class Request(work: IO[Unit], settle: Outcome[IO, Throwable, Unit] => IO[Unit])

  def create(logger: Logger[IO]): IO[StateManagerDispatcher] =
    for
      inbox     <- Queue.unbounded[IO, Request]
      consuming <- Ref.of[IO, Boolean](false)
      exclusive <- Semaphore[IO](1)
    yield new StateManagerDispatcher(inbox, consuming, exclusive, logger)
