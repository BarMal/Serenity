package com.serenity.state.effects

import scala.collection.immutable.SortedSet

import cats.effect.{Deferred, IO}

/** An accepted job. The ticket orders acceptance for `drain`; completing `cancelSignal` cancels the job whether or not
  * it has started yet.
  */
final private[effects] case class Job(ticket: Long, run: IO[Unit], cancelSignal: Deferred[IO, Unit])

final private[effects] case class Started(lane: Lane.Scheduled, job: Job)

/** A key is present in the table exactly while one of its jobs runs; `waiting` holds what runs after it. */
final private[effects] case class KeyedWork(running: Job, waiting: Vector[Job])

private[effects] enum Gate:
  case Open
  case Draining(exclusive: Job, held: Vector[Started])
  case RunningAlone(exclusive: Job, held: Vector[Started])

/** What the interpreter must do after a transition, in this order: signal cancellations, release drain waiters, start
  * jobs.
  */
final private[effects] case class Actions(
    cancel: Vector[Deferred[IO, Unit]],
    release: Vector[Deferred[IO, Unit]],
    start: Vector[Started]
):
  def ++(other: Actions): Actions = Actions(cancel ++ other.cancel, release ++ other.release, start ++ other.start)

private[effects] object Actions:
  val none: Actions                                  = Actions(Vector.empty, Vector.empty, Vector.empty)
  def cancel(signals: Vector[Deferred[IO, Unit]])    = Actions(signals, Vector.empty, Vector.empty)
  def release(waiters: Vector[Deferred[IO, Unit]])   = Actions(Vector.empty, waiters, Vector.empty)
  def start(lane: Lane.Scheduled, job: Job): Actions = Actions(Vector.empty, Vector.empty, Vector(Started(lane, job)))

/** The pure state machine behind [[EffectLanes]]. Every transition returns the next table plus the [[Actions]] it
  * implies, so the interpreter can apply it with a single `Ref.modify` and never runs a job while deciding.
  */
final private[effects] case class LaneTable(
    nextTicket: Long,
    keyed: Map[Lane.Keyed, KeyedWork],
    gate: Gate,
    outstanding: SortedSet[Long],
    drainWaiters: Vector[(Long, Deferred[IO, Unit])],
    released: Boolean
):

  def accept(lane: Lane.Scheduled, run: IO[Unit], cancelSignal: Deferred[IO, Unit]): (LaneTable, Actions) =
    val job = Job(nextTicket, run, cancelSignal)
    copy(nextTicket = nextTicket + 1, outstanding = outstanding + job.ticket).route(Started(lane, job))

  def finish(lane: Lane.Scheduled, ticket: Long): (LaneTable, Actions) =
    if released then (this, Actions.none)
    else
      val (settled, releases) = settle(Vector(ticket))
      val (next, actions) = lane match
        case keyedLane: Lane.Keyed => settled.finishKeyed(keyedLane, ticket)
        case Lane.Exclusive        => settled.finishExclusive
      (next, releases ++ actions)

  /** Releases `waiter` once every job accepted so far has settled. */
  def awaitDrain(waiter: Deferred[IO, Unit]): (LaneTable, Actions) =
    val target = nextTicket - 1
    if released || outstanding.headOption.forall(_ > target) then (this, Actions.release(Vector(waiter)))
    else (copy(drainWaiters = drainWaiters :+ (target -> waiter)), Actions.none)

  def release: (LaneTable, Actions) =
    (copy(released = true, drainWaiters = Vector.empty), Actions.release(drainWaiters.map(_._2)))

  private def route(started: Started): (LaneTable, Actions) =
    gate match
      case Gate.Open                      => admit(started)
      case Gate.Draining(exclusive, held) => (copy(gate = Gate.Draining(exclusive, held :+ started)), Actions.none)
      case Gate.RunningAlone(exclusive, held) =>
        (copy(gate = Gate.RunningAlone(exclusive, held :+ started)), Actions.none)

  private def admit(started: Started): (LaneTable, Actions) =
    started.lane match
      case Lane.Exclusive        => beginExclusive(started.job)
      case keyedLane: Lane.Keyed => admitKeyed(keyedLane, started.job)

  private def admitKeyed(lane: Lane.Keyed, job: Job): (LaneTable, Actions) =
    keyed.get(lane) match
      case None => (copy(keyed = keyed.updated(lane, KeyedWork(job, Vector.empty))), Actions.start(lane, job))
      case Some(work) =>
        lane.policy match
          case LanePolicy.Sequential =>
            (copy(keyed = keyed.updated(lane, work.copy(waiting = work.waiting :+ job))), Actions.none)
          case LanePolicy.SwitchLatest =>
            val (next, releases) =
              copy(keyed = keyed.updated(lane, KeyedWork(work.running, Vector(job)))).settle(work.waiting.map(_.ticket))
            (next, releases ++ Actions.cancel(Vector(work.running.cancelSignal)))
          case LanePolicy.DropIfBusy => settle(Vector(job.ticket))

  private def beginExclusive(exclusive: Job): (LaneTable, Actions) =
    val (interruptible, sequential) = keyed.partition((lane, _) => lane.policy != LanePolicy.Sequential)
    val superseded                  = interruptible.values.toVector.flatMap(_.waiting.map(_.ticket))
    val (settled, releases) = copy(
      keyed = sequential ++ interruptible.view.mapValues(_.copy(waiting = Vector.empty)),
      gate = Gate.Draining(exclusive, Vector.empty)
    ).settle(superseded)
    val (next, starts) = settled.startExclusiveIfIdle
    (next, Actions.cancel(interruptible.values.toVector.map(_.running.cancelSignal)) ++ releases ++ starts)

  private def startExclusiveIfIdle: (LaneTable, Actions) =
    gate match
      case Gate.Draining(exclusive, held) if keyed.isEmpty =>
        (copy(gate = Gate.RunningAlone(exclusive, held)), Actions.start(Lane.Exclusive, exclusive))
      case _ => (this, Actions.none)

  private def finishKeyed(lane: Lane.Keyed, ticket: Long): (LaneTable, Actions) =
    keyed.get(lane) match
      case Some(KeyedWork(running, next +: rest)) if running.ticket == ticket =>
        (copy(keyed = keyed.updated(lane, KeyedWork(next, rest))), Actions.start(lane, next))
      case Some(KeyedWork(running, _)) if running.ticket == ticket =>
        copy(keyed = keyed - lane).startExclusiveIfIdle
      case _ => (this, Actions.none)

  private def finishExclusive: (LaneTable, Actions) =
    gate match
      case Gate.RunningAlone(_, held) =>
        held.foldLeft((copy(gate = Gate.Open), Actions.none)) {
          case ((table, actions), started) =>
            val (next, more) = table.route(started)
            (next, actions ++ more)
        }
      case Gate.Open | Gate.Draining(_, _) => (this, Actions.none)

  private def settle(tickets: Vector[Long]): (LaneTable, Actions) =
    val remaining          = outstanding -- tickets
    val oldestOutstanding  = remaining.headOption.getOrElse(Long.MaxValue)
    val (ready, stillWait) = drainWaiters.partition((target, _) => target < oldestOutstanding)
    (copy(outstanding = remaining, drainWaiters = stillWait), Actions.release(ready.map(_._2)))

private[effects] object LaneTable:
  val empty: LaneTable = LaneTable(0L, Map.empty, Gate.Open, SortedSet.empty, Vector.empty, released = false)
