package com.serenity.state.effects

import cats.effect.IO
import com.serenity.testkit.VirtualTime.runVirtual
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** The bookkeeping behind [[EffectLanes]], checked directly: per-key entries and outstanding tickets must not outlive
  * the work they describe, or a long session would grow without bound.
  */
class LaneTableSpec extends AnyFlatSpec with Matchers:

  private val saves: Lane.Keyed   = Lane.Keyed(LaneKey.Config, LanePolicy.Sequential)
  private val search: Lane.Keyed  = Lane.Keyed(LaneKey.Search, LanePolicy.SwitchLatest)
  private val dialogs: Lane.Keyed = Lane.Keyed(LaneKey.Dialog, LanePolicy.DropIfBusy)
  private val freshSignal         = IO.deferred[Unit]

  private def accept(table: LaneTable, lane: Lane.Scheduled): IO[(LaneTable, Actions)] =
    freshSignal.map(table.accept(lane, IO.unit, _))

  private def startedTickets(actions: Actions): Vector[Long] = actions.start.map(_.job.ticket)

  "A LaneTable" should "forget a key once its last job finishes" in {
    val (afterFirst, afterSecond) = runVirtual(
      for
        (queuedOne, started) <- accept(LaneTable.empty, saves)
        (queuedTwo, _)       <- accept(queuedOne, saves)
        (afterFirst, next)   <- IO.pure(queuedTwo.finish(saves, startedTickets(started).head))
        (afterSecond, idle)  <- IO.pure(afterFirst.finish(saves, startedTickets(next).head))
        _                    <- IO(idle.start shouldBe empty)
      yield (afterFirst, afterSecond)
    )
    afterFirst.keyed.keySet shouldBe Set(saves)
    afterSecond.keyed shouldBe empty
    afterSecond.outstanding shouldBe empty
  }

  it should "settle a SwitchLatest job superseded before it started" in {
    val table = runVirtual(
      for
        (running, started) <- accept(LaneTable.empty, search)
        (oneQueued, _)     <- accept(running, search)
        (superseded, _)    <- accept(oneQueued, search)
      yield superseded.finish(search, startedTickets(started).head)
    )
    val (afterSwitch, next) = table
    afterSwitch.outstanding shouldBe Set(startedTickets(next).head)
    afterSwitch.finish(search, startedTickets(next).head)._1.keyed shouldBe empty
  }

  it should "settle a DropIfBusy job as soon as it is dropped" in {
    val (table, dropped) = runVirtual(
      for
        (busy, started)      <- accept(LaneTable.empty, dialogs)
        (afterDrop, actions) <- accept(busy, dialogs)
      yield (afterDrop, (started, actions))
    )
    val (started, dropActions) = dropped
    dropActions.start shouldBe empty
    table.outstanding shouldBe startedTickets(started).toSet
  }

  it should "forget cancelled keys and the Exclusive gate once everything has finished" in {
    val table = runVirtual(
      for
        (searching, searchStart) <- accept(LaneTable.empty, search)
        (gated, gateActions)     <- accept(searching, Lane.Exclusive)
        (held, _)                <- accept(gated, saves)
        _                        <- IO(gateActions.cancel should have size 1)
        (admitted, exclusive)    <- IO.pure(held.finish(search, startedTickets(searchStart).head))
        (reopened, replayed)     <- IO.pure(admitted.finish(Lane.Exclusive, startedTickets(exclusive).head))
      yield reopened.finish(saves, startedTickets(replayed).head)._1
    )
    table.keyed shouldBe empty
    table.gate shouldBe Gate.Open
    table.outstanding shouldBe empty
  }
