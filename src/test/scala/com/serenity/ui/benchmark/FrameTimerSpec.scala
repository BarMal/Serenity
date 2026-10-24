package com.serenity.ui.benchmark

import java.util.concurrent.atomic.{AtomicLong, AtomicReference}

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class FrameTimerSpec extends AnyFlatSpec with Matchers:

  private val ms = 1_000_000L

  /** A clock that moves only when the timer pauses, and that presents the frames due by then. */
  final private class Harness(due: List[(Long, Int)]):
    val now     = new AtomicLong(0L)
    val pending = new AtomicReference(due.sortBy(_._1))

    val timer: FrameTimer = new FrameTimer(
      clock = () => now.get,
      pause = () =>
        val _              = now.addAndGet(ms)
        val (ready, later) = pending.get().partition(_._1 * ms <= now.get)
        pending.set(later)
        ready.foreach((_, shown) => timer.presented(shown))
      ,
      quietNanos = 15 * ms,
      dropAfterNanos = 3000 * ms
    )

    def presentAt(atNanos: Long, shown: Int): Unit =
      now.set(atNanos)
      timer.presented(shown)

  "A frame timer" should "ignore a draw from before the frame's publish and end at the draw that shows it" in {
    val harness = new Harness(List(210L -> 5))
    harness.presentAt(1_500_000L, 4)
    harness.now.set(200 * ms)
    harness.timer.await(start = 0L, publishesNeeded = 5) shouldBe Some(210 * ms)
  }

  it should "end at the last draw once nothing more is drawn for a quiet moment" in {
    val harness = new Harness(List(3L -> 0, 10L -> 0))
    harness.timer.await(start = 0L, publishesNeeded = 0) shouldBe Some(10 * ms)
  }

  it should "count the draw of the publish even when it came before the render returned" in {
    val harness = new Harness(Nil)
    harness.presentAt(120 * ms, 5)
    harness.now.set(125 * ms)
    harness.timer.await(start = 0L, publishesNeeded = 5) shouldBe Some(120 * ms)
  }

  it should "give up on a frame whose publish was never drawn, however many other draws there were" in {
    val harness = new Harness(Nil)
    harness.presentAt(2 * ms, 4)
    harness.timer.await(start = 0L, publishesNeeded = 5) shouldBe None
  }

  it should "give up on a frame nothing drew" in {
    new Harness(Nil).timer.await(start = 0L, publishesNeeded = 0) shouldBe None
  }
