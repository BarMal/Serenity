package com.serenity.diagnostics

import scala.collection.mutable

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

final class FramePhaseClockSpec extends AnyFlatSpec with Matchers:

  private def clockReading(times: Long*): FramePhaseClock =
    val remaining = mutable.Queue.from(times)
    new FramePhaseClock(() => remaining.dequeue())

  "A frame phase clock" should "start with nothing counted" in {
    new FramePhaseClock().snapshot shouldBe FramePhaseClock.Snapshot(0L, 0L)
  }

  it should "count the time layout takes and hand back what it produced" in {
    val clock = clockReading(100L, 160L)

    clock.layout("scene") shouldBe "scene"

    clock.snapshot shouldBe FramePhaseClock.Snapshot(layoutNanos = 60L, drawNanos = 0L)
  }

  it should "count the time drawing takes apart from layout" in {
    val clock = clockReading(0L, 40L, 100L, 175L)

    clock.layout(())
    clock.draw(())

    clock.snapshot shouldBe FramePhaseClock.Snapshot(layoutNanos = 40L, drawNanos = 75L)
  }

  it should "add up every phase of the same kind" in {
    val clock = clockReading(0L, 10L, 50L, 80L)

    clock.draw(())
    clock.draw(())

    clock.snapshot.drawNanos shouldBe 40L
  }

  it should "count the time of a phase that fails and let the failure through" in {
    val clock = clockReading(5L, 25L)

    an[IllegalStateException] should be thrownBy clock.draw(throw new IllegalStateException("no surface"))

    clock.snapshot.drawNanos shouldBe 20L
  }
