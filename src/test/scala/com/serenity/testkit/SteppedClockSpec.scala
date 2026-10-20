package com.serenity.testkit

import scala.concurrent.duration.*

import cats.effect.IO
import cats.effect.unsafe.Scheduler
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class SteppedClockSpec extends AnyFlatSpec with Matchers:

  private val realScheduler = Scheduler.createDefaultScheduler("stepped-clock-spec")._1

  "A stepped clock" should "advance by exactly one step per read" in {
    val clock = SteppedClock.scheduler(realScheduler)

    val reads = List.fill(4)(clock.monotonicNanos())

    reads.zip(reads.tail).map((before, after) => after - before) shouldBe List.fill(3)(SteppedClock.Step.toNanos)
  }

  it should "not show the time a program spent waiting between reads" in {
    // Other specs read this runtime's clock too, so the gap is bounded rather than exact.
    val waited = (for
      first  <- IO.monotonic
      _      <- IO.sleep(200.millis)
      second <- IO.monotonic
    yield second - first).unsafeRunSync()(using SteppedClock.runtime)

    waited should (be >= SteppedClock.Step and be < 100.millis)
  }
