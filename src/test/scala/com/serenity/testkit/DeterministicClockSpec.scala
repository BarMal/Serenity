package com.serenity.testkit

import scala.concurrent.duration.*

import cats.effect.IO
import org.scalatest.BeforeAndAfterAll
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class DeterministicClockSpec extends AnyFlatSpec with Matchers with BeforeAndAfterAll:

  private val clock = DeterministicClock(step = 1.millis)

  override protected def afterAll(): Unit =
    clock.shutdown()
    super.afterAll()

  private def run[A](program: IO[A]): A = program.unsafeRunSync()(using clock.runtime)

  "IO.monotonic on the clock's runtime" should "advance by exactly one step per read" in {
    val reads = run(IO.monotonic.replicateA(4)).map(_.toNanos)
    reads.sliding(2).map(pair => pair(1) - pair(0)).toList shouldBe List.fill(3)(1.millis.toNanos)
  }

  it should "not move with the wall clock while a program sleeps" in {
    val started = System.nanoTime()
    val reads   = run(IO.monotonic >> IO.sleep(80.millis) >> IO.monotonic).toNanos
    val slept   = (System.nanoTime() - started).nanos
    slept should be >= 70.millis
    reads should be > 0L
    (run(IO.monotonic).toNanos - reads) should be < 1.second.toNanos
  }

  "Advancing the clock" should "show up as one jump of that size in the next read" in {
    val before = run(IO.monotonic).toNanos
    clock.advance(2.seconds)
    val after = run(IO.monotonic).toNanos
    (after - before) shouldBe (2.seconds.toNanos + 1.millis.toNanos)
    val next = run(IO.monotonic).toNanos
    (next - after) shouldBe 1.millis.toNanos
  }

  "IO.realTime on the clock's runtime" should "still follow the wall clock" in {
    val real = run(IO.realTime).toMillis
    math.abs(real - System.currentTimeMillis()) should be < 5000L
  }

  "A clock with another step" should "advance by that step" in {
    val coarse = DeterministicClock(step = 250.millis)
    try
      val values = IO.monotonic.replicateA(3).unsafeRunSync()(using coarse.runtime).map(_.toNanos)
      values.sliding(2).map(pair => pair(1) - pair(0)).toList shouldBe List.fill(2)(250.millis.toNanos)
    finally coarse.shutdown()
  }
