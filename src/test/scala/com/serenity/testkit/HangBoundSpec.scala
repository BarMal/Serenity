package com.serenity.testkit

import java.util.concurrent.TimeoutException
import java.util.concurrent.atomic.AtomicLong

import scala.concurrent.duration.*

import cats.effect.unsafe.implicits.global
import cats.effect.{Deferred, IO}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class HangBoundSpec extends AnyFlatSpec with Matchers:

  private val bound = 300.millis

  private def noPause: HangBound.PausedTime = () => 0L

  private def elapsedMillis[A](program: IO[A]): (Either[Throwable, A], Long) =
    val started = System.nanoTime()
    val result  = program.attempt.unsafeRunSync()
    (result, (System.nanoTime() - started) / 1_000_000L)

  "A program that finishes" should "return its value" in {
    HangBound.within(IO.pure(7), bound, noPause).unsafeRunSync() shouldBe 7
  }

  it should "be a defined result when run for an option" in {
    HangBound.unsafeRunTimed(IO.pure(7), bound, noPause) shouldBe Some(7)
  }

  "A gate that never opens" should "fail with a timeout once the bound has passed" in {
    val program           = Deferred[IO, Unit].flatMap(gate => HangBound.within(gate.get, bound, noPause))
    val (result, elapsed) = elapsedMillis(program)
    result.left.map(_.getClass) shouldBe Left(classOf[TimeoutException])
    elapsed should be >= bound.toMillis
  }

  it should "be no result when run for an option" in {
    val program = Deferred[IO, Unit].map(gate => HangBound.unsafeRunTimed(gate.get, bound, noPause))
    program.unsafeRunSync() shouldBe None
  }

  "Time the collectors spent paused" should "not count against a program that was moving" in {
    val paused = AtomicLong(0L)
    val clock = new HangBound.PausedTime:
      def millis(): Long = paused.get
    val program = IO(paused.set(2000L)) >> IO.sleep(bound + 100.millis) >> IO.pure(7)
    HangBound.within(program, bound, clock).unsafeRunSync() shouldBe 7
  }

  it should "extend the bound by that time and no more, so a hang still fails" in {
    val paused = AtomicLong(0L)
    val clock = new HangBound.PausedTime:
      def millis(): Long = paused.get
    val program =
      Deferred[IO, Unit].flatMap(gate => HangBound.within(IO(paused.addAndGet(300L)) >> gate.get, bound, clock))
    val (result, elapsed) = elapsedMillis(program)
    result.left.map(_.getClass) shouldBe Left(classOf[TimeoutException])
    elapsed should be >= (bound.toMillis + 300L - 20L)
  }

  "A timeout raised by the program itself" should "reach the caller, not be taken for a hang" in {
    val own = new TimeoutException("the program's own")
    HangBound.within(IO.raiseError[Int](own), bound, noPause).attempt.unsafeRunSync() shouldBe Left(own)
    val raised = scala.util.Try(HangBound.unsafeRunTimed(IO.raiseError[Int](own), bound, noPause))
    raised.failed.toOption shouldBe Some(own)
  }

  "The JVM's paused time" should "only ever grow" in {
    val before = HangBound.PausedTime.jvm.millis()
    System.gc()
    HangBound.PausedTime.jvm.millis() should be >= before
  }

  "The default bound" should "be ten seconds" in {
    HangBound.Default shouldBe 10.seconds
  }
