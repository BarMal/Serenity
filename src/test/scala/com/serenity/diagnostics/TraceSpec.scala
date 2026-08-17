package com.serenity.diagnostics

import cats.effect.unsafe.implicits.global
import cats.effect.{IO, Ref}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.typelevel.log4cats.Logger

class TraceSpec extends AnyFlatSpec with Matchers:

  /** Records every `.debug` message logged against it; every other level is a no-op. */
  private class RecordingLogger(ref: Ref[IO, List[String]]) extends Logger[IO]:
    def debug(message: => String): IO[Unit]               = ref.update(_ :+ message)
    def debug(t: Throwable)(message: => String): IO[Unit] = ref.update(_ :+ message)
    def error(message: => String): IO[Unit]               = IO.unit
    def error(t: Throwable)(message: => String): IO[Unit] = IO.unit
    def warn(message: => String): IO[Unit]                = IO.unit
    def warn(t: Throwable)(message: => String): IO[Unit]  = IO.unit
    def info(message: => String): IO[Unit]                = IO.unit
    def info(t: Throwable)(message: => String): IO[Unit]  = IO.unit
    def trace(message: => String): IO[Unit]               = IO.unit
    def trace(t: Throwable)(message: => String): IO[Unit] = IO.unit

  "Trace.timed" should "return the wrapped action's result unchanged" in {
    val logged       = Ref.unsafe[IO, List[String]](Nil)
    given Logger[IO] = RecordingLogger(logged)

    Trace.timed("unit-test")(IO.pure(42)).unsafeRunSync() shouldBe 42
  }

  it should "log exactly one debug line naming the label on success" in {
    val logged       = Ref.unsafe[IO, List[String]](Nil)
    given Logger[IO] = RecordingLogger(logged)

    Trace.timed("render.full")(IO.pure(())).unsafeRunSync()
    val messages = logged.get.unsafeRunSync()

    messages.size shouldBe 1
    messages.head should include("render.full")
    messages.head should include("ms")
  }

  it should "still log and propagate the original error when the action fails" in {
    val logged       = Ref.unsafe[IO, List[String]](Nil)
    given Logger[IO] = RecordingLogger(logged)
    val boom         = new RuntimeException("boom")

    val outcome  = Trace.timed("event.dispatch")(IO.raiseError[Unit](boom)).attempt.unsafeRunSync()
    val messages = logged.get.unsafeRunSync()

    outcome shouldBe Left(boom)
    messages.size shouldBe 1
    messages.head should include("event.dispatch")
  }

  it should "log independently for sequential calls under the same label" in {
    val logged       = Ref.unsafe[IO, List[String]](Nil)
    given Logger[IO] = RecordingLogger(logged)

    Trace.timed("tick")(IO.unit).unsafeRunSync()
    Trace.timed("tick")(IO.unit).unsafeRunSync()

    logged.get.unsafeRunSync().size shouldBe 2
  }
