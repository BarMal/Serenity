package com.serenity.diagnostics

import scala.concurrent.duration.*

import cats.effect.unsafe.implicits.global
import cats.effect.{Deferred, IO, Resource}
import com.serenity.testkit.RecordingLogger
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ShutdownTraceSpec extends AnyFlatSpec with Matchers:

  private def withLogger[A](body: RecordingLogger => IO[A]): A =
    RecordingLogger.create.flatMap(body).unsafeRunSync()

  private def withoutMillis(line: String): String = line.replaceAll("""\(\d+ ms\)""", "(N ms)")

  private def releaseSaying(logger: RecordingLogger, message: String): Resource[IO, Unit] =
    Resource.make(IO.unit)(_ => logger.info(message))

  "ShutdownTrace.traced" should "say when a resource starts and finishes releasing, around its own release" in {
    val lines = withLogger { logger =>
      given org.typelevel.log4cats.Logger[IO] = logger
      for
        _     <- ShutdownTrace.traced("window")(releaseSaying(logger, "the release itself")).use(_ => IO.unit)
        lines <- logger.messages
      yield lines
    }

    lines.map(withoutMillis) shouldBe Vector(
      "info [SHUTDOWN] releasing window",
      "info the release itself",
      "info [SHUTDOWN] released window (N ms)"
    )
  }

  it should "leave a releasing line with no released line while the release has not finished" in {
    val (whileStuck, afterwards) = withLogger { logger =>
      given org.typelevel.log4cats.Logger[IO] = logger
      for
        gate <- Deferred[IO, Unit]
        stuck = Resource.make(IO.unit)(_ => gate.get)
        fiber      <- ShutdownTrace.traced("paint executor")(stuck).use(_ => IO.unit).start
        _          <- IO.sleep(150.millis)
        whileStuck <- logger.messages
        _          <- gate.complete(())
        _          <- fiber.joinWithNever
        afterwards <- logger.messages
      yield (whileStuck, afterwards)
    }

    whileStuck shouldBe Vector("info [SHUTDOWN] releasing paint executor")
    afterwards.map(withoutMillis) shouldBe Vector(
      "info [SHUTDOWN] releasing paint executor",
      "info [SHUTDOWN] released paint executor (N ms)"
    )
  }

  it should "release nested resources in reverse order, each between its own two lines" in {
    val lines = withLogger { logger =>
      given org.typelevel.log4cats.Logger[IO] = logger
      val outer                               = ShutdownTrace.traced("outer")(Resource.unit[IO])
      val inner                               = ShutdownTrace.traced("inner")(Resource.unit[IO])
      outer.flatMap(_ => inner).use(_ => IO.unit) *> logger.messages
    }

    lines.map(withoutMillis) shouldBe Vector(
      "info [SHUTDOWN] releasing inner",
      "info [SHUTDOWN] released inner (N ms)",
      "info [SHUTDOWN] releasing outer",
      "info [SHUTDOWN] released outer (N ms)"
    )
  }

  it should "say nothing for a resource that was never acquired" in {
    val (outcome, lines) = withLogger { logger =>
      given org.typelevel.log4cats.Logger[IO] = logger
      for
        outcome <- ShutdownTrace
          .traced("never acquired")(Resource.eval(IO.raiseError[Unit](new RuntimeException("no"))))
          .use(_ => IO.unit)
          .attempt
        lines <- logger.messages
      yield (outcome, lines)
    }

    outcome.isLeft shouldBe true
    lines shouldBe Vector.empty
  }

  "ShutdownTrace.step" should "say when an action starts and finishes, with the time it took" in {
    val lines = withLogger { logger =>
      given org.typelevel.log4cats.Logger[IO] = logger
      ShutdownTrace.step("save session")(logger.info("saving")) *> logger.messages
    }

    lines.map(withoutMillis) shouldBe Vector(
      "info [SHUTDOWN] releasing save session",
      "info saving",
      "info [SHUTDOWN] released save session (N ms)"
    )
  }

  it should "still say it finished when the action fails, and let the failure through" in {
    val (outcome, lines) = withLogger { logger =>
      given org.typelevel.log4cats.Logger[IO] = logger
      for
        outcome <- ShutdownTrace.step("save session")(IO.raiseError[Unit](new RuntimeException("disk full"))).attempt
        lines   <- logger.messages
      yield (outcome, lines)
    }

    outcome.left.map(_.getMessage) shouldBe Left("disk full")
    lines.map(withoutMillis) shouldBe Vector(
      "info [SHUTDOWN] releasing save session",
      "info [SHUTDOWN] released save session (N ms)"
    )
  }

  it should "leave only the starting line while the action is still running" in {
    val whileRunning = withLogger { logger =>
      given org.typelevel.log4cats.Logger[IO] = logger
      for
        gate  <- Deferred[IO, Unit]
        fiber <- ShutdownTrace.step("persist")(gate.get).start
        _     <- IO.sleep(150.millis)
        lines <- logger.messages
        _     <- gate.complete(())
        _     <- fiber.joinWithNever
      yield lines
    }

    whileRunning shouldBe Vector("info [SHUTDOWN] releasing persist")
  }
