package com.serenity.state.manager

import scala.concurrent.duration.*

import cats.effect.testkit.TestControl
import cats.effect.unsafe.implicits.global
import cats.effect.{IO, Ref}
import com.serenity.testkit.VirtualTime.runVirtual
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ProjectTaskOutputBatcherSpec extends AnyFlatSpec with Matchers:

  private val Interval = StateManagerProjectLspEffects.OutputPublishInterval

  private def withBatcher[A](body: (ProjectTaskOutputBatcher, IO[List[(FiniteDuration, String)]]) => IO[A]): IO[A] =
    for
      published <- Ref.of[IO, List[(FiniteDuration, String)]](Nil)
      batcher <- ProjectTaskOutputBatcher.create(
        Interval,
        batch => IO.monotonic.flatMap(now => published.update(_ :+ ((now, batch))))
      )
      fiber  <- batcher.run.start
      result <- body(batcher, published.get)
      _      <- fiber.cancel
    yield result

  "ProjectTaskOutputBatcher" should "make no publish attempt while the task stays silent" in {
    val program = withBatcher((_, published) => IO.sleep(60.seconds) >> published)

    runVirtual(program) shouldBe empty
  }

  it should "schedule no timer at all while the task stays silent" in {
    val program = ProjectTaskOutputBatcher.create(Interval, _ => IO.unit).flatMap(_.run)

    val parked = (for
      control  <- TestControl.execute(program)
      _        <- control.tickFor(60.seconds)
      finished <- control.results
      idle     <- control.isDeadlocked
    yield (finished, idle)).unsafeRunSync()

    parked shouldBe ((None, true))
  }

  it should "publish a burst as one complete batch an interval after its first chunk" in {
    val program = withBatcher { (batcher, published) =>
      for
        _     <- IO.sleep(5.seconds)
        start <- IO.monotonic
        _     <- batcher.append("one ")
        _     <- IO.sleep(10.millis)
        _     <- batcher.append("two ")
        _     <- IO.sleep(10.millis)
        _     <- batcher.append("three")
        _     <- IO.sleep(5.seconds)
        out   <- published
      yield out.map { case (at, batch) => (at - start, batch) }
    }

    runVirtual(program) shouldBe List((Interval, "one two three"))
  }

  it should "publish at the next refresh boundary after the task started, not an interval after the chunk" in {
    val program = withBatcher { (batcher, published) =>
      for
        start <- IO.monotonic
        _     <- IO.sleep(Interval * 2 + 50.millis)
        _     <- batcher.append("late")
        _     <- IO.sleep(5.seconds)
        out   <- published
      yield out.map { case (at, batch) => (at - start, batch) }
    }

    runVirtual(program) shouldBe List((Interval * 3, "late"))
  }

  it should "start a new batch for output arriving after a publish, then fall silent again" in {
    val program = withBatcher { (batcher, published) =>
      for
        _   <- batcher.append("first")
        _   <- IO.sleep(Interval * 3)
        _   <- batcher.append("second")
        _   <- IO.sleep(30.seconds)
        out <- published
      yield out.map(_._2)
    }

    runVirtual(program) shouldBe List("first", "second")
  }
