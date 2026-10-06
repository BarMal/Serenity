package com.serenity.diagnostics

import scala.concurrent.duration.*

import cats.effect.testkit.TestControl
import cats.effect.unsafe.implicits.global
import cats.effect.{IO, Ref}
import com.serenity.testkit.VirtualTime.runVirtual
import fs2.concurrent.SignallingRef
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class FrameTimingReportSpec extends AnyFlatSpec with Matchers:

  private def withOneFrame(): FrameTimings =
    val timings = FrameTimings()
    timings.setEnabled(true)
    timings.renderStarted()
    timings.framePublished()
    timings.renderFinished(FrameKind.Full)
    timings

  "FrameTimingReport.tick" should "log the window's summary when frame timing is enabled" in {
    val timings = withOneFrame()

    val logged = (for
      lines <- Ref.of[IO, List[String]](Nil)
      _     <- FrameTimingReport.tick(timings, IO.pure(true), line => lines.update(_ :+ line))
      out   <- lines.get
    yield out).unsafeRunSync()

    logged should have size 1
    logged.headOption.exists(_.startsWith("[FRAME] ")) shouldBe true
    logged.headOption.exists(_.contains("full=1")) shouldBe true
  }

  it should "stay silent when disabled, still starting a new window so samples never pile up" in {
    val timings = withOneFrame()

    val logged = (for
      lines <- Ref.of[IO, List[String]](Nil)
      _     <- FrameTimingReport.tick(timings, IO.pure(false), line => lines.update(_ :+ line))
      out   <- lines.get
    yield out).unsafeRunSync()

    logged shouldBe empty
    timings.drain().frames shouldBe empty
  }

  "FrameTimingReport.stream" should "switch recording with the setting" in {
    val timings = FrameTimings()
    val program = for
      enabled  <- SignallingRef.of[IO, Boolean](false)
      fiber    <- FrameTimingReport.stream(timings, enabled.discrete, _ => IO.unit).compile.drain.start
      _        <- IO.sleep(1.second)
      off      <- IO(timings.isEnabled)
      _        <- enabled.set(true)
      _        <- IO.sleep(1.second)
      on       <- IO(timings.isEnabled)
      _        <- enabled.set(false)
      _        <- IO.sleep(1.second)
      offAgain <- IO(timings.isEnabled)
      _        <- fiber.cancel
    yield (off, on, offAgain)

    runVirtual(program) shouldBe ((false, true, false))
  }

  it should "schedule no wakeups at all while frame timing is off" in {
    val program = for
      enabled <- SignallingRef.of[IO, Boolean](false)
      _       <- FrameTimingReport.stream(FrameTimings(), enabled.discrete, _ => IO.unit).compile.drain
    yield ()

    val parked = (for
      control  <- TestControl.execute(program)
      _        <- control.tickFor(1.minute)
      finished <- control.results
      idle     <- control.isDeadlocked
    yield (finished, idle)).unsafeRunSync()

    parked shouldBe ((None, true))
  }

  it should "log one line per interval only while frame timing is on" in {
    val program = for
      enabled <- SignallingRef.of[IO, Boolean](false)
      lines   <- Ref.of[IO, Int](0)
      report = FrameTimingReport.stream(withOneFrame(), enabled.discrete, _ => lines.update(_ + 1))
      fiber    <- report.compile.drain.start
      _        <- IO.sleep(FrameTimingReport.Interval * 3)
      whileOff <- lines.get
      _        <- enabled.set(true)
      _        <- IO.sleep(FrameTimingReport.Interval * 2 + 1.second)
      whileOn  <- lines.get
      _        <- enabled.set(false)
      _        <- IO.sleep(FrameTimingReport.Interval * 3)
      afterOff <- lines.get
      _        <- fiber.cancel
    yield (whileOff, whileOn, afterOff)

    runVirtual(program) shouldBe ((0, 2, 2))
  }
