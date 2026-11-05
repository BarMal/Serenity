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

  it should "take the event-thread lag into the logged line and start the next window from nothing" in {
    val edtLag = EdtLagTrace()
    edtLag.setEnabled(true)
    edtLag.record(12 * 1_000_000L)

    val logged = (for
      lines <- Ref.of[IO, List[String]](Nil)
      _     <- FrameTimingReport.tick(withOneFrame(), IO.pure(true), line => lines.update(_ :+ line), edtLag = edtLag)
      out   <- lines.get
    yield out).unsafeRunSync()

    logged.headOption.exists(_.contains("edt-lag p50=12.00 p95=12.00 max=12.00 n=1 ms")) shouldBe true
    edtLag.drain().count shouldBe 0
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

  it should "probe the event thread about twenty times a second while on, and report the lag in its line" in {
    val program = for
      enabled <- SignallingRef.of[IO, Boolean](true)
      lines   <- Ref.of[IO, List[String]](Nil)
      report = FrameTimingReport.stream(
        withOneFrame(),
        enabled.discrete,
        line => lines.update(_ :+ line),
        edtLag = EdtLagTrace(),
        eventThread = Some(task => task.run())
      )
      fiber <- report.compile.drain.start
      _     <- IO.sleep(FrameTimingReport.Interval + 100.millis)
      out   <- lines.get
      _     <- fiber.cancel
    yield out

    val samples = """edt-lag [^|]* n=(\d+) ms""".r.findFirstMatchIn(runVirtual(program).mkString).map(_.group(1).toInt)
    samples.exists(n => n >= 90 && n <= 101) shouldBe true
  }

  it should "report no event-thread lag when the frontend has no event thread" in {
    val program = for
      enabled <- SignallingRef.of[IO, Boolean](true)
      lines   <- Ref.of[IO, List[String]](Nil)
      fiber <- FrameTimingReport
        .stream(withOneFrame(), enabled.discrete, line => lines.update(_ :+ line), eventThread = None)
        .compile
        .drain
        .start
      _   <- IO.sleep(FrameTimingReport.Interval + 100.millis)
      out <- lines.get
      _   <- fiber.cancel
    yield out

    runVirtual(program).mkString should not include "edt-lag"
  }

  it should "schedule no probe wakeups either while frame timing is off" in {
    val program = for
      enabled <- SignallingRef.of[IO, Boolean](false)
      _ <- FrameTimingReport
        .stream(FrameTimings(), enabled.discrete, _ => IO.unit, eventThread = Some(task => task.run()))
        .compile
        .drain
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
