package com.serenity.diagnostics

import cats.effect.unsafe.implicits.global
import cats.effect.{IO, Ref}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class FrameTimingReportSpec extends AnyFlatSpec with Matchers:

  private def withOneFrame(): FrameTimings =
    val timings = FrameTimings()
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
