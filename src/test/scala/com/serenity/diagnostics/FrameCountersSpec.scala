package com.serenity.diagnostics

import cats.effect.unsafe.implicits.global
import cats.effect.{IO, Ref}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class FrameCountersSpec extends AnyFlatSpec with Matchers:

  private def enabled(): FrameCounters =
    val counters = FrameCounters()
    counters.setEnabled(true)
    counters

  "FrameCounters" should "count nothing until enabled" in {
    val counters = FrameCounters()

    counters.add(FrameCounter.RunsDrawn, 5)
    counters.rows(total = 40, drawn = 1)
    counters.frameFinished()

    val window = counters.drain()
    counters.lastFrame(FrameCounter.RunsDrawn) shouldBe 0L
    window.frames shouldBe 0
    window.totals.values.sum shouldBe 0L
  }

  it should "report a finished frame's counts as the last frame, and start the next from zero" in {
    val counters = enabled()

    counters.rows(total = 40, drawn = 1)
    counters.add(FrameCounter.RunsDrawn, 7)
    counters.frameFinished()
    counters.rows(total = 40, drawn = 3)
    counters.frameFinished()

    val last = counters.lastFrame
    last(FrameCounter.RowsDrawn) shouldBe 3L
    last(FrameCounter.RowsPreserved) shouldBe 37L
    last(FrameCounter.RunsDrawn) shouldBe 0L
  }

  it should "hold a frame's counts back until the frame finishes" in {
    val counters = enabled()

    counters.rows(total = 10, drawn = 2)

    counters.lastFrame(FrameCounter.RowsDrawn) shouldBe 0L
  }

  it should "never report more rows drawn than there are rows" in {
    val counters = enabled()

    counters.rows(total = 4, drawn = 9)
    counters.frameFinished()

    counters.lastFrame(FrameCounter.RowsDrawn) shouldBe 4L
    counters.lastFrame(FrameCounter.RowsPreserved) shouldBe 0L
  }

  it should "total and take the largest of each counter over the frames of a window, then start a new window" in {
    val counters = enabled()
    counters.rows(total = 40, drawn = 1)
    counters.frameFinished()
    counters.rows(total = 40, drawn = 3)
    counters.frameFinished()

    val window = counters.drain()

    window.frames shouldBe 2
    window.totals(FrameCounter.RowsDrawn) shouldBe 4L
    window.maxima(FrameCounter.RowsDrawn) shouldBe 3L
    window.mean(FrameCounter.RowsDrawn) shouldBe 2.0
    counters.drain().frames shouldBe 0
  }

  it should "forget everything counted before it is switched on or off" in {
    val counters = enabled()
    counters.rows(total = 40, drawn = 1)
    counters.frameFinished()

    counters.setEnabled(false)
    counters.setEnabled(true)

    counters.lastFrame(FrameCounter.RowsDrawn) shouldBe 0L
    counters.drain().frames shouldBe 0
  }

  "FrameCounterStats" should "name only the counters a window ever counted, with their mean and largest frame" in {
    val counters = enabled()
    counters.rows(total = 40, drawn = 1)
    counters.frameFinished()
    counters.rows(total = 40, drawn = 3)
    counters.frameFinished()

    val parts = counters.drain().logParts

    parts should contain("rows-drawn avg=2.0 max=3")
    parts should contain("rows-preserved avg=38.0 max=39")
    parts.exists(_.startsWith("tui-bytes")) shouldBe false
  }

  it should "name nothing for a window with no frames" in {
    FrameCounterStats.empty.logParts shouldBe empty
  }

  "A [FRAME] line" should "carry the window's counters beside its phase timings" in {
    val counters = enabled()
    counters.rows(total = 40, drawn = 1)
    counters.frameFinished()
    val summary = FrameTimingSummary(5000.0, Map.empty, Map.empty).withCounters(counters.drain())

    summary.logLine should include("rows-drawn avg=1.0 max=1")
    summary.logLine should include("rows-preserved avg=39.0 max=39")
  }

  it should "stay as it was when no frame was counted" in {
    FrameTimingSummary(5000.0, Map.empty, Map.empty).logLine should endWith("| no samples")
  }

  "FrameTimingReport.tick" should "log the counters of the window it drains" in {
    val counters = enabled()
    counters.rows(total = 40, drawn = 2)
    counters.frameFinished()

    val logged = (for
      lines <- Ref.of[IO, List[String]](Nil)
      _     <- FrameTimingReport.tick(FrameTimings(), IO.pure(true), line => lines.update(_ :+ line), counters)
      out   <- lines.get
    yield out).unsafeRunSync()

    logged.headOption.exists(_.contains("rows-drawn avg=2.0 max=2")) shouldBe true
    counters.drain().frames shouldBe 0
  }

  "FrameTimingReport.stream" should "switch the counters with the setting" in {
    import scala.concurrent.duration.*
    import com.serenity.testkit.VirtualTime.runVirtual
    import fs2.concurrent.SignallingRef
    val counters = FrameCounters()
    val program = for
      setting <- SignallingRef.of[IO, Boolean](false)
      fiber <- FrameTimingReport
        .stream(FrameTimings(), setting.discrete, _ => IO.unit, counters)
        .compile
        .drain
        .start
      _   <- IO.sleep(1.second)
      off <- IO(counters.isEnabled)
      _   <- setting.set(true)
      _   <- IO.sleep(1.second)
      on  <- IO(counters.isEnabled)
      _   <- fiber.cancel
    yield (off, on)

    runVirtual(program) shouldBe ((false, true))
  }
