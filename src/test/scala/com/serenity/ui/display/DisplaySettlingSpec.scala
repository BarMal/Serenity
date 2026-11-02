package com.serenity.ui.display

import scala.concurrent.duration.*

import cats.effect.std.Queue
import cats.effect.unsafe.implicits.global
import cats.effect.{IO, Ref}
import cats.syntax.foldable.*
import com.serenity.ui.display.DisplayScale.DeviceScale
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** The window is shown before the compositor has answered with its own size, so the size is read again until two reads
  * agree (#2479).
  */
class DisplaySettlingSpec extends AnyFlatSpec with Matchers:

  private def sized(width: Int, height: Int): DisplaySnapshot =
    DisplaySnapshot(
      toolkitClass = "sun.awt.X11.XToolkit",
      scale = DeviceScale(1.0, 1.0),
      frameWidthPx = width,
      frameHeightPx = height,
      canvasWidthPx = width - 10,
      canvasHeightPx = height - 30,
      preferredWidthPx = None,
      preferredHeightPx = None
    )

  /** Plays `reads` in order, then repeats the last one; counts how many reads were taken. */
  private def settle(reads: List[DisplaySnapshot], interval: FiniteDuration, maxWait: FiniteDuration) =
    (for
      queue <- Queue.unbounded[IO, DisplaySnapshot]
      _     <- reads.traverse_(queue.offer)
      taken <- Ref.of[IO, Int](0)
      last  <- Ref.of[IO, DisplaySnapshot](reads.head)
      sample = taken.update(_ + 1) >> queue.tryTake.flatMap {
        case Some(next) => last.set(next).as(next)
        case None       => last.get
      }
      outcome <- DisplaySettling.untilSettled(sample, interval, maxWait)
      count   <- taken.get
    yield (outcome, count)).unsafeRunSync()

  "untilSettled" should "report a window whose size is unchanged between two reads as settled" in {
    val (outcome, count) = settle(List(sized(1034, 798), sized(1034, 798)), 1.millis, 100.millis)

    outcome shouldBe DisplaySettling.Outcome(sized(1034, 798), settled = true)
    count shouldBe 2
  }

  it should "keep reading through a resize and report the size it settled on" in {
    val (outcome, count) =
      settle(List(sized(1034, 798), sized(1470, 916), sized(1470, 916)), 1.millis, 100.millis)

    outcome shouldBe DisplaySettling.Outcome(sized(1470, 916), settled = true)
    count shouldBe 3
  }

  it should "compare the frame and the canvas, not the toolkit or scale" in {
    val resizedCanvasOnly = sized(1034, 798).copy(canvasWidthPx = 900)

    val (outcome, _) = settle(List(sized(1034, 798), resizedCanvasOnly, resizedCanvasOnly), 1.millis, 100.millis)

    outcome shouldBe DisplaySettling.Outcome(resizedCanvasOnly, settled = true)
  }

  it should "give up after maxWait and say the size had not settled" in {
    val growing = List.tabulate(50)(i => sized(1000 + i, 800))

    val (outcome, count) = settle(growing, 1.millis, 5.millis)

    outcome.settled shouldBe false
    count shouldBe 6
  }
