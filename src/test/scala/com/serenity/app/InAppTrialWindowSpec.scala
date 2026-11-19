package com.serenity.app

import java.util.concurrent.atomic.AtomicLong

import scala.concurrent.duration.*

import cats.effect.IO
import com.serenity.app.PixelClock.Region
import com.serenity.app.RendererWorkload.{Redraw, Step}
import com.serenity.diagnostics.FrameTimings
import com.serenity.testkit.VirtualTime.runVirtual
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** The in-app clock times a trial step by what the app itself stamps for it, for a screen that cannot be read. */
class InAppTrialWindowSpec extends AnyFlatSpec with Matchers:

  private val Ms = 1_000_000L

  final private class Rig(paintsAfter: Option[FiniteDuration]):
    val now     = new AtomicLong(1_000_000_000L)
    val timings = FrameTimings(() => now.get())
    timings.keyLatency.setEnabled(true)
    timings.setEnabled(true)

    private def painted(after: FiniteDuration, finish: IO[Unit]): IO[Unit] =
      paintsAfter.fold(IO.unit)(_ => (IO.sleep(after) *> finish).start.void)

    private def stampKey(): Unit =
      val trace = timings.keyLatency
      trace.keyReceived(System.currentTimeMillis())
      trace.keyEnqueued()
      trace.keysDequeued(1)
      trace.dispatchStarted()
      trace.keysApplied(1)
      trace.damageEmitted()
      trace.frameWoke()
      trace.frameModelRead()

    private def finishKey(): Unit =
      now.addAndGet(7 * Ms): Unit
      timings.framePublished()
      timings.keyLatency.frameEnded()
      val started = timings.paintStarted()
      timings.paintFinished(started)

    def leaveStaleKey(): Unit =
      stampKey()
      finishKey()

    val driver: RendererTrialWindow.Driver = new RendererTrialWindow.Driver:
      def typeCharacter(char: Char): IO[Unit] =
        IO(stampKey()) *> paintsAfter.fold(IO.unit)(after => painted(after, IO(finishKey())))
      def pressPageDown: IO[Unit] = typeCharacter('x')
      def toggleTheme: IO[Unit] =
        IO {
          timings.inputArrived()
          timings.inputApplyStarted()
          timings.inputApplyFinished()
          timings.renderStarted()
          timings.framePublished()
          timings.renderFinished(com.serenity.diagnostics.FrameKind.Full)
        } *> paintsAfter.fold(IO.unit)(after =>
          painted(after, IO { now.addAndGet(11 * Ms); timings.paintFinished(timings.paintStarted()) })
        )
      def dismissNotices: IO[Unit]           = IO.unit
      def resize: IO[Unit]                   = IO.unit
      def watchedFor(step: Step): IO[Region] = IO.pure(Region(0, 0, 1, 1))

  private def perform(rig: Rig, step: Step): Option[Double] =
    runVirtual(InAppTrialWindow(rig.driver, rig.timings, timeout = 2.seconds).perform(step))

  "The in-app trial window" should "answer a typed character's key-to-screen time, as the app stamps it" in {
    val answer = perform(new Rig(Some(10.millis)), Step.Type('a'))
    answer.getOrElse(fail("no answer")) shouldBe (7.0 +- 2.0)
  }

  it should "answer a page-down the same way" in {
    perform(new Rig(Some(10.millis)), Step.PageDown).getOrElse(fail("no answer")) shouldBe (7.0 +- 2.0)
  }

  it should "answer a theme toggle by the frame timings' input-to-paint" in {
    perform(new Rig(Some(10.millis)), Step.FullRedraw(Redraw.ThemeToggle)).getOrElse(fail("no answer")) shouldBe 11.0
  }

  it should "answer none when the app never paints the step within the timeout" in {
    perform(new Rig(None), Step.Type('a')) shouldBe None
    perform(new Rig(None), Step.FullRedraw(Redraw.ThemeToggle)) shouldBe None
  }

  it should "leave a resize alone: the app has no stamp for it, so it answers none without waiting" in {
    val rig = new Rig(Some(10.millis))
    val (answer, elapsed) = runVirtual(
      for
        started <- IO.monotonic
        result <- InAppTrialWindow(rig.driver, rig.timings, timeout = 2.seconds).perform(Step.FullRedraw(Redraw.Resize))
        ended  <- IO.monotonic
      yield (result, ended - started)
    )
    answer shouldBe None
    elapsed shouldBe Duration.Zero
  }

  it should "not read a key left by an earlier step as the answer to this one" in {
    val rig = new Rig(None)
    rig.leaveStaleKey()
    perform(rig, Step.Type('b')) shouldBe None
  }
