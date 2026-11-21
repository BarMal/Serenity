package com.serenity.ui.terminal

import java.util.concurrent.atomic.AtomicLong

import com.serenity.diagnostics.{FrameKind, FramePhase, FrameTimings, PaintSource}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** The Skia layer presents by drawing, not by painting the canvas, so its draws have to stamp the same marks a canvas
  * paint does or no keystroke ever completes on it.
  */
class LayerPresentStampsSpec extends AnyFlatSpec with Matchers:

  private val Ms = 1_000_000L

  final private class ManualClock:
    private val now                 = new AtomicLong(0L)
    def advanceMs(ms: Double): Unit = now.addAndGet((ms * Ms).toLong): Unit
    def read(): Long                = now.get()

  private def recorder(): (FrameTimings, ManualClock) =
    val clock   = ManualClock()
    val timings = FrameTimings(() => clock.read(), backend = "skia")
    timings.setEnabled(true)
    timings.keyLatency.setEnabled(true)
    (timings, clock)

  /** One keystroke from arrival to the render loop having read the model, as the input and render loops stamp it. */
  private def keystrokeReadByAFrame(timings: FrameTimings, clock: ManualClock): Unit =
    val trace = timings.keyLatency
    timings.inputArrived()
    trace.keyReceived(0L)
    trace.keyEnqueued()
    clock.advanceMs(1)
    trace.keysDequeued(1)
    timings.inputApplyStarted()
    clock.advanceMs(2)
    trace.keysApplied(1)
    timings.inputApplyFinished()
    trace.damageEmitted()
    clock.advanceMs(1)
    trace.frameWoke()
    trace.frameDeadlineReached()
    timings.renderStarted()
    trace.frameModelRead()
    clock.advanceMs(4)

  "LayerPresentStamps" should "complete a keystroke's input-to-paint when the layer draws the frame that shows it" in {
    val (timings, clock) = recorder()
    val stamps           = new LayerPresentStamps(timings)

    keystrokeReadByAFrame(timings, clock)
    stamps.published()
    timings.renderFinished(FrameKind.Full)
    timings.keyLatency.frameEnded()
    clock.advanceMs(3)
    stamps.drawStarted()
    clock.advanceMs(2)
    stamps.drawFinished()

    val summary = timings.drain()
    summary.phases(FramePhase.InputToPaint).p50Ms shouldBe 13.0
    summary.phases(FramePhase.PaintWait).p50Ms shouldBe 3.0
    summary.phases(FramePhase.Paint).p50Ms shouldBe 2.0
  }

  it should "complete the same keystroke's latency trace, ending at the end of the draw" in {
    val (timings, clock) = recorder()
    val stamps           = new LayerPresentStamps(timings)

    keystrokeReadByAFrame(timings, clock)
    stamps.published()
    timings.renderFinished(FrameKind.Full)
    timings.keyLatency.frameEnded()
    clock.advanceMs(3)
    stamps.drawStarted()
    clock.advanceMs(2)
    stamps.drawFinished()

    val window = timings.keyLatency.drain()
    window.keys should have size 1
    window.keys.head.totalNanos shouldBe 13 * Ms
    window.unpainted shouldBe 0
  }

  it should "leave a draw that shows no new frame as nothing to report" in {
    val (timings, clock) = recorder()
    val stamps           = new LayerPresentStamps(timings)

    stamps.drawStarted()
    clock.advanceMs(2)
    stamps.drawFinished()

    timings.drain().phases.get(FramePhase.InputToPaint) shouldBe None
    timings.keyLatency.drain().keys shouldBe empty
  }

  it should "end each draw against its own start when the next draw starts before the first one's end arrives" in {
    val (timings, clock) = recorder()
    val stamps           = new LayerPresentStamps(timings)

    stamps.drawStarted()
    clock.advanceMs(5)
    stamps.drawStarted()
    clock.advanceMs(1)
    stamps.drawFinished()
    clock.advanceMs(4)
    stamps.drawFinished()

    val paint = timings.drain().phases(FramePhase.Paint)
    paint.count shouldBe 2
    paint.maxMs shouldBe 6.0
    paint.p50Ms shouldBe 5.0
  }

  it should "leave out a draw cycle's end when it saw no start of its own" in {
    val (timings, clock) = recorder()
    val stamps           = new LayerPresentStamps(timings)

    clock.advanceMs(2)
    stamps.drawFinished()

    val summary = timings.drain()
    summary.phases.get(FramePhase.Paint) shouldBe None
    summary.paints shouldBe empty
  }

  it should "count each draw it ends as a paint of the layer" in {
    val (timings, clock) = recorder()
    val stamps           = new LayerPresentStamps(timings)

    stamps.drawStarted()
    clock.advanceMs(1)
    stamps.drawFinished()

    timings.drain().paints shouldBe Map(PaintSource.Layer -> 1)
  }

  it should "forget the oldest starts whose ends never come, so a lost end cannot hold the queue for ever" in {
    val (timings, clock) = recorder()
    val stamps           = new LayerPresentStamps(timings)

    (1 to 20).foreach { _ =>
      stamps.drawStarted()
      clock.advanceMs(1)
    }
    stamps.drawFinished()

    // 20 starts, the 16 newest kept: the end pairs with the 5th, which began 16 ms before the clock now reads.
    timings.drain().phases(FramePhase.Paint).maxMs shouldBe 16.0
  }

  it should "record nothing while timing is off" in {
    val clock   = ManualClock()
    val timings = FrameTimings(() => clock.read(), backend = "skia")
    val stamps  = new LayerPresentStamps(timings)

    stamps.published()
    stamps.drawStarted()
    clock.advanceMs(2)
    stamps.drawFinished()

    timings.drain().phases shouldBe empty
    timings.keyLatency.drain().keys shouldBe empty
  }
