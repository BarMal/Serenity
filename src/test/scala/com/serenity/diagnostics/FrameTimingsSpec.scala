package com.serenity.diagnostics

import java.util.concurrent.atomic.AtomicLong

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class FrameTimingsSpec extends AnyFlatSpec with Matchers:

  private val Ms = 1_000_000L

  /** A clock the test advances by hand, in nanoseconds. */
  final private class ManualClock(start: Long = 0L):
    private val now                 = new AtomicLong(start)
    def advanceMs(ms: Double): Unit = now.addAndGet((ms * Ms).toLong): Unit
    def read(): Long                = now.get()

  private def recorder(): (FrameTimings, ManualClock) =
    val clock   = ManualClock()
    val timings = FrameTimings(() => clock.read())
    timings.setEnabled(true)
    (timings, clock)

  "FrameTimings" should "attribute one keystroke's journey to each phase it passes through" in {
    val (timings, clock) = recorder()

    timings.inputArrived()
    clock.advanceMs(1)
    timings.inputApplyStarted()
    clock.advanceMs(2)
    timings.inputApplyFinished()
    clock.advanceMs(3)
    timings.renderStarted()
    clock.advanceMs(4)
    timings.framePublished()
    timings.renderFinished(FrameKind.Full)
    clock.advanceMs(5)
    val paintStart = timings.paintStarted()
    clock.advanceMs(6)
    timings.paintFinished(paintStart)

    val summary = timings.drain()
    summary.phases(FramePhase.InputQueue).p50Ms shouldBe 1.0
    summary.phases(FramePhase.InputApply).p50Ms shouldBe 2.0
    summary.phases(FramePhase.RenderWait).p50Ms shouldBe 3.0
    summary.phases(FramePhase.Render).p50Ms shouldBe 4.0
    summary.phases(FramePhase.PaintWait).p50Ms shouldBe 5.0
    summary.phases(FramePhase.Paint).p50Ms shouldBe 6.0
    summary.phases(FramePhase.InputToPaint).p50Ms shouldBe 21.0
    summary.frames shouldBe Map(FrameKind.Full -> 1)
  }

  it should "measure input-to-paint from the earliest input a painted frame reflects" in {
    val (timings, clock) = recorder()

    timings.inputArrived()
    clock.advanceMs(5)
    timings.inputArrived()
    timings.inputApplyStarted()
    timings.inputApplyFinished()
    timings.renderStarted()
    timings.framePublished()
    timings.renderFinished(FrameKind.Full)
    timings.paintFinished(timings.paintStarted())

    timings.drain().phases(FramePhase.InputToPaint).p50Ms shouldBe 5.0
  }

  it should "not charge input latency to a paint whose frame was drawn before the input was applied" in {
    val (timings, clock) = recorder()

    timings.renderStarted()
    timings.framePublished()
    timings.renderFinished(FrameKind.Full)
    timings.inputArrived()
    clock.advanceMs(2)
    timings.paintFinished(timings.paintStarted())

    timings.drain().phases.get(FramePhase.InputToPaint) shouldBe None
  }

  it should "time a paint Swing requested on its own without inventing a paint wait" in {
    val (timings, clock) = recorder()

    val start = timings.paintStarted()
    clock.advanceMs(3)
    timings.paintFinished(start)

    val summary = timings.drain()
    summary.phases(FramePhase.Paint).p50Ms shouldBe 3.0
    summary.phases.get(FramePhase.PaintWait) shouldBe None
  }

  it should "time a block of work against a phase" in {
    val (timings, clock) = recorder()

    val result = timings.timed(FramePhase.Sync) { clock.advanceMs(7); "done" }

    result shouldBe "done"
    timings.drain().phases(FramePhase.Sync).maxMs shouldBe 7.0
  }

  it should "time an IO against a phase, even when it fails" in {
    import cats.effect.IO
    import cats.effect.unsafe.implicits.global
    val (timings, clock) = recorder()

    val outcome = timings.timedIO(FramePhase.Sync)(IO(clock.advanceMs(4)) >> IO.raiseError(new RuntimeException("x")))

    outcome.attempt.unsafeRunSync().isLeft shouldBe true
    timings.drain().phases(FramePhase.Sync).maxMs shouldBe 4.0
  }

  it should "start a fresh window after each drain, keeping in-flight input" in {
    val (timings, clock) = recorder()
    timings.renderStarted()
    timings.framePublished()
    timings.renderFinished(FrameKind.CursorOnly)
    timings.inputArrived()
    clock.advanceMs(5_000)

    val first = timings.drain()
    first.elapsedMs shouldBe 5_000.0
    first.frames shouldBe Map(FrameKind.CursorOnly -> 1)

    clock.advanceMs(1)
    timings.inputApplyStarted()
    val second = timings.drain()
    second.frames shouldBe empty
    second.phases(FramePhase.InputQueue).p50Ms shouldBe 5_001.0
  }

  "FrameTimingState" should "keep only the most recent samples per phase" in {
    val filled = (1 to FrameTimingState.MaxSamplesPerPhase + 10).foldLeft(FrameTimingState.start(0L)) { (state, i) =>
      state.record(FramePhase.Paint, i.toLong * Ms)
    }

    val stats = filled.summary(now = 0L).phases(FramePhase.Paint)
    stats.count shouldBe FrameTimingState.MaxSamplesPerPhase
    stats.maxMs shouldBe (FrameTimingState.MaxSamplesPerPhase + 10).toDouble
  }

  "PhaseStats" should "report nearest-rank percentiles" in {
    val stats = PhaseStats.of((1 to 100).map(_.toLong * Ms).toVector)

    stats.count shouldBe 100
    stats.p50Ms shouldBe 50.0
    stats.p95Ms shouldBe 95.0
    stats.maxMs shouldBe 100.0
  }

  "FrameTimingSummary.logLine" should "report frame rates and every phase that saw samples, in pipeline order" in {
    val (timings, clock) = recorder()
    timings.renderStarted()
    clock.advanceMs(2.5)
    timings.framePublished()
    timings.renderFinished(FrameKind.Full)
    timings.timed(FramePhase.Render)(clock.advanceMs(2.5))
    clock.advanceMs(4_995)

    timings.drain().logLine shouldBe
      "[FRAME] window=5.0s full=1 (0.2/s) cursor=0 (0.0/s) | render p50=2.50 p95=2.50 max=2.50 n=2 ms"
  }

  it should "say so when a window saw no frames at all" in {
    val (timings, clock) = recorder()
    clock.advanceMs(5_000)

    timings.drain().logLine shouldBe "[FRAME] window=5.0s full=0 (0.0/s) cursor=0 (0.0/s) | no samples"
  }

  it should "record nothing while frame timing is off, however many frames pass" in {
    val clock   = ManualClock()
    val timings = FrameTimings(() => clock.read())

    (1 to 10_000).foreach { _ =>
      timings.inputArrived()
      timings.inputApplyStarted()
      timings.inputApplyFinished()
      timings.renderStarted()
      timings.framePublished()
      timings.renderFinished(FrameKind.Full)
      timings.paintFinished(timings.paintStarted())
      timings.timed(FramePhase.Sync)(())
      clock.advanceMs(1)
    }

    val summary = timings.drain()
    timings.isEnabled shouldBe false
    summary.phases shouldBe empty
    summary.frames shouldBe empty
  }

  it should "start recording when enabled and stop again when disabled, discarding what was half-recorded" in {
    val (timings, clock) = recorder()
    timings.setEnabled(false)
    timings.renderStarted()
    timings.setEnabled(true)
    clock.advanceMs(4)
    timings.renderFinished(FrameKind.Full)

    timings.drain().phases shouldBe empty

    timings.renderStarted()
    clock.advanceMs(4)
    timings.renderFinished(FrameKind.Full)
    timings.drain().phases(FramePhase.Render).p50Ms shouldBe 4.0

    timings.setEnabled(false)
    timings.renderStarted()
    timings.renderFinished(FrameKind.Full)
    timings.drain().frames shouldBe empty
  }
