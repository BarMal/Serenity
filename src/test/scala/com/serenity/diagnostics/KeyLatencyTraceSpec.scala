package com.serenity.diagnostics

import java.util.concurrent.atomic.AtomicLong

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class KeyLatencyTraceSpec extends AnyFlatSpec with Matchers:
  import LatencyStage.*

  private val Ms = 1_000_000L

  /** A clock the test advances by hand, in nanoseconds. */
  final private class ManualClock:
    private val now                 = new AtomicLong(0L)
    def advanceMs(ms: Double): Unit = now.addAndGet((ms * Ms).toLong): Unit
    def read(): Long                = now.get()

  private val WallClockMs = 10_000L

  private def recorder(): (KeyLatencyTrace, ManualClock) =
    val clock = ManualClock()
    val trace = KeyLatencyTrace(() => clock.read(), () => WallClockMs)
    trace.setEnabled(true)
    (trace, clock)

  private def stagesMs(latency: KeyLatency): Map[LatencyStage, Double] =
    LatencyStage.values.map(stage => stage -> latency.stage(stage).toDouble / Ms).toMap

  private val oneKeystroke = KeyStamps(
    seq = 7,
    whenToEdtMs = 4,
    receivedAt = 0,
    enqueuedAt = Some(1 * Ms),
    dequeuedAt = Some(3 * Ms),
    applyStartedAt = Some(4 * Ms),
    appliedAt = Some(9 * Ms),
    damagedAt = Some(10 * Ms)
  )

  private val itsFrame = FrameStamps(
    wokeAt = 12 * Ms,
    deadlineAt = Some(20 * Ms),
    modelReadAt = Some(21 * Ms),
    publishedAt = Some(36 * Ms),
    paintStartedAt = Some(50 * Ms)
  )

  private val oneKeystrokeLine =
    "[LATENCY] seq=7 when_to_edt=4 edt_to_enqueue=1.00 queue=2.00 dispatch=1.00 apply=5.00 damage=1.00 " +
      "frame_wait=2.00 pacing_wait=8.00 pre_render=1.00 render=15.00 publish_to_paint=14.00 paint=3.00 total=53.00 ms"

  "KeyLatency.of" should "split a keystroke's time into consecutive stages that add up to the total" in {
    val latency = KeyLatency.of(oneKeystroke, itsFrame, paintFinishedAt = 53 * Ms)

    stagesMs(latency) shouldBe Map(
      EdtToEnqueue   -> 1.0,
      Queue          -> 2.0,
      Dispatch       -> 1.0,
      Apply          -> 5.0,
      Damage         -> 1.0,
      FrameWait      -> 2.0,
      PacingWait     -> 8.0,
      PreRender      -> 1.0,
      Render         -> 15.0,
      PublishToPaint -> 14.0,
      Paint          -> 3.0
    )
    latency.totalNanos shouldBe 53 * Ms
    latency.whenToEdtMs shouldBe 4
  }

  it should "charge a stage the keystroke overtook, or never passed through, as zero, keeping the total exact" in {
    val lateDamage = oneKeystroke.copy(damagedAt = Some(15 * Ms))
    val noDeadline = itsFrame.copy(deadlineAt = None)

    val overtaken = KeyLatency.of(lateDamage, itsFrame, paintFinishedAt = 53 * Ms)
    stagesMs(overtaken)(FrameWait) shouldBe 0.0
    stagesMs(overtaken)(PacingWait) shouldBe 5.0
    overtaken.totalNanos shouldBe 53 * Ms

    val skipped = KeyLatency.of(oneKeystroke, noDeadline, paintFinishedAt = 53 * Ms)
    stagesMs(skipped)(PacingWait) shouldBe 0.0
    stagesMs(skipped)(PreRender) shouldBe 9.0
    skipped.stageNanos.sum shouldBe skipped.totalNanos
  }

  "KeyLatency.logLine" should "name every stage in pipeline order, in milliseconds" in {
    KeyLatency.of(oneKeystroke, itsFrame, paintFinishedAt = 53 * Ms).logLine shouldBe oneKeystrokeLine
  }

  "LatencyWindow.summaryLine" should "give p50 and p95 for the OS-to-EDT gap, every stage and the total" in {
    def uniform(seq: Long, whenToEdtMs: Long, stageMs: Long) =
      KeyLatency(seq, whenToEdtMs, Vector.fill(LatencyStage.values.length)(stageMs * Ms), 11 * stageMs * Ms)
    val window = LatencyWindow(5000.0, Vector(uniform(0, 2, 1), uniform(1, 6, 3)), unpainted = 1)

    val stageParts = LatencyStage.values.toList.map(stage => s"${stage.label} p50=1.00 p95=3.00")
    window.summaryLine shouldBe
      ("[LATENCY] summary window=5.0s keys=2 unpainted=1 | when_to_edt p50=2.00 p95=6.00 | " +
        stageParts.mkString(" | ") + " | total p50=11.00 p95=33.00 ms")
  }

  it should "say so when no keystroke was painted" in {
    LatencyWindow(5000.0, Vector.empty, unpainted = 0).summaryLine shouldBe
      "[LATENCY] summary window=5.0s keys=0 unpainted=0 | no keystrokes"
  }

  "LatencyWindow.logLines" should "list each keystroke before the summary" in {
    val latency = KeyLatency.of(oneKeystroke, itsFrame, paintFinishedAt = 53 * Ms)
    val window  = LatencyWindow(5000.0, Vector(latency), unpainted = 0)

    window.logLines shouldBe Vector(oneKeystrokeLine, window.summaryLine)
  }

  private def arriveAndApply(trace: KeyLatencyTrace, keys: Int): Unit =
    (1 to keys).foreach { _ =>
      trace.keyReceived(WallClockMs)
      trace.keyEnqueued()
    }
    trace.keysDequeued(keys)
    trace.dispatchStarted()
    trace.keysApplied(keys)
    trace.damageEmitted()

  private def presentFrame(trace: KeyLatencyTrace): Unit =
    trace.frameWoke()
    trace.frameDeadlineReached()
    trace.frameModelRead()
    trace.framePublished()
    trace.frameEnded()
    trace.paintStarted()
    trace.paintFinished()

  "KeyLatencyTrace" should "follow one keystroke from the event thread to the end of the paint showing it" in {
    val (trace, clock) = recorder()

    trace.keyReceived(WallClockMs - 4)
    clock.advanceMs(1)
    trace.keyEnqueued()
    clock.advanceMs(2)
    trace.keysDequeued(1)
    clock.advanceMs(1)
    trace.dispatchStarted()
    clock.advanceMs(5)
    trace.keysApplied(1)
    clock.advanceMs(1)
    trace.damageEmitted()
    clock.advanceMs(2)
    trace.frameWoke()
    clock.advanceMs(8)
    trace.frameDeadlineReached()
    clock.advanceMs(1)
    trace.frameModelRead()
    clock.advanceMs(15)
    trace.framePublished()
    clock.advanceMs(1)
    trace.frameEnded()
    clock.advanceMs(13)
    trace.paintStarted()
    clock.advanceMs(3)
    trace.paintFinished()

    trace.drain().keys.map(_.logLine) shouldBe Vector(oneKeystrokeLine.replace("seq=7", "seq=0"))
  }

  it should "show a keystroke in the first frame to read the model after its damage, not one already under way" in {
    val (trace, clock) = recorder()

    arriveAndApply(trace, keys = 1)
    trace.frameWoke()
    trace.frameDeadlineReached()
    trace.frameModelRead()
    clock.advanceMs(2)
    arriveAndApply(trace, keys = 1)
    clock.advanceMs(10)
    trace.framePublished()
    trace.frameEnded()
    clock.advanceMs(1)
    presentFrame(trace)

    val painted = trace.drain().keys
    painted.map(_.seq) shouldBe Vector(0L, 1L)
    painted.map(latency => latency.stage(FrameWait).toDouble / Ms) shouldBe Vector(0.0, 11.0)
  }

  it should "stamp keystrokes in arrival order when a batch is applied over several dispatches" in {
    val (trace, clock) = recorder()

    (1 to 3).foreach { _ =>
      trace.keyReceived(WallClockMs)
      trace.keyEnqueued()
    }
    trace.keysDequeued(3)
    trace.dispatchStarted()
    clock.advanceMs(2)
    trace.keysApplied(2)
    clock.advanceMs(1)
    trace.dispatchStarted()
    clock.advanceMs(4)
    trace.keysApplied(1)
    trace.damageEmitted()
    presentFrame(trace)

    val painted = trace.drain().keys
    painted.map(latency => latency.stage(Dispatch).toDouble / Ms) shouldBe Vector(0.0, 0.0, 3.0)
    painted.map(latency => latency.stage(Apply).toDouble / Ms) shouldBe Vector(2.0, 2.0, 4.0)
  }

  it should "count keystrokes whose frame published nothing as unpainted" in {
    val (trace, _) = recorder()

    arriveAndApply(trace, keys = 2)
    trace.frameWoke()
    trace.frameModelRead()
    trace.frameEnded()
    trace.paintStarted()
    trace.paintFinished()

    val window = trace.drain()
    window.keys shouldBe empty
    window.unpainted shouldBe 2
  }

  it should "report a keystroke once when its paint finishes before the frame's render returns" in {
    val (trace, _) = recorder()

    arriveAndApply(trace, keys = 1)
    trace.frameWoke()
    trace.frameModelRead()
    trace.framePublished()
    trace.paintStarted()
    trace.paintFinished()
    trace.frameEnded()
    trace.paintStarted()
    trace.paintFinished()

    val window = trace.drain()
    window.keys.map(_.seq) shouldBe Vector(0L)
    window.unpainted shouldBe 0
  }

  it should "drop a frame that reads no keystroke, leaving later keystrokes to the next one" in {
    val (trace, _) = recorder()

    trace.frameWoke()
    trace.frameModelRead()
    arriveAndApply(trace, keys = 1)
    trace.framePublished()
    trace.frameEnded()
    trace.paintStarted()
    trace.paintFinished()
    trace.drain().keys shouldBe empty

    presentFrame(trace)
    trace.drain().keys.map(_.seq) shouldBe Vector(0L)
  }

  it should "record nothing while disabled" in {
    val clock = ManualClock()
    val trace = KeyLatencyTrace(() => clock.read(), () => WallClockMs)

    arriveAndApply(trace, keys = 2)
    presentFrame(trace)

    trace.isEnabled shouldBe false
    trace.pendingKeys shouldBe empty
    val window = trace.drain()
    window.keys shouldBe empty
    window.unpainted shouldBe 0
  }

  it should "start afresh when toggled, so nothing half-recorded before is reported after" in {
    val (trace, _) = recorder()

    arriveAndApply(trace, keys = 1)
    trace.setEnabled(false)
    trace.setEnabled(true)
    presentFrame(trace)

    trace.pendingKeys shouldBe empty
    trace.drain().keys shouldBe empty
  }

  it should "start a new window on each drain" in {
    val (trace, clock) = recorder()

    arriveAndApply(trace, keys = 1)
    presentFrame(trace)
    clock.advanceMs(5000)
    val first = trace.drain()

    first.keys should have size 1
    first.elapsedMs shouldBe 5000.0
    trace.drain().keys shouldBe empty
  }

  "FrameTimings" should "pass its dispatch, publish and paint points on to the keystroke trace" in {
    val clock   = ManualClock()
    val timings = FrameTimings(() => clock.read())
    timings.keyLatency.setEnabled(true)

    timings.keyLatency.keyReceived(System.currentTimeMillis())
    timings.keyLatency.keyEnqueued()
    timings.keyLatency.keysDequeued(1)
    clock.advanceMs(1)
    timings.inputApplyStarted()
    clock.advanceMs(2)
    timings.keyLatency.keysApplied(1)
    timings.inputApplyFinished()
    timings.keyLatency.damageEmitted()
    timings.keyLatency.frameWoke()
    timings.keyLatency.frameModelRead()
    clock.advanceMs(3)
    timings.framePublished()
    timings.keyLatency.frameEnded()
    clock.advanceMs(4)
    val paintStart = timings.paintStarted()
    clock.advanceMs(5)
    timings.paintFinished(paintStart)

    val painted = timings.keyLatency.drain().keys
    painted.map(latency => latency.stage(Dispatch).toDouble / Ms) shouldBe Vector(1.0)
    painted.map(latency => latency.stage(Apply).toDouble / Ms) shouldBe Vector(2.0)
    painted.map(latency => latency.stage(Render).toDouble / Ms) shouldBe Vector(3.0)
    painted.map(latency => latency.stage(PublishToPaint).toDouble / Ms) shouldBe Vector(4.0)
    painted.map(latency => latency.stage(Paint).toDouble / Ms) shouldBe Vector(5.0)
  }

  it should "leave the keystroke trace off unless asked" in {
    FrameTimings().keyLatency.isEnabled shouldBe false
  }
