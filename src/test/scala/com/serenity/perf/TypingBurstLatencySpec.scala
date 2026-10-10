package com.serenity.perf

import java.awt.event.KeyEvent
import java.util.concurrent.locks.LockSupport
import javax.swing.JPanel

import scala.annotation.tailrec
import scala.concurrent.duration.*

import cats.effect.unsafe.implicits.global
import cats.effect.{IO, Ref}
import com.serenity.app.AppRuntimeRenderLoops
import com.serenity.config.AppConfig
import com.serenity.diagnostics.{FrameTimings, KeyStamps}
import com.serenity.input.{InputRouter, SwingInputHandler}
import com.serenity.keystroke.events.Event
import com.serenity.keystroke.translators.TextEntryTranslator
import com.serenity.rope.Balance
import com.serenity.state.manager.StateManager
import com.serenity.state.models.{Buffer, BufferId}
import com.serenity.ui.layout.CellMetrics
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** The latency a typing burst sees through the production input path (#1985): AWT key events reach `SwingInputHandler`,
  * queue there, and leave `inputBatches` for `inputBatchPhase` and `applyEventBatch`, as the `bench/bench-laptop.sh`
  * burst (200 keys, `xdotool` 5 ms apart) does. Each key is timed by the handler's own latency trace, from
  * `keyReceived` to the damage its slice published.
  *
  * Real time rather than `TestControl`: keys apply in zero virtual time, so a virtual clock could only ever report the
  * pacing, never the cost of draining. The bound below is loose enough for a loaded CI machine, and still a long way
  * under the 400-745 ms dispatch p95 the burst showed on the laptop before keys were settled once per batch (#1988).
  */
class TypingBurstLatencySpec extends AnyFlatSpec with Matchers:
  given Balance = Balance.default

  private val burstKeys = 200
  private val spacing   = 5.millis

  /** Three times the p95 measured with keys settled once per batch (about 15 ms on the 2-core cloud box), and under the
    * 110 ms p95 measured with the run settling switched off. A p95 past it means keys queue behind batches that cost
    * more than the keys' arrival rate pays for.
    */
  private val p95BoundMs = 50.0

  private def burst: List[Char] = TypingBurst.letters.take(burstKeys).toList

  private def editor(): (StateManager, BufferId) =
    LaptopFrameBenchmarks.proseStateManager(config = AppConfig.default.withWordWrap(true))

  private def typedBuffer(stateManager: StateManager, bufferId: BufferId): Option[Buffer] =
    stateManager.getCurrentState.unsafeRunSync().persisted.buffers.get(bufferId)

  @tailrec
  private def parkUntil(deadlineNanos: Long): Unit =
    val remaining = deadlineNanos - System.nanoTime
    if remaining > 0 then
      LockSupport.parkNanos(remaining)
      parkUntil(deadlineNanos)

  /** Delivers each key to the component's key listener on a thread of its own, on a fixed schedule so a slow key does
    * not delay the next, as the event dispatch thread would.
    */
  private def typePaced(component: JPanel, chars: List[Char]): IO[Unit] =
    IO.blocking {
      val started = System.nanoTime
      chars.zipWithIndex.foreach { (char, index) =>
        parkUntil(started + index * spacing.toNanos)
        component.getKeyListeners.foreach(
          _.keyTyped(KeyEvent(component, KeyEvent.KEY_TYPED, System.currentTimeMillis, 0, KeyEvent.VK_UNDEFINED, char))
        )
      }
    }

  private def allDamaged(frameTimings: FrameTimings): IO[Boolean] =
    IO(frameTimings.keyLatency.pendingKeys.count(_.damagedAt.isDefined) >= burstKeys)

  private def awaitDamaged(frameTimings: FrameTimings): IO[Unit] =
    allDamaged(frameTimings).ifM(IO.unit, IO.sleep(2.millis) >> awaitDamaged(frameTimings))

  /** Replays `chars` paced through the production input path and returns the trace of every key. */
  private def replay(stateManager: StateManager, chars: List[Char]): IO[Vector[KeyStamps]] =
    val frameTimings = FrameTimings()
    frameTimings.keyLatency.setEnabled(true)
    val component = new JPanel()
    for
      router <- InputRouter.create[IO, Event](new TextEntryTranslator(AppConfig.default))
      metrics = () => CellMetrics(8, 16, 13)
      handler = new SwingInputHandler[IO, Event](component, router, metrics, metrics, frameTimings = frameTimings)
      slices  <- Ref.of[IO, Long](0L)
      context <- TypingBurst.inputContext(stateManager, slices, frameTimings)
      inputLoop <- handler.inputBatches
        .through(AppRuntimeRenderLoops.inputBatchPhase(context))
        .compile
        .drain
        .start
      _ <- typePaced(component, chars)
      _ <- awaitDamaged(frameTimings).timeout(60.seconds)
      _ <- handler.shutdown
      _ <- inputLoop.joinWithNever
    yield frameTimings.keyLatency.pendingKeys

  private def millisBetween(from: Long, to: Long): Double = (to - from) / 1e6

  private def queuePlusDispatchMs(keys: Vector[KeyStamps]): Vector[Double] =
    keys.flatMap(key => key.damagedAt.map(millisBetween(key.receivedAt, _)))

  private def queueMs(keys: Vector[KeyStamps]): Vector[Double] =
    keys.flatMap(key => key.dequeuedAt.map(millisBetween(key.receivedAt, _)))

  "A 200-key burst typed 5 ms apart through SwingInputHandler" should "see a bounded queue-plus-dispatch latency per key" in {
    val (stateManager, _) = editor()
    val _                 = TypingBurst.typeEachAlone(stateManager, List('w')).unsafeRunSync()
    val trace             = replay(stateManager, burst).unsafeRunSync()
    val total             = queuePlusDispatchMs(trace)
    val summary           = TypingStatistics.summarize(total)
    val queued            = TypingStatistics.summarize(queueMs(trace))

    withClue(s"queue+dispatch $summary, queue alone $queued, max ${total.maxOption}: ") {
      trace should have size burstKeys.toLong
      total should have size burstKeys.toLong
      summary.map(_.p95Ms).getOrElse(Double.MaxValue) should be <= p95BoundMs
    }
  }

  it should "leave the same text, cursors and viewport as the keys applied one at a time" in {
    val (alone, aloneBuffer) = editor()
    val (paced, pacedBuffer) = editor()
    val _                    = TypingBurst.typeEachAlone(alone, burst).unsafeRunSync()
    val _                    = replay(paced, burst).unsafeRunSync()
    def state(m: StateManager, id: BufferId) =
      typedBuffer(m, id).map(buffer => (buffer.document.content.toString, buffer.editing, buffer.viewport))

    state(paced, pacedBuffer) shouldBe state(alone, aloneBuffer)
  }
