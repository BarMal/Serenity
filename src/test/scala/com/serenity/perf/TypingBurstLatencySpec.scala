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
import com.serenity.state.manager.{StateManager, TypedRuns}
import com.serenity.state.models.{Buffer, BufferId}
import com.serenity.ui.layout.{CellMetrics, WrappedLineCache}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** What a typing burst costs through the production input path (#1985): AWT key events reach `SwingInputHandler`, queue
  * there, and leave `inputBatches` for `inputBatchPhase` and `applyEventBatch`, as the `bench/bench-laptop.sh` burst
  * (200 keys, `xdotool` 5 ms apart) does. Each key is traced by the handler's own latency trace, from `keyReceived` to
  * the damage its slice published.
  *
  * The guard counts work instead of reading a clock. A burst only queues up if a batch costs more than its keys'
  * arrival rate pays for, and the cost that did it was re-wrapping the paragraph once per key instead of once per
  * published slice (#1988). That holds however the keys happen to be grouped into batches, so unlike a p95 bound it
  * cannot fail on a loaded machine, and it fails whenever a batch settles its keys one at a time.
  * `TypingBurstBatchSpec` pins the same count for a single batch; this spec pins it through the real queue.
  */
class TypingBurstLatencySpec extends AnyFlatSpec with Matchers:
  given Balance = Balance.default

  private val burstKeys = 200

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
    * not delay the next, as the event dispatch thread would. A zero `spacing` delivers them back to back.
    */
  private def typePaced(component: JPanel, chars: List[Char], spacing: FiniteDuration): IO[Unit] =
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

  /** Replays `chars` through the production input path and returns the trace of every key and how many slices published
    * damage.
    */
  private def replay(
    stateManager: StateManager,
    chars: List[Char],
    spacing: FiniteDuration
  ): IO[(Vector[KeyStamps], Long)] =
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
      _         <- typePaced(component, chars, spacing)
      _         <- awaitDamaged(frameTimings).timeout(60.seconds)
      _         <- handler.shutdown
      _         <- inputLoop.joinWithNever
      published <- slices.get
    yield (frameTimings.keyLatency.pendingKeys, published)

  private def millisBetween(from: Long, to: Long): Double = (to - from) / 1e6

  private def wrapsPerformed(stateManager: StateManager): Long =
    stateManager.renderCaches.wrappedLines match
      case cache: WrappedLineCache.Bounded => cache.wrapStats.coldWraps + cache.wrapStats.incrementalWraps
      case _                               => 0L

  private def warmEditor(): StateManager =
    val (stateManager, _) = editor()
    val _                 = TypingBurst.typeEachAlone(stateManager, List('w')).unsafeRunSync()
    stateManager

  private def mostWrapsForOneKey(): Long =
    val alone = warmEditor()
    burst
      .map { char =>
        val before = wrapsPerformed(alone)
        val _      = TypingBurst.typeEachAlone(alone, List(char)).unsafeRunSync()
        wrapsPerformed(alone) - before
      }
      .maxOption
      .getOrElse(0L)

  private def assertRewrapsOncePerSlice(spacing: FiniteDuration, keysMustQueue: Boolean): Unit =
    val mostPerKey      = mostWrapsForOneKey()
    val stateManager    = warmEditor()
    val before          = wrapsPerformed(stateManager)
    val (trace, slices) = replay(stateManager, burst, spacing).unsafeRunSync()
    val wraps           = wrapsPerformed(stateManager) - before
    val settledRuns     = slices + burstKeys / TypedRuns.MaxKeys
    val waits           = trace.flatMap(key => key.damagedAt.map(millisBetween(key.receivedAt, _)))

    withClue(
      s"keys=$burstKeys slices=$slices runs<=$settledRuns wraps=$wraps (most for one key alone $mostPerKey), " +
        s"queue+dispatch ${TypingStatistics.summarize(waits)}: "
    ) {
      trace should have size burstKeys.toLong
      trace.flatMap(_.damagedAt) should have size burstKeys.toLong
      mostPerKey should be > 0L
      if keysMustQueue then slices should be < burstKeys.toLong
      wraps should be <= settledRuns * mostPerKey
    }

  "A 200-key burst typed back to back through SwingInputHandler" should "re-wrap once per published slice, not once per key" in
    assertRewrapsOncePerSlice(0.millis, keysMustQueue = true)

  "A 200-key burst typed 5 ms apart through SwingInputHandler" should "re-wrap once per published slice, not once per key" in
    assertRewrapsOncePerSlice(5.millis, keysMustQueue = false)

  it should "damage every key after it was received and dequeued, in the order the keys arrived" in {
    val (trace, _) = replay(warmEditor(), burst, 5.millis).unsafeRunSync()

    trace.map(_.receivedAt) shouldBe trace.map(_.receivedAt).sorted
    trace.foreach { key =>
      key.dequeuedAt.exists(_ >= key.receivedAt) shouldBe true
      key.damagedAt.exists(damaged => key.dequeuedAt.exists(_ <= damaged)) shouldBe true
    }
  }

  it should "leave the same text, cursors and viewport as the keys applied one at a time" in {
    val (alone, aloneBuffer) = editor()
    val (paced, pacedBuffer) = editor()
    val _                    = TypingBurst.typeEachAlone(alone, burst).unsafeRunSync()
    val _                    = replay(paced, burst, 5.millis).unsafeRunSync()
    def state(m: StateManager, id: BufferId) =
      typedBuffer(m, id).map(buffer => (buffer.document.content.toString, buffer.editing, buffer.viewport))

    state(paced, pacedBuffer) shouldBe state(alone, aloneBuffer)
  }
