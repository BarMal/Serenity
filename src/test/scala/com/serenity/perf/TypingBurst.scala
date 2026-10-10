package com.serenity.perf

import java.util.concurrent.ConcurrentLinkedQueue

import scala.concurrent.duration.*

import cats.effect.unsafe.implicits.global
import cats.effect.{IO, Ref}
import cats.syntax.all.*
import com.serenity.app.AppRuntimeRenderLoops
import com.serenity.config.AppConfig
import com.serenity.diagnostics.FrameTimings
import com.serenity.input.{InputRouter, PendingInput, SystemClipboard}
import com.serenity.keystroke.events.Event
import com.serenity.keystroke.translators.TextEntryTranslator
import com.serenity.keystroke.{InputKey, KeyStrokeInfo}
import com.serenity.rope.Balance
import com.serenity.state.manager.StateManager
import fs2.{Chunk, Stream}

/** Typed keys driven through the production input loop (`AppRuntimeRenderLoops.inputBatchPhase` → `applyEventBatch`),
  * the path a burst from `bench/bench-laptop.sh` takes (#1985).
  *
  * {{{
  * TypingBurst [keys=200] [spacing-ms=5] [rounds=5]
  * TypingBurst sweep [keys=200] [rounds=3]
  * }}}
  *
  * replays `keys` random letters arriving `spacing-ms` apart (or at 10, 30, 60 and 200 keys/s), applying whatever
  * queued up while the previous batch ran as the next batch, and reports how long each key waited from arrival until
  * its batch was applied, and how large the batches were.
  */
object TypingBurst:
  given Balance = Balance.default

  def letters: Iterator[Char] = BenchmarkFixtures.randomLetters(seed = 1985L)

  private def keystroke(char: Char): PendingInput =
    PendingInput.Keystroke(KeyStrokeInfo(InputKey.Character, Some(char), Set.empty))

  /** The input loop's context over `stateManager`, counting the slices that published damage in `slices`. */
  private[perf] def inputContext(
    stateManager: StateManager,
    slices: Ref[IO, Long],
    frameTimings: FrameTimings = FrameTimings()
  ): IO[AppRuntimeRenderLoops.InputBatchContext] =
    for
      router          <- InputRouter.create[IO, Event](new TextEntryTranslator(AppConfig.default))
      cursorVisible   <- Ref.of[IO, Boolean](true)
      translatorCache <- Ref.of[IO, Option[AppRuntimeRenderLoops.FocusedTranslatorCacheEntry]](None)
    yield AppRuntimeRenderLoops.InputBatchContext(
      stateManager,
      router,
      SystemClipboard[IO](readText = IO.pure(None), writeText = _ => IO.unit),
      IO.unit,
      cursorVisible,
      _ => slices.update(_ + 1),
      translatorCache,
      frameTimings
    )

  /** Applies each batch as the input loop does and returns how many slices published damage. */
  private def applyBatches(stateManager: StateManager, batches: List[List[Char]]): IO[Long] =
    for
      slices  <- Ref.of[IO, Long](0L)
      context <- inputContext(stateManager, slices)
      _ <- Stream
        .emits(batches.map(batch => Chunk.from(batch.map(keystroke))))
        .through(AppRuntimeRenderLoops.inputBatchPhase(context))
        .compile
        .drain
      published <- slices.get
    yield published

  def typeEachAlone(stateManager: StateManager, chars: List[Char]): IO[Long] =
    applyBatches(stateManager, chars.map(List(_)))

  def typeAsOneBatch(stateManager: StateManager, chars: List[Char]): IO[Long] =
    applyBatches(stateManager, List(chars))

  final private case class Arrival(char: Char, arrivedAt: FiniteDuration)

  /** Each key's wait from arrival until its batch was applied, and the size of every batch, in order. */
  final private case class Burst(latenciesMs: Vector[Double], batchSizes: Vector[Int])

  /** Keys land on `queue` `spacing` apart; each batch is exactly what had queued when the previous one finished. */
  private def pacedBurst(stateManager: StateManager, keys: Int, spacing: FiniteDuration): IO[Burst] =
    val queue = new ConcurrentLinkedQueue[Arrival]()
    val arrive = letters
      .take(keys)
      .toList
      .traverse_(char => IO.monotonic.flatMap(now => IO(queue.offer(Arrival(char, now)))) >> IO.sleep(spacing))
    def takeQueued: IO[List[Arrival]] =
      IO(Iterator.continually(Option(queue.poll())).takeWhile(_.nonEmpty).flatten.toList)
    def drain(applied: Int, burst: Burst): IO[Burst] =
      if applied >= keys then IO.pure(burst)
      else
        takeQueued.flatMap {
          case Nil => IO.sleep(200.micros) >> drain(applied, burst)
          case taken =>
            for
              _   <- typeAsOneBatch(stateManager, taken.map(_.char))
              now <- IO.monotonic
              waited = taken.map(arrival => (now - arrival.arrivedAt).toNanos / 1e6)
              result <- drain(applied + taken.size, Burst(burst.latenciesMs ++ waited, burst.batchSizes :+ taken.size))
            yield result
        }
    arrive.background.surround(drain(0, Burst(Vector.empty, Vector.empty)))

  private val batchSizeBuckets =
    List("1" -> (1, 1), "2-3" -> (2, 3), "4-7" -> (4, 7), "8-15" -> (8, 15), "16+" -> (16, Int.MaxValue))

  private def batchSizeHistogram(sizes: Vector[Int]): String =
    batchSizeBuckets
      .map { case (label, (low, high)) => s"$label:${sizes.count(size => size >= low && size <= high)}" }
      .mkString(" ")

  private def report(stateManager: StateManager, label: String, keys: Int, spacing: FiniteDuration, round: Int): Unit =
    val started   = System.nanoTime
    val burst     = pacedBurst(stateManager, keys, spacing).unsafeRunSync()
    val elapsedMs = (System.nanoTime - started) / 1e6
    TypingStatistics.summarize(burst.latenciesMs).foreach { summary =>
      println(
        f"RESULT $label round=$round keys=$keys arrival-to-applied p50=${summary.p50Ms}%.2f " +
          f"p95=${summary.p95Ms}%.2f max=${burst.latenciesMs.max}%.2f ms elapsed=$elapsedMs%.0f ms " +
          s"batches=${burst.batchSizes.size} max-batch=${burst.batchSizes.max} sizes ${batchSizeHistogram(burst.batchSizes)}"
      )
    }

  def main(args: Array[String]): Unit =
    val sweep   = args.headOption.contains("sweep")
    val numbers = if sweep then args.drop(1) else args
    def argument(index: Int, default: Int): Int =
      numbers.lift(index).flatMap(_.toIntOption).filter(_ > 0).getOrElse(default)
    val keys              = argument(0, 200)
    val (stateManager, _) = LaptopFrameBenchmarks.proseStateManager(config = AppConfig.default.withWordWrap(true))
    if sweep then
      List(10, 30, 60, 200).foreach { keysPerSecond =>
        (1 to argument(1, 3)).foreach(round =>
          report(stateManager, s"rate=$keysPerSecond/s", keys, (1_000_000 / keysPerSecond).micros, round)
        )
      }
    else
      val spacingMs = argument(1, 5)
      (1 to argument(2, 5)).foreach(round =>
        report(stateManager, s"spacing=${spacingMs}ms", keys, spacingMs.millis, round)
      )
    sys.exit(0)
