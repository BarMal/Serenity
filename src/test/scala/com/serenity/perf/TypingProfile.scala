package com.serenity.perf

import java.lang.management.ManagementFactory
import java.util.concurrent.atomic.AtomicInteger
import java.util.concurrent.locks.LockSupport
import javax.swing.JPanel

import scala.annotation.tailrec
import scala.concurrent.duration.*

import cats.effect.unsafe.implicits.global
import cats.effect.{Deferred, IO}
import com.serenity.app.{OffscreenWarmUpFrames, RuntimeDisplayState, StartupWarmUp}
import com.serenity.config.AppConfig
import com.serenity.keystroke.events.{Event, InsertChar, MoveDown, MoveUp, PageDown, PageUp}
import com.serenity.perf.TypingStatistics.{Bucket, BucketSummary, Summary}
import com.serenity.rope.Balance
import com.serenity.state.manager.StateManager
import com.serenity.ui.layout.ViewportSize
import com.serenity.ui.theme.Theme
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.noop.NoOpLogger

/** Replays one per-keystroke scenario against a live `StateManager` for long enough to profile it, outside the CI perf
  * gate. Run it under JFR as `bench/README.md` describes.
  *
  * {{{
  * TypingProfile <scenario> <seconds | keys> [--warmup-s=10] [--pace-ms=100] [--warm-ms=0] [--real-warm-ms=0]
  * }}}
  *
  * `typing_random`, `typing_long_paragraph`, `move_down_up` and `page_down_up` warm up for `--warmup-s`, then run for
  * the given seconds. `cold_typing` types the given number of random letters in a fresh JVM with no warm-up, pausing
  * `--pace-ms` between keys outside the timed region; `--warm-ms` first replays the startup warm-up on a separate
  * `StateManager` for that long. `--real-warm-ms` instead runs the app's own `StartupWarmUp.run`, drawing into an
  * offscreen Java2D image (so it needs a display), and stops it after that long as the user's first input would.
  */
object TypingProfile:
  given Balance = Balance.default

  private val LongParagraphChars = 4_000
  private val DriftWindow        = 400

  final private case class Options(warmupSeconds: Int, paceMs: Long, warmMs: Long, realWarmMs: Long)

  private object Options:
    private val defaults = Options(warmupSeconds = 10, paceMs = 100, warmMs = 0, realWarmMs = 0)

    def parse(flags: List[String]): Either[String, Options] =
      flags.foldLeft[Either[String, Options]](Right(defaults)) { (parsed, flag) =>
        parsed.flatMap(options =>
          flag.stripPrefix("--").split("=", 2).toList match
            case "warmup-s" :: value :: Nil =>
              value.toIntOption.toRight(s"bad $flag").map(v => options.copy(warmupSeconds = v))
            case "pace-ms" :: value :: Nil =>
              value.toLongOption.toRight(s"bad $flag").map(v => options.copy(paceMs = v))
            case "warm-ms" :: value :: Nil =>
              value.toLongOption.toRight(s"bad $flag").map(v => options.copy(warmMs = v))
            case "real-warm-ms" :: value :: Nil =>
              value.toLongOption.toRight(s"bad $flag").map(v => options.copy(realWarmMs = v))
            case _ => Left(s"unknown flag $flag")
        )
      }

  /** One repeatable unit of work and how many events it applies, so a two-event step reports per event. */
  final private case class Step(eventsPerStep: Int, run: () => Unit)

  private val Usage =
    "usage: TypingProfile <typing_random|typing_long_paragraph|cold_typing|move_down_up|page_down_up> " +
      "<seconds|keys> [--warmup-s=10] [--pace-ms=100] [--warm-ms=0] [--real-warm-ms=0]"

  def main(args: Array[String]): Unit =
    val outcome = for
      parsed <- args.toList match
        case scenario :: count :: flags => count.toIntOption.toRight(Usage).map((scenario, _, flags))
        case _                          => Left(Usage)
      (scenario, count, flags) = parsed
      options <- Options.parse(flags)
    yield run(scenario, count, options)
    outcome.left.foreach(message => Console.err.println(message))
    sys.exit(if outcome.isRight then 0 else 2)

  private def run(scenario: String, count: Int, options: Options): Unit =
    scenario match
      case "cold_typing" => coldTyping(count, options)
      case "typing_random" =>
        timed(scenario, randomTyping(LaptopFrameBenchmarks.proseStateManager()._1), count, options)
      case "typing_long_paragraph" => timed(scenario, randomTyping(longParagraphManager()), count, options)
      case "move_down_up" =>
        timed(scenario, roundTrip(LaptopFrameBenchmarks.proseStateManager()._1, MoveDown, MoveUp), count, options)
      case "page_down_up" =>
        timed(scenario, roundTrip(LaptopFrameBenchmarks.proseStateManager()._1, PageDown, PageUp), count, options)
      case other => Console.err.println(s"unknown scenario $other\n$Usage")

  private def longParagraphManager(): StateManager =
    LaptopFrameBenchmarks
      .stateManagerHolding(BenchmarkFixtures.longParagraph(LongParagraphChars), cursorColumn = LongParagraphChars / 2)
      ._1

  private def apply(stateManager: StateManager, event: Event): Unit = stateManager.applyEvent(event).unsafeRunSync()

  private def randomTyping(stateManager: StateManager): Step =
    val letters = BenchmarkFixtures.randomLetters(seed = 42L)
    Step(1, () => apply(stateManager, InsertChar(letters.next())))

  private def roundTrip(stateManager: StateManager, there: Event, back: Event): Step =
    Step(
      2,
      () =>
        apply(stateManager, there); apply(stateManager, back)
    )

  private def elapsedMs(action: => Unit): Double =
    val started = System.nanoTime
    action
    (System.nanoTime - started) / 1e6

  private def untilNanoTime(deadline: Long): Iterator[Unit] =
    Iterator.continually(()).takeWhile(_ => System.nanoTime < deadline)

  private def deadlineAfterSeconds(seconds: Int): Long = System.nanoTime + seconds * 1_000_000_000L

  private val allocatedBytes: () => Long =
    ManagementFactory.getThreadMXBean match
      case bean: com.sun.management.ThreadMXBean =>
        () => bean.getThreadAllocatedBytes(Thread.currentThread.threadId)
      case _ => () => 0L

  private def timed(scenario: String, step: Step, seconds: Int, options: Options): Unit =
    untilNanoTime(deadlineAfterSeconds(options.warmupSeconds)).foreach(_ => step.run())
    val allocatedBefore = allocatedBytes()
    val samplesMs =
      untilNanoTime(deadlineAfterSeconds(seconds)).map(_ => elapsedMs(step.run()) / step.eventsPerStep).toVector
    val allocatedPerEvent = (allocatedBytes() - allocatedBefore) / (samplesMs.size.toLong * step.eventsPerStep).max(1L)
    TypingStatistics.summarize(samplesMs).foreach { summary =>
      println(
        f"RESULT $scenario events=${samplesMs.size * step.eventsPerStep} ms/event p50=${summary.p50Ms}%.3f " +
          f"p95=${summary.p95Ms}%.3f mean=${summary.meanMs}%.3f alloc/event=$allocatedPerEvent bytes"
      )
    }
    val windows =
      (0 until samplesMs.size / DriftWindow).toVector.map(i => Bucket(i * DriftWindow + 1, (i + 1) * DriftWindow))
    TypingStatistics.bucketed(samplesMs, windows).foreach(printDrift)

  private def printDrift(bucketSummary: BucketSummary): Unit =
    bucketSummary.summary.foreach(summary =>
      println(f"BUCKET steps ${bucketSummary.bucket.label}%-13s mean ms/event ${summary.meanMs}%.3f")
    )

  private def coldTyping(keys: Int, options: Options): Unit =
    val (stateManager, _) = LaptopFrameBenchmarks.proseStateManager()
    if options.warmMs > 0 then println(s"warm burst rounds=${warmBurst(options.warmMs)}")
    if options.realWarmMs > 0 then println(realWarmUp(options.realWarmMs))
    val paceNanos = options.paceMs * 1_000_000L
    val samplesMs = BenchmarkFixtures
      .randomLetters(seed = 42L)
      .take(keys)
      .map { letter =>
        val ms = elapsedMs(apply(stateManager, InsertChar(letter)))
        parkUntil(System.nanoTime + paceNanos)
        ms
      }
      .toVector
    TypingStatistics.bucketed(samplesMs, Bucket.cold).foreach(printCold)
    TypingStatistics
      .summarize(samplesMs)
      .foreach(summary => println(coldLine("all", summary) + f" first=${samplesMs.head}%.1f"))

  private def printCold(bucketSummary: BucketSummary): Unit =
    println(
      bucketSummary.summary
        .fold(s"COLD ${bucketSummary.bucket.label} no samples")(coldLine(bucketSummary.bucket.label, _))
    )

  private def coldLine(label: String, summary: Summary): String =
    f"COLD $label%-8s n=${summary.count}%3d mean ${summary.meanMs}%.3f p95 ${summary.p95Ms}%.3f"

  /** The startup warm-up's rounds on a second `StateManager`, as the app runs them before the first keystroke. */
  private def warmBurst(milliseconds: Long): Int =
    val (other, _) =
      LaptopFrameBenchmarks.proseStateManager(paragraphs = StartupWarmUp.Plan.default.paragraphs, cursorLine = 2)
    val deadline = System.nanoTime + milliseconds * 1_000_000L
    untilNanoTime(deadline).map(_ => StartupWarmUp.round.foreach(apply(other, _))).size

  /** The app's own warm-up pipeline, interrupted `milliseconds` after it starts like a first keystroke would. */
  private def realWarmUp(milliseconds: Long): String =
    given Logger[IO] = NoOpLogger[IO]
    val config       = AppConfig.default
    val program = for
      display    <- RuntimeDisplayState.create(config.editorConfig.fontConfig)
      firstInput <- Deferred[IO, Unit]
      canvas = new JPanel
      _ <- IO(canvas.setSize(1280, 800))
      framesDrawn = new AtomicInteger(0)
      frames = OffscreenWarmUpFrames
        .forCanvas(canvas, () => ViewportSize(120, 40), () => display.snapshot)
        .map(offscreen =>
          offscreen.copy(full =
            (state, damage, caches) => IO(framesDrawn.incrementAndGet()) >> offscreen.full(state, damage, caches)
          )
        )
      _       <- (IO.sleep(milliseconds.millis) >> firstInput.complete(())).start
      outcome <- StartupWarmUp.run(config, Theme.dark, ViewportSize(120, 40), frames, firstInput).timed
    yield s"real warm-up ${outcome._2} after ${outcome._1.toMillis}ms, steps=${framesDrawn.get}"
    program.unsafeRunSync()

  @tailrec
  private def parkUntil(deadline: Long): Unit =
    val remaining = deadline - System.nanoTime
    if remaining > 0 then
      LockSupport.parkNanos(remaining)
      parkUntil(deadline)

end TypingProfile
