package com.serenity.perf

import java.nio.file.Path
import java.time.Instant
import java.time.temporal.ChronoUnit
import java.util.Locale
import java.util.concurrent.atomic.AtomicInteger
import java.util.concurrent.locks.LockSupport

import cats.effect.unsafe.IORuntime
import cats.effect.{Deferred, IO}
import cats.syntax.all.*
import com.serenity.TestTemp
import com.serenity.io.ProjectFileIndex
import com.serenity.keystroke.events.InsertChar
import com.serenity.rope.Balance
import com.serenity.state.manager.DamageProducer
import com.serenity.state.models.Damage

/** How much typing slows while a project is being indexed (#2420), measured and reported, not gated: the same
  * keystrokes typed idle and then while a cold walk of the 100,000-file tree runs back to back on other threads, in
  * idle, loaded, loaded, idle blocks so drift falls on both alike. A keystroke is the edit applied through a live
  * `StateManager` plus the damaged frame drawn for it, which is the in-process part of input-to-paint; the Swing
  * present and the compositor are not in it. No threshold is applied: what ratio is acceptable is for the owner to set
  * once a measurement exists. Pace defaults to 100 ms between keys, the 10 keys per second that `bench/bench-laptop.sh`
  * types at. Run with `sbt "Test/runMain com.serenity.perf.TypingWhileIndexingProfile"` (add `--keys=N --pace-ms=N
  * --warmup-keys=N --small`) or, from a bench-artifacts jar, `java -cp serenity-perf.jar
  * com.serenity.perf.TypingWhileIndexingProfile`; it needs a display.
  */
object TypingWhileIndexingProfile:

  given Balance = Balance.default

  private val Seed = 20260430L

  enum Mode:
    case Idle, Loaded

  val BlockOrder: List[Mode] = List(Mode.Idle, Mode.Loaded, Mode.Loaded, Mode.Idle)

  /** `keys` per block, `warmupKeys` typed first and not measured, `paceMs` between keys. These are how long to measure
    * for, not targets.
    */
  final case class Options(keys: Int, paceMs: Long, warmupKeys: Int, shape: TreeShape)

  object Options:
    val default: Options = Options(keys = 300, paceMs = 100, warmupKeys = 200, shape = TreeShape.Monorepo100k)

  final case class Summary(count: Int, p50Ms: Double, p95Ms: Double, maxMs: Double)

  object Summary:

    /** The same nearest-rank percentiles as `BenchmarkRunner`. */
    def of(samplesMs: Vector[Double]): Summary =
      if samplesMs.isEmpty then Summary(0, 0.0, 0.0, 0.0)
      else
        val ordered = samplesMs.sorted
        Summary(ordered.size, nearestRank(ordered, 0.50), nearestRank(ordered, 0.95), ordered.last)

    private def nearestRank(ordered: Vector[Double], fraction: Double): Double =
      ordered((math.ceil(fraction * ordered.size).toInt - 1).max(0).min(ordered.size - 1))

  final case class Ratios(p50: Double, p95: Double)

  object Ratios:

    def of(idle: Summary, loaded: Summary): Option[Ratios] =
      Option.when(idle.p50Ms > 0.0 && idle.p95Ms > 0.0)(Ratios(loaded.p50Ms / idle.p50Ms, loaded.p95Ms / idle.p95Ms))

  final case class Outcome(idle: Summary, loaded: Summary, walksCompleted: Int)

  def main(args: Array[String]): Unit =
    import cats.effect.unsafe.implicits.global

    val options = optionsFrom(args.toList)
    report(run(options), options).foreach(println)

  private def flagsOf(args: List[String]): Map[String, String] =
    args.map(_.stripPrefix("--").split("=", 2).toList).collect { case key :: value :: Nil => key -> value }.toMap

  private def optionsFrom(args: List[String]): Options =
    val flags    = flagsOf(args)
    val defaults = Options.default
    Options(
      keys = flags.get("keys").flatMap(_.toIntOption).getOrElse(defaults.keys),
      paceMs = flags.get("pace-ms").flatMap(_.toLongOption).getOrElse(defaults.paceMs),
      warmupKeys = flags.get("warmup-keys").flatMap(_.toIntOption).getOrElse(defaults.warmupKeys),
      shape = if args.contains("--small") then TreeShape.Small else defaults.shape
    )

  def run(options: Options)(using IORuntime): Outcome =
    val root              = written(options.shape)
    val (stateManager, _) = LaptopFrameBenchmarks.proseStateManager()
    val frames            = new LaptopFrameBenchmarks.PersistingFrames()
    val letters           = BenchmarkFixtures.randomLetters(seed = 42L)
    val walks             = new AtomicInteger(0)
    val _                 = frames.render(stateManager.getCurrentState.unsafeRunSync(), Damage.Everything)

    def keystroke(): Double =
      val before  = stateManager.getCurrentState.unsafeRunSync()
      val started = System.nanoTime()
      stateManager.applyEvent(InsertChar(letters.next())).unsafeRunSync()
      val after = stateManager.getCurrentState.unsafeRunSync()
      val _     = frames.render(after, DamageProducer.forTransition(before, after))
      (System.nanoTime() - started) / 1e6

    def typed(count: Int): Vector[Double] =
      Vector.fill(count) {
        val elapsed = keystroke()
        LockSupport.parkNanos(options.paceMs * 1_000_000L)
        elapsed
      }

    def block(mode: Mode): Vector[Double] =
      mode match
        case Mode.Idle   => typed(options.keys)
        case Mode.Loaded => underWalks(root, walks, () => typed(options.keys))

    val _      = typed(options.warmupKeys)
    val blocks = BlockOrder.map(mode => mode -> block(mode))

    def samplesOf(mode: Mode): Vector[Double] = blocks.filter(_._1 == mode).flatMap(_._2).toVector

    Outcome(Summary.of(samplesOf(Mode.Idle)), Summary.of(samplesOf(Mode.Loaded)), walks.get)

  def report(outcome: Outcome, options: Options): List[String] =
    val ratios = Ratios
      .of(outcome.idle, outcome.loaded)
      .fold("n/a")(ratio => "p50_ratio=%.2f p95_ratio=%.2f".formatLocal(Locale.ROOT, ratio.p50, ratio.p95))
    List(
      s"typing_while_indexing keys=${options.keys} pace_ms=${options.paceMs} warmup_keys=${options.warmupKeys}",
      s"tree files=${options.shape.visibleFiles} directories=${options.shape.directories}",
      line("typing.idle", outcome.idle),
      line("typing.loaded", outcome.loaded),
      s"RESULT typing.loaded_over_idle $ratios",
      s"RESULT walks_completed=${outcome.walksCompleted}"
    )

  private def line(name: String, summary: Summary): String =
    "RESULT %s count=%d p50_ms=%.3f p95_ms=%.3f max_ms=%.3f"
      .formatLocal(Locale.ROOT, name, summary.count, summary.p50Ms, summary.p95Ms, summary.maxMs)

  /** Cold walks, one after another, until `work` is done. The first has finished before `work` starts, so the measured
    * keys are typed with a walk already going and the walker's own warm-up behind it.
    */
  private def underWalks(root: Path, walks: AtomicInteger, work: () => Vector[Double])(using
    IORuntime
  ): Vector[Double] =
    (for
      started <- Deferred[IO, Unit]
      walker  <- (walkOnce(root) >> IO(walks.incrementAndGet()) >> started.complete(())).foreverM.start
      _       <- started.get
      samples <- IO.blocking(work())
      _       <- walker.cancel
    yield samples).unsafeRunSync()

  private def walkOnce(root: Path): IO[Unit] =
    ProjectFileIndex.empty(globalExcludes = IO.pure(Vector.empty)).listing(root, _ => IO.unit).void

  private def written(shape: TreeShape): Path =
    val root = TestTemp.directory("typing-while-indexing")
    val plan = SyntheticProjectTree.plan(shape, Seed)
    SyntheticProjectTree.materialize(plan, root, Instant.now().minus(1, ChronoUnit.DAYS))
    root
