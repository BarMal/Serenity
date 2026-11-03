package com.serenity.perf

import java.util.concurrent.atomic.AtomicLong

import scala.annotation.tailrec

/** Keeps a benchmark running after its ordinary warm-up until the JIT has finished with it, within a budget.
  *
  * A benchmark's p50 depends on how far the JIT had got when it was timed: the same code reads about 1.4 ms in one JVM
  * and 3 to 7 ms in the next, because the 500 ms warm-up covers only the first 50 to 90 calls of a frame that takes
  * about 3000 to settle (issue #2443). Rounds of one `roundNanos` are timed after the ordinary warm-up; a row that is
  * already flat and has done `minInvocations` stops after `flatRounds` of them, any other keeps going until the last
  * `windowRounds` agree, the row has spent `rowCapNanos`, or the whole run has spent `runCapNanos`.
  *
  * The budget is spent in the order the rows are handed in, so the order of the suite is part of what a run measures
  * (see `PerformanceBenchmarksOrderSpec`). Every constant is the value the 12-run study on #2443 used; the two caps
  * were chosen, not derived, and the study ran only that one setting.
  */
private[perf] object WarmupExtension:

  final case class Settings(
      roundNanos: Long,
      flatRounds: Int,
      flatTolerance: Double,
      minInvocations: Int,
      windowRounds: Int,
      windowTolerance: Double,
      rowCapNanos: Long,
      runCapNanos: Long
  )

  object Settings:

    val default: Settings = Settings(
      roundNanos = 100_000_000L,
      flatRounds = 5,
      flatTolerance = 1.10,
      minInvocations = 10_000,
      windowRounds = 40,
      windowTolerance = 1.20,
      rowCapNanos = 3_000_000_000L,
      runCapNanos = 90_000_000_000L
    )

    /** The defaults with the two caps replaced by `serenity.bench.warmup.rowCapSeconds` / `runCapSeconds`, for a study.
      */
    def fromProperties(property: String => Option[String]): Settings =
      def nanos(key: String, fallback: Long): Long =
        property(s"serenity.bench.warmup.$key")
          .flatMap(_.toDoubleOption)
          .fold(fallback)(seconds => (seconds * 1e9).toLong)
      default.copy(
        rowCapNanos = nanos("rowCapSeconds", default.rowCapNanos),
        runCapNanos = nanos("runCapSeconds", default.runCapNanos)
      )

  enum Verdict:
    case Stop, Continue

  /** Whether to stop warming a row, given the per-call time of each round so far (oldest first). */
  def decide(
    settings: Settings,
    rounds: Vector[Double],
    invocations: Int,
    rowSpentNanos: Long,
    runSpentNanos: Long
  ): Verdict =
    val capped = rowSpentNanos >= settings.rowCapNanos || runSpentNanos >= settings.runCapNanos
    val flatStart = rounds.length == settings.flatRounds && invocations >= settings.minInvocations &&
      agree(rounds, settings.flatTolerance)
    val settled =
      rounds.length >= settings.windowRounds && agree(rounds.takeRight(settings.windowRounds), settings.windowTolerance)
    if capped || flatStart || settled then Verdict.Stop else Verdict.Continue

  /** Whether the median of the earlier half of `window` is within `tolerance` of the median of the later half. */
  private def agree(window: Vector[Double], tolerance: Double): Boolean =
    val (earlier, later) = window.splitAt(window.length / 2)
    val (before, after)  = (median(earlier), median(later))
    before.max(after) <= before.min(after) * tolerance

  private def median(values: Vector[Double]): Double = values.sorted.apply(values.length / 2)

  /** What is left of the time a whole run may spend extending warm-ups. */
  final class Budget(val capNanos: Long):
    private val spent = new AtomicLong(0L)

    def charge(nanos: Long): Unit =
      val _ = spent.addAndGet(nanos)
    def spentNanos: Long   = spent.get()
    def exhausted: Boolean = spent.get() >= capNanos

  /** The settings and the budget one run of the suite shares. */
  final class Session(val settings: Settings, val budget: Budget, val enabled: Boolean):

    /** Extends `run`'s warm-up, unless the session is off or `growsState` (a benchmark whose every call adds to what it
      * measures, so extra calls would change its p50 rather than settle it).
      */
    def extend(run: () => Any, invocations: Int, growsState: Boolean): Long =
      if !enabled || growsState then 0L else WarmupExtension.extend(settings, budget, run, invocations)

  object Session:
    def apply(settings: Settings): Session = new Session(settings, new Budget(settings.runCapNanos), enabled = true)

    val off: Session = new Session(Settings.default, new Budget(0L), enabled = false)

    /** The session `PerformanceBenchmarks` uses: on, unless `-Dserenity.bench.warmup=off`. */
    def standard(): Session =
      if sys.props.get("serenity.bench.warmup").contains("off") then off
      else apply(Settings.fromProperties(sys.props.get))

  /** Runs `run` in timed rounds until [[decide]] says stop; returns what the calls returned, folded to a number so the
    * JIT cannot discard them.
    */
  def extend(
    settings: Settings,
    budget: Budget,
    run: () => Any,
    invocations: Int,
    clock: () => Long = () => System.nanoTime()
  ): Long =
    @tailrec
    def go(rounds: Vector[Double], observed: Long, rowSpent: Long): Long =
      if budget.exhausted || decide(settings, rounds, invocations, rowSpent, budget.spentNanos) == Verdict.Stop then
        observed
      else
        val (perCall, seen, elapsed) = round(settings, run, clock)
        budget.charge(elapsed)
        go(rounds :+ perCall, observed + seen, rowSpent + elapsed)
    go(Vector.empty, 0L, 0L)

  private def round(settings: Settings, run: () => Any, clock: () => Long): (Double, Long, Long) =
    val started = clock()

    @tailrec
    def loop(calls: Int, observed: Long): (Int, Long) =
      val seen = observed + System.identityHashCode(run())
      if clock() - started >= settings.roundNanos then (calls + 1, seen) else loop(calls + 1, seen)

    val (calls, observed) = loop(0, 0L)
    val elapsed           = clock() - started
    (elapsed.toDouble / calls, observed, elapsed)
