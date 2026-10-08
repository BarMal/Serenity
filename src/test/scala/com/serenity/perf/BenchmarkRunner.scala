package com.serenity.perf

import java.lang.management.ManagementFactory

/** Measurement machinery, separated from the benchmark definitions so that how a number is produced can be reviewed
  * apart from what is being measured.
  */
object BenchmarkRunner:

  /** Benchmarks whose allocation per invocation is worth the second measurement pass. */
  private val AllocationTracked =
    Set(
      "reducer.normal_editing",
      "reducer.backspace",
      "reducer.delete_word_backward",
      "reducer.arrow_navigation",
      "reducer.extend_selection",
      "reducer.multi_cursor_insert",
      "reducer.multi_cursor_move",
      "reducer.multi_cursor_move_down",
      "reducer.deep_scroll.plain",
      "reducer.deep_scroll.rich_text",
      "render.long_measured_line.java2d",
      "damage.single_char_long_line.rows",
      "damage.single_char_long_line.cells",
      "damage.single_char_short_line.rows",
      "damage.single_char_short_line.cells",
      "damage.multi_cursor.rows",
      "damage.multi_cursor.cells",
      "damage.markdown.rows",
      "damage.markdown.cells",
      "damage.scroll.rows",
      "damage.scroll.cells",
      "damage.caret_move_100k_lines.rows",
      "damage.caret_move_100k_lines.cells",
      "equals.appstate.same_reference",
      "equals.appstate.shared_fields_different_instance",
      "equals.appstate.independent_equal_content",
      "equals.buffer.same_reference",
      "equals.buffer.shared_fields_different_instance",
      "equals.buffer.independent_equal_content",
      "equals.appstate.multi_buffer_session.same_reference",
      "equals.appstate.multi_buffer_session.one_buffer_edited",
      "equals.appstate.multi_buffer_session.independent_equal_content"
    )

  final private[perf] case class Benchmark(
      name: String,
      warmups: Int,
      iterations: Int,
      verify: () => Unit,
      run: () => Any,
      minBatch: Int = 1,
      settleJit: Boolean = false,
      fixedSampleCount: Boolean = false
  )

  final private[perf] case class BenchmarkResult(
      name: String,
      iterations: Int,
      warmupInvocations: Int,
      batch: Int,
      minMs: Double,
      p50Ms: Double,
      p95Ms: Double,
      maxMs: Double,
      allocationP50Bytes: Option[Long],
      allocationP95Bytes: Option[Long]
  )

  /** Warm up until the JIT has had a fair chance rather than a fixed handful of invocations: HotSpot needs roughly 200
    * invocations for C1 and 10,000 for C2, and measuring below that reports interpreted performance. That matters most
    * for the comparison this harness exists to make -- indirection and short-lived allocation are penalised heavily in
    * the interpreter and largely optimised away by C2, so an under-warmed benchmark systematically favours whichever
    * version allocates less.
    *
    * The budget is a wall-clock ceiling so slow benchmarks stay bounded while microsecond ones get tens of thousands of
    * invocations.
    */
  private val WarmupBudgetNanos    = 500_000_000L
  private val MaxWarmupInvocations = 200_000

  /** Batch invocations until a timed sample is far enough above `System.nanoTime` resolution and its call overhead. */
  private val TargetSampleNanos = 2_000_000L
  private val MaxBatch          = 100_000

  /** C2 compiles on its own thread, so the swap from profiled to optimised code can land seconds after the invocation
    * counters trip, and a fixed count leaves the timed samples straddling it. Past the warmup floor, a benchmark that
    * opts in keeps running until the median per-call time of the older and newer halves of a two-second window agree.
    */
  private val SettleRoundNanos   = 100_000_000L
  private val SettleWindowRounds = 20
  private val SettleTolerance    = 1.05
  private val MaxSettleNanos     = 30_000_000_000L

  /** A benchmark of a few milliseconds per sample finishes its 8 to 60 samples inside 20 to 200 ms, so one stall that
    * long -- a GC pause, a noisy neighbour on a shared runner -- moves every sample and with them the median. Sampling
    * on past the configured count until the samples span this window lets the median ride out a stall shorter than half
    * of it. A benchmark that types without deleting opts out with `fixedSampleCount`: each extra sample would grow its
    * document and so change what the median measures.
    */
  private[perf] val MinSamplingWindowNanos = 400_000_000L
  private[perf] val MaxSamples             = 1_000

  private val sink = new java.util.concurrent.atomic.AtomicLong(0L)

  /** `identityHashCode` rather than `hashCode`: it reads the object header instead of traversing the value, so it does
    * not add the cost of hashing an `AppState` to every measured invocation. It pins the returned result, which escapes
    * in production too, while leaving a benchmark's internal short-lived allocations free to be scalar-replaced.
    */
  private def repeat(times: Int, run: () => Any): Long =
    (0 until times).foldLeft(0L)((acc, _) => acc + System.identityHashCode(run()))

  @annotation.tailrec
  private def warmUp(run: () => Any, deadline: Long, done: Int, minimum: Int, acc: Long): (Int, Long) =
    if done >= MaxWarmupInvocations || (done >= minimum && System.nanoTime() >= deadline) then (done, acc)
    else warmUp(run, deadline, done + 1, minimum, acc + System.identityHashCode(run()))

  @annotation.tailrec
  private def settle(run: () => Any, deadline: Long, recent: Vector[Double], acc: Long): Long =
    val (nanosPerCall, observed) = timedRound(run)
    val window                   = (recent :+ nanosPerCall).takeRight(SettleWindowRounds)
    if isSteady(window) || System.nanoTime() >= deadline then acc + observed
    else settle(run, deadline, window, acc + observed)

  private def timedRound(run: () => Any): (Double, Long) =
    val started = System.nanoTime()

    @annotation.tailrec
    def loop(calls: Int, acc: Long): (Int, Long) =
      val observed = acc + System.identityHashCode(run())
      if System.nanoTime() - started >= SettleRoundNanos then (calls + 1, observed) else loop(calls + 1, observed)

    val (calls, observed) = loop(0, 0L)
    ((System.nanoTime() - started).toDouble / calls, observed)

  private[perf] def isSteady(window: Vector[Double]): Boolean =
    window.length >= SettleWindowRounds && {
      val (earlier, later) = window.splitAt(window.length / 2)
      val (before, after)  = (median(earlier), median(later))
      before.max(after) <= before.min(after) * SettleTolerance
    }

  private def median(values: Vector[Double]): Double = values.sorted.apply(values.length / 2)

  private[perf] def enoughSamples(
    collected: Int,
    minimum: Int,
    elapsedNanos: Long,
    extendToWindow: Boolean = true
  ): Boolean =
    collected >= MaxSamples || (collected >= minimum && (!extendToWindow || elapsedNanos >= MinSamplingWindowNanos))

  private def sample(run: () => Any, batch: Int, minimum: Int, extendToWindow: Boolean): Vector[Double] =
    @annotation.tailrec
    def loop(collected: Vector[Double], elapsedNanos: Long, observed: Long): (Vector[Double], Long) =
      if enoughSamples(collected.length, minimum, elapsedNanos, extendToWindow) then (collected, observed)
      else
        val started = System.nanoTime()
        val seen    = repeat(batch, run)
        val elapsed = System.nanoTime() - started
        loop(collected :+ elapsed.toDouble / 1_000_000.0 / batch, elapsedNanos + elapsed, observed + seen)

    val (samples, observed) = loop(Vector.empty, 0L, 0L)
    sink.addAndGet(observed)
    samples.sorted

  @annotation.tailrec
  private def calibrate(run: () => Any, batch: Int): Int =
    if batch >= MaxBatch then batch
    else
      val started  = System.nanoTime()
      val observed = repeat(batch, run)
      sink.addAndGet(observed)
      if System.nanoTime() - started >= TargetSampleNanos then batch else calibrate(run, batch * 2)

  private val allocationBean = ManagementFactory.getThreadMXBean match
    case bean: com.sun.management.ThreadMXBean if bean.isThreadAllocatedMemorySupported =>
      if !bean.isThreadAllocatedMemoryEnabled then bean.setThreadAllocatedMemoryEnabled(true)
      Some(bean)
    case _ => None

  /** Runs the benchmarks whose names start with any of `prefixes`, or all of them when there are none. */
  private[perf] def runMatching(prefixes: List[String], benchmarks: List[Benchmark]): List[BenchmarkResult] =
    benchmarks.filter(b => prefixes.isEmpty || prefixes.exists(b.name.startsWith)).map(runBenchmark)

  private[perf] def runBenchmark(benchmark: Benchmark): BenchmarkResult =
    benchmark.verify()
    val (warmupInvocations, warmupAcc) =
      warmUp(benchmark.run, System.nanoTime() + WarmupBudgetNanos, 0, benchmark.warmups, 0L)
    sink.addAndGet(warmupAcc)
    if benchmark.settleJit || BenchmarkIterationCounts.JitSettled.contains(benchmark.name) then
      sink.addAndGet(settle(benchmark.run, System.nanoTime() + MaxSettleNanos, Vector.empty, 0L))
    val batch   = calibrate(benchmark.run, 1).max(benchmark.minBatch)
    val samples = sample(benchmark.run, batch, benchmark.iterations, !benchmark.fixedSampleCount)
    val allocationSamples =
      if AllocationTracked.contains(benchmark.name) then
        allocationBean
          .map { bean =>
            val threadId = Thread.currentThread().threadId()
            (0 until benchmark.iterations).map { _ =>
              val started   = bean.getThreadAllocatedBytes(threadId)
              val observed  = repeat(batch, benchmark.run)
              val allocated = bean.getThreadAllocatedBytes(threadId) - started
              sink.addAndGet(observed)
              (allocated / batch).max(0L)
            }
          }
          .getOrElse(Vector.empty[Long])
          .sorted
      else Vector.empty[Long]
    BenchmarkResult(
      name = benchmark.name,
      iterations = samples.length,
      warmupInvocations = warmupInvocations,
      batch = batch,
      minMs = samples.headOption.getOrElse(0.0),
      p50Ms = percentile(samples, 0.50),
      p95Ms = percentile(samples, 0.95),
      maxMs = samples.lastOption.getOrElse(0.0),
      allocationP50Bytes = allocationSamples.headOption.map(_ => percentileLong(allocationSamples, 0.50)),
      allocationP95Bytes = allocationSamples.headOption.map(_ => percentileLong(allocationSamples, 0.95))
    )

  private def percentile(samples: IndexedSeq[Double], percentile: Double): Double =
    if samples.isEmpty then 0.0
    else
      val index = math.ceil(percentile.max(0.0).min(1.0) * samples.length).toInt - 1
      samples(index.max(0).min(samples.length - 1))

  private def percentileLong(samples: IndexedSeq[Long], percentile: Double): Long =
    if samples.isEmpty then 0L
    else
      val index = math.ceil(percentile.max(0.0).min(1.0) * samples.length).toInt - 1
      samples(index.max(0).min(samples.length - 1))

  private[perf] def printResults(results: List[BenchmarkResult]): Unit =
    println("Serenity performance benchmarks")
    println(s"context,java_runtime,${System.getProperty("java.runtime.version", "unknown")}")
    println(s"context,java_vendor,${System.getProperty("java.vendor", "unknown")}")
    println(s"context,os,${System.getProperty("os.name", "unknown")} ${System.getProperty("os.version", "unknown")}")
    println(s"context,available_processors,${Runtime.getRuntime.availableProcessors()}")
    println(
      "name,iterations,warmup_invocations,batch,min_ms,p50_ms,p95_ms,max_ms,allocation_p50_bytes,allocation_p95_bytes"
    )
    results.foreach(result => println(csvRow(result)))

  private[perf] def csvRow(result: BenchmarkResult): String =
    val allocationP50 = result.allocationP50Bytes.fold("")(_.toString)
    val allocationP95 = result.allocationP95Bytes.fold("")(_.toString)
    f"${result.name},${result.iterations},${result.warmupInvocations},${result.batch}," +
      f"${result.minMs}%.7f,${result.p50Ms}%.7f,${result.p95Ms}%.7f,${result.maxMs}%.7f,$allocationP50,$allocationP95"
