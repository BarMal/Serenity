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
      "reducer.deep_scroll.plain",
      "reducer.deep_scroll.rich_text",
      "render.long_measured_line.java2d"
    )

  final private[perf] case class Benchmark(
      name: String,
      warmups: Int,
      iterations: Int,
      verify: () => Unit,
      run: () => Any
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

  private[perf] def runBenchmark(benchmark: Benchmark): BenchmarkResult =
    benchmark.verify()
    val (warmupInvocations, warmupAcc) =
      warmUp(benchmark.run, System.nanoTime() + WarmupBudgetNanos, 0, benchmark.warmups, 0L)
    sink.addAndGet(warmupAcc)
    val batch = calibrate(benchmark.run, 1)
    val samples = (0 until benchmark.iterations).map { _ =>
      val started  = System.nanoTime()
      val observed = repeat(batch, benchmark.run)
      val elapsed  = System.nanoTime() - started
      sink.addAndGet(observed)
      elapsed.toDouble / 1_000_000.0 / batch
    }.sorted
    val allocationSamples =
      if AllocationTracked.contains(benchmark.name) then
        allocationBean
          .map { bean =>
            val threadId = Thread.currentThread().getId
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
      iterations = benchmark.iterations,
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
    results.foreach { result =>
      val allocationP50 = result.allocationP50Bytes.fold("")(_.toString)
      val allocationP95 = result.allocationP95Bytes.fold("")(_.toString)
      println(
        f"${result.name},${result.iterations},${result.warmupInvocations},${result.batch}," +
          f"${result.minMs}%.5f,${result.p50Ms}%.5f,${result.p95Ms}%.5f,${result.maxMs}%.5f,$allocationP50,$allocationP95"
      )
    }
