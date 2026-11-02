package com.serenity.perf

/** Run by [[BenchmarkLaunchSpec]] in a separate JVM, so the spec can read how a benchmark launch ends. */
object CellGridFontChild:

  def main(args: Array[String]): Unit =
    println(s"loaded ${CellGridBenchmarks.benchmarks().size} cell grid benchmarks")

/** Builds the suite inside its windows as `PerformanceBenchmarks.main` does, then fails with a `LinkageError` -- the
  * kind `NonFatal` does not catch -- while the windows are open.
  */
object WindowFailureChild:

  def main(args: Array[String]): Unit =
    val _ = PerformanceBenchmarks.withSuite(_ => throw new ExceptionInInitializerError("benchmark setup failed"))
