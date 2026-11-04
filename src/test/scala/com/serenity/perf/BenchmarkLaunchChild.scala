package com.serenity.perf

/** Run by [[BenchmarkLaunchSpec]] in a separate JVM, so the spec can read how a benchmark launch ends. */
object CellGridFontChild:

  def main(args: Array[String]): Unit =
    BenchmarkMain.run:
      println(s"loaded ${CellGridBenchmarks.benchmarks().size} cell grid benchmarks")

/** Builds the suite inside its windows as `PerformanceBenchmarks.main` does, then fails with a `LinkageError` -- the
  * kind `NonFatal` does not catch -- while the windows are open.
  */
object WindowFailureChild:

  def main(args: Array[String]): Unit =
    BenchmarkMain.run:
      val _ = PerformanceBenchmarks.withSuite(_ => throw new ExceptionInInitializerError("benchmark setup failed"))

/** Runs the body the first argument names inside [[BenchmarkMain]]. */
object BenchmarkMainChild:

  def main(args: Array[String]): Unit =
    BenchmarkMain.run:
      args.headOption match
        case Some("success") => println("finished")
        case Some("failure") => throw new IllegalStateException("benchmark failed")
        case other           => throw new IllegalArgumentException(s"unknown mode $other")
