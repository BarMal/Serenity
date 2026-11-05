package com.serenity.perf

import java.util.concurrent.TimeUnit

/** Run by [[BenchmarkLaunchSpec]] in a separate JVM, so the spec can read how a benchmark launch ends. */
object CellGridFontChild:

  def main(args: Array[String]): Unit =
    BenchmarkMain.run:
      println(s"loaded ${CellGridBenchmarks.benchmarks().size} cell grid benchmarks")

/** Builds the suite inside its windows as `PerformanceBenchmarks.main` does, then fails with a `LinkageError` -- the
  * kind `NonFatal` does not catch -- while the windows are open.
  */
object WindowFailureChild:

  /** Printed once the windows are open, so [[BenchmarkLaunchSpec]] can time the exit apart from the start-up. */
  val WindowsOpen: String = "benchmark windows open"

  def announceWindowsOpen(): Unit =
    println(WindowsOpen)
    System.out.flush()

  def main(args: Array[String]): Unit =
    BenchmarkMain.run:
      val _ = PerformanceBenchmarks.withSuite { _ =>
        announceWindowsOpen()
        throw new ExceptionInInitializerError("benchmark setup failed")
      }

/** Stand-ins for [[WindowFailureChild]] that reach its marker late, never exit after it, or never reach it, so
  * [[BenchmarkLaunchSpec]] can show how its helper judges each.
  */
object MarkerChild:

  def main(args: Array[String]): Unit =
    BenchmarkMain.run:
      args.toList match
        case "slow" :: delayMillis :: _ =>
          Thread.sleep(delayMillis.toLong)
          WindowFailureChild.announceWindowsOpen()
          throw new IllegalStateException("failed after the marker")
        case "hang" :: _ =>
          WindowFailureChild.announceWindowsOpen()
          TimeUnit.DAYS.sleep(1)
        case "silent" :: _ => TimeUnit.DAYS.sleep(1)
        case other         => throw new IllegalArgumentException(s"unknown mode $other")

/** Runs the body the first argument names inside [[BenchmarkMain]]. */
object BenchmarkMainChild:

  def main(args: Array[String]): Unit =
    BenchmarkMain.run:
      args.headOption match
        case Some("success") => println("finished")
        case Some("failure") => throw new IllegalStateException("benchmark failed")
        case other           => throw new IllegalArgumentException(s"unknown mode $other")
