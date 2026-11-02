package com.serenity.perf

/** The body of every benchmark and probe `main`. A failure ends the process with exit code 1.
  *
  * Without it an exception that escapes `main` leaves the JVM running whenever a non-daemon AWT thread is alive, and a
  * benchmark that opened a window has two. A `LinkageError` such as `ExceptionInInitializerError` also skips every
  * `Resource` finalizer, so the windows are never closed (#2486). Exiting here is what ends the process.
  */
private[perf] object BenchmarkMain:

  val FailureExitCode: Int = 1

  def run(body: => Unit): Unit =
    try body
    catch
      case failure: Throwable =>
        try failure.printStackTrace()
        finally sys.exit(FailureExitCode)
