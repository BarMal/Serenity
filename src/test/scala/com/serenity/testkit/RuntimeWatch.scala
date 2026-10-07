package com.serenity.testkit

import java.io.{PrintWriter, StringWriter}
import java.lang.Thread.UncaughtExceptionHandler
import java.util.concurrent.CountDownLatch
import java.util.concurrent.TimeUnit.NANOSECONDS
import java.util.concurrent.atomic.AtomicReference

import scala.concurrent.duration.{DurationInt, FiniteDuration}
import scala.util.Try

import cats.effect.IO
import cats.effect.unsafe.IORuntime

/** What silently kills or wedges the global IO runtime, made visible in the log of a CI run that then hangs.
  *
  * Cats Effect shuts the runtime down without a log line when a fiber throws a fatal error or a worker thread is
  * interrupted, and its workers report an uncaught throwable only to their own pool's handler (stderr). Every test
  * thread then waits for a runtime that no longer runs anything, and the only trace is a run that never ends. Cats
  * Effect offers no hook for the global runtime's fatal-failure path, so this installs the JVM-wide handler for every
  * other thread and probes the runtime itself each time [[HangReporter]] reports.
  */
object RuntimeWatch:

  val ProbeDeadline: FiniteDuration = 5.seconds

  private val AbortExitCode = 70

  enum Liveness:
    case Responsive
    case Unresponsive(cause: String)

  def print(text: String): Unit =
    System.out.print(text + System.lineSeparator)
    System.out.flush()

  def death(thread: String, error: Throwable): String =
    val trace = StringWriter()
    error.printStackTrace(PrintWriter(trace, true))
    s"[RUNTIME-DEATH] uncaught ${error.getClass.getName} on thread $thread${System.lineSeparator}$trace"

  def uncaughtHandler(emit: String => Unit, previous: Option[UncaughtExceptionHandler]): UncaughtExceptionHandler =
    (thread, error) =>
      emit(death(thread.getName, error))
      previous.foreach(_.uncaughtException(thread, error))

  /** Installs the JVM-wide handler once per test JVM and returns the global runtime as it was then. */
  def install(): IORuntime = installed

  private lazy val installed: IORuntime =
    Thread.setDefaultUncaughtExceptionHandler(
      uncaughtHandler(print, Option(Thread.getDefaultUncaughtExceptionHandler))
    )
    IORuntime.global

  /** Asks `runtime` for a trivial IO from a plain thread, so a runtime that rejects work or never runs it cannot block
    * the caller.
    */
  def probe(runtime: IORuntime, within: FiniteDuration): Liveness =
    val answered = CountDownLatch(1)
    val rejected = AtomicReference(Option.empty[Throwable])
    val submit: Runnable = () =>
      Try(IO.unit.unsafeRunAsync(_ => answered.countDown())(using runtime)).failed.foreach { error =>
        rejected.set(Some(error))
        answered.countDown()
      }
    val prober = Thread(submit, "runtime-liveness-probe")
    prober.setDaemon(true)
    prober.start()
    if answered.await(within.toNanos, NANOSECONDS) then
      rejected.get.fold(Liveness.Responsive)(error => Liveness.Unresponsive(s"the runtime rejected work: $error"))
    else Liveness.Unresponsive(s"no answer to a trivial IO within $within")

  /** `current` is read last: asking a shut-down global runtime for itself starts a fresh one. */
  def unresponsiveReport(cause: String, probed: IORuntime, current: => IORuntime): String =
    val stacks = HangReport.allThreadStacks()
    val pool   = describePool(probed eq current, stacks)
    ("[HANG-REPORT] GLOBAL IORuntime unresponsive: " + cause :: pool.map("  " + _) ::: List(
      HangReport.renderDump(stacks)
    ))
      .mkString(System.lineSeparator)

  def describePool(probedIsCurrent: Boolean, stacks: List[HangReport.ThreadStack]): List[String] =
    val (blockers, workers) = stacks.filter(_.name.startsWith("io-compute")).partition(_.name.contains("-blocker"))
    val workerStates        = workers.groupMapReduce(_.state)(_ => 1)(_ + _).toList.sortBy(_._1).map((s, n) => s"$n $s")
    val interrupted         = (workers ::: blockers).filter(_.interrupted).map(_.name).sorted
    List(
      if probedIsCurrent then "the probed runtime is still the global one"
      else "the probed runtime is no longer the global one: it was shut down and the global replaced",
      s"io-compute workers: ${workers.size} ${workerStates.mkString("[", ", ", "]")}",
      s"io-compute-blocker threads: ${blockers.size}",
      s"interrupted runtime threads: ${if interrupted.isEmpty then "none" else interrupted.mkString(", ")}"
    )

  def abortRun(cause: String): Unit =
    print(s"[RUNTIME-DEATH] aborting the test run instead of waiting for the CI guard: $cause")
    Runtime.getRuntime.halt(AbortExitCode)

end RuntimeWatch
