package com.serenity.diagnostics

import java.lang.management.{ManagementFactory, ThreadMXBean}
import java.util.concurrent.TimeUnit
import java.util.concurrent.atomic.AtomicReference

import scala.concurrent.duration.*

import cats.effect.IO
import cats.effect.unsafe.IORuntimeConfig
import org.slf4j.LoggerFactory

/** The bound on how long Serenity may take to end (#2507), and what it leaves behind when it overruns. One per process,
  * owned by `Main`, which also owns the [[ShutdownLedger]] it reports from.
  *
  * Two paths end a JVM, and each is bounded separately. A signal runs cats-effect's shutdown hook, which cancels the
  * program and waits for its finalizers; [[ShutdownWatchdog.withHookTimeout]] gives that wait a limit, and
  * [[installSignalDump]] writes the open steps and every thread's stack shortly before the limit. A quit from inside
  * makes `IOApp.main` return when the program has finished, with no hook and no limit; [[armHalt]] covers that: once
  * the editor has shut down and saved, the JVM has [[ShutdownWatchdog.HookTimeout]] to finish releasing, or it dumps
  * and halts.
  */
final class ShutdownWatchdog(val ledger: ShutdownLedger):

  import ShutdownWatchdog.*

  private val armed = new AtomicReference[Option[Thread]](None)

  /** Registers a JVM shutdown hook that, `after` the hook starts, reports what the shutdown is still doing. The hook
    * itself returns at once: the JVM waits for every hook, so a hook that slept would delay every normal exit. The
    * reporting thread is a daemon and dies with the JVM when the shutdown finishes in time.
    */
  def installSignalDump(
    after: FiniteDuration = HookTimeout - DumpBeforeTimeout,
    report: String => Unit = defaultReport
  ): Unit =
    val starter = new Thread(
      () =>
        daemon("serenity-shutdown-dump") {
          try
            TimeUnit.MILLISECONDS.sleep(after.toMillis)
            report(
              describe(
                s"still shutting down ${after.toSeconds} s after the JVM began to",
                ledger.unfinished,
                captureThreads()
              )
            )
          catch case _: InterruptedException => ()
        }.start(),
      "serenity-shutdown-dump-starter"
    )
    Runtime.getRuntime.addShutdownHook(starter)

  /** Starts the clock on a quit: if the JVM is still alive after `bound`, it reports and halts with `exitCode`. A halt
    * skips the shutdown hooks, which is the point: they are what has not finished. Arming again replaces the clock.
    */
  def armHalt(
    bound: FiniteDuration = HookTimeout,
    exitCode: Int = HaltExitCode,
    report: String => Unit = defaultReport,
    halt: Int => Unit = code => Runtime.getRuntime.halt(code),
    awtState: () => AwtStateProbe.State = AwtStateProbe.current,
    snapshotGap: FiniteDuration = DumpBeforeTimeout
  ): IO[Unit] = IO {
    val watchdog = daemon("serenity-quit-watchdog") {
      try
        val gap = snapshotGap.min(bound)
        TimeUnit.MILLISECONDS.sleep((bound - gap).toMillis)
        val before = AwtStateProbe.describeRead(s"${(bound - gap).toMillis} ms after the quit", awtState)
        TimeUnit.MILLISECONDS.sleep(gap.toMillis)
        val after = AwtStateProbe.describeRead(s"${bound.toMillis} ms after the quit", awtState)
        report(
          describe(
            s"quit not finished ${bound.toSeconds} s after the editor shut down",
            ledger.unfinished,
            captureThreads(),
            before ++ after
          )
        )
        halt(exitCode)
      catch case _: InterruptedException => ()
    }
    armed.getAndSet(Some(watchdog)).foreach(_.interrupt())
    watchdog.start()
  }

  /** Stops the clock, for a quit that is followed by a restart in the same JVM. */
  val disarm: IO[Unit] = IO(armed.getAndSet(None).foreach(_.interrupt()))

object ShutdownWatchdog:

  val HookTimeout: FiniteDuration       = 10.seconds
  val DumpBeforeTimeout: FiniteDuration = 2.seconds

  /** `EX_SOFTWARE`: the process ended itself because its own shutdown did not finish. */
  val HaltExitCode: Int = 70

  final case class ThreadSnapshot(name: String, daemon: Boolean, state: String, stack: List[String])

  def withHookTimeout(config: IORuntimeConfig, timeout: FiniteDuration = HookTimeout): IORuntimeConfig =
    config.copy(shutdownHookTimeout = timeout)

  /** Stacks only: asking for locked synchronizers makes the JVM walk the heap at a safepoint, which took about nine
    * seconds on a CI Mac, longer than the margin before the halt, and the snapshot holds no lock information anyway.
    */
  def captureThreads(threads: ThreadMXBean = ManagementFactory.getThreadMXBean): List[ThreadSnapshot] =
    threads
      .dumpAllThreads(false, false)
      .toList
      .map(info =>
        ThreadSnapshot(
          info.getThreadName,
          info.isDaemon,
          info.getThreadState.toString,
          info.getStackTrace.toList.map(_.toString)
        )
      )

  def describe(
    reason: String,
    unfinished: List[String],
    threads: List[ThreadSnapshot],
    awt: List[String] = Nil
  ): String =
    val steps =
      if unfinished.isEmpty then "no step is releasing, so the shutdown is not waiting inside a traced release"
      else s"steps that started releasing and have not finished: ${unfinished.mkString(", ")}"
    val blocks = threads.flatMap { thread =>
      val flags = if thread.daemon then s"${thread.state}, daemon" else thread.state
      s"\"${thread.name}\" ($flags)" :: thread.stack.map(frame => s"\tat $frame")
    }
    (s"[SHUTDOWN] $reason" :: s"[SHUTDOWN] $steps" :: awt ++ ("[SHUTDOWN] threads:" :: blocks)).mkString("\n")

  /** Logs the dump, and also prints it: the log may already be closing, and a terminal launch silences stderr, which
    * costs nothing.
    */
  val defaultReport: String => Unit = text =>
    LoggerFactory.getLogger("ShutdownWatchdog").warn(text)
    System.err.println(text)

  private def daemon(name: String)(body: => Unit): Thread =
    val thread = new Thread(() => body, name)
    thread.setDaemon(true)
    thread
