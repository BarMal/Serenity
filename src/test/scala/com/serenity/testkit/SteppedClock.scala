package com.serenity.testkit

import java.util.concurrent.atomic.AtomicLong

import scala.concurrent.duration.FiniteDuration

import cats.effect.unsafe.{IORuntime, IORuntimeConfig, Scheduler}

/** A runtime whose monotonic clock moves only when something reads it, by a fixed step per read, so a program that
  * measures elapsed time between its own steps (undo grouping's one-second pause, say) sees the same intervals however
  * loaded the machine is. Unlike `TestControl` it keeps the real compute and blocking pools, so a program with real
  * threads still runs; timers (`IO.sleep`) still wait real time.
  */
object SteppedClock:

  val Step: FiniteDuration = FiniteDuration(1, "microsecond")

  /** Starts well past any pause a program might compare against, as a real monotonic clock does. */
  private val Origin = 1_000_000_000_000L

  def scheduler(delegate: Scheduler): Scheduler =
    val reads = new AtomicLong(0L)
    new Scheduler:
      def sleep(delay: FiniteDuration, task: Runnable): Runnable = delegate.sleep(delay, task)
      def nowMillis(): Long                                      = delegate.nowMillis()
      def monotonicNanos(): Long                                 = Origin + reads.incrementAndGet() * Step.toNanos

  /** Shared by every user for the life of the JVM, as the global runtime is; its threads are daemons. */
  lazy val runtime: IORuntime =
    val prefix             = "stepped-compute"
    val (compute, _, _)    = IORuntime.createWorkStealingComputeThreadPool(threadPrefix = prefix)
    val (blocking, _)      = IORuntime.createDefaultBlockingExecutionContext(s"$prefix-blocking")
    val (realScheduler, _) = IORuntime.createDefaultScheduler(s"$prefix-scheduler")
    IORuntime(compute, blocking, scheduler(realScheduler), () => (), IORuntimeConfig())

end SteppedClock
