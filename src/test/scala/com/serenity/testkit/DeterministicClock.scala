package com.serenity.testkit

import java.util.concurrent.atomic.AtomicLong

import scala.concurrent.duration.*

import cats.effect.unsafe.{IORuntime, Scheduler}

/** An `IORuntime` whose monotonic clock a spec controls: every `IO.monotonic` on it reads `step` later than the last,
  * and [[advance]] moves it on by a gap. `IO.sleep` and `IO.realTime` are the real ones.
  *
  * A spec that compares two ways of applying the same events cannot let the machine's speed decide the result when the
  * code under test reads the clock: undo grouping ends an edit run after a pause of over a second between keys, and a
  * stall of that size on a loaded runner is enough to split one (#2484). Run the spec's programs on [[runtime]] and the
  * gaps between keys are the ones the spec scripts.
  */
final class DeterministicClock(step: FiniteDuration = 1.milli):

  private val nanos = AtomicLong(0L)

  /** Moves the clock on by `gap`, as a stall of that length would. */
  def advance(gap: FiniteDuration): Unit =
    val _ = nanos.addAndGet(gap.toNanos)

  private val (compute, _, shutdownCompute) = IORuntime.createWorkStealingComputeThreadPool()
  private val (blocking, shutdownBlocking)  = IORuntime.createDefaultBlockingExecutionContext()
  private val (real, shutdownScheduler)     = Scheduler.createDefaultScheduler()

  private val scripted: Scheduler = new Scheduler:
    def sleep(delay: FiniteDuration, task: Runnable): Runnable = real.sleep(delay, task)
    def nowMillis(): Long                                      = real.nowMillis()
    def monotonicNanos(): Long                                 = nanos.addAndGet(step.toNanos)

  val runtime: IORuntime =
    IORuntime
      .builder()
      .setCompute(compute, shutdownCompute)
      .setBlocking(blocking, shutdownBlocking)
      .setScheduler(scripted, shutdownScheduler)
      .build()

  /** Stops the runtime's threads; the clock is unusable afterwards. */
  def shutdown(): Unit = runtime.shutdown()
