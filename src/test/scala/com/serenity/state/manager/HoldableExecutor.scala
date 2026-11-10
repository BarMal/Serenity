package com.serenity.state.manager

import java.util.concurrent.atomic.AtomicBoolean
import java.util.concurrent.{CountDownLatch, Executor, Executors, TimeUnit}

import scala.concurrent.duration.FiniteDuration

/** Runs the tasks it is handed on `inner`, and while holding keeps each one from starting until released, so a spec can
  * fix when a count ends relative to the notices for the texts before it.
  */
final private[manager] class HoldableExecutor(inner: Executor) extends Executor:

  private val holding = new AtomicBoolean(false)
  private val gate    = new CountDownLatch(1)

  /** Every task handed in from now on waits for [[releaseAfter]]. */
  def hold(): Unit = holding.set(true)

  /** Tasks handed in from now on start at once; those already held still wait. */
  def stopHolding(): Unit = holding.set(false)

  def releaseAfter(delay: FiniteDuration): Unit =
    val timer = Executors.newSingleThreadScheduledExecutor { runnable =>
      val thread = new Thread(runnable, "holdable-executor-release")
      thread.setDaemon(true)
      thread
    }
    val _ = timer.schedule((() => gate.countDown()): Runnable, delay.toMillis, TimeUnit.MILLISECONDS)
    timer.shutdown()

  override def execute(task: Runnable): Unit =
    val held = holding.get
    inner.execute { () =>
      if held then gate.await(60, TimeUnit.SECONDS): Unit
      task.run()
    }
