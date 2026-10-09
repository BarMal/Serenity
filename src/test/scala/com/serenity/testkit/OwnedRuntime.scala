package com.serenity.testkit

import scala.concurrent.duration.DurationInt

import cats.effect.unsafe.{IORuntime, IORuntimeConfig}

/** A runtime a test builds and shuts down itself. Its compute threads must not carry the `io-compute` prefix of the
  * global runtime: [[RuntimeShutdownWatch]] reads every thread under that prefix as the global pool, and a normal
  * shutdown interrupts all workers of a pool.
  */
object OwnedRuntime:

  val ThreadPrefix = "owned-compute"

  def build(): IORuntime =
    val (compute, _, stopCompute) = IORuntime.createWorkStealingComputeThreadPool(
      threads = 2,
      threadPrefix = ThreadPrefix,
      blockerThreadPrefix = s"$ThreadPrefix-blocker",
      runtimeBlockingExpiration = 3.seconds
    )
    val (blocking, stopBlocking) = IORuntime.createDefaultBlockingExecutionContext(s"$ThreadPrefix-blocking")
    val (scheduler, stopScheduler) =
      IORuntime.createDefaultScheduler(s"$ThreadPrefix-scheduler")
    IORuntime(
      compute,
      blocking,
      scheduler,
      () =>
        stopCompute()
        stopBlocking()
        stopScheduler()
      ,
      IORuntimeConfig()
    )

end OwnedRuntime
