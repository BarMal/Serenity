package com.serenity.ui.renderer

import java.util.concurrent.Executors

import scala.concurrent.ExecutionContext

import cats.effect.{IO, Resource}

/** A dedicated single-thread execution context for CPU-bound Swing/Java2D paint work.
  *
  * IO.blocking hands work to Cats Effect's unbounded, cached blocking thread pool -- meant for calls that actually
  * block (file/socket I/O), not synchronous CPU-bound painting. Routing that work onto IO's default compute pool
  * instead would be worse on a low-core machine: that pool is small (sized to availableProcessors) and shared by every
  * other fiber in the app, so any multi-millisecond paint call would stall unrelated work. A dedicated single thread
  * keeps paint work off both pools, and as a side effect gives every AWT/Swing call in the render path a single
  * consistent thread instead of whichever blocking-pool thread happened to be free.
  */
object PaintExecutionContext:

  def resource: Resource[IO, ExecutionContext] =
    Resource
      .make(IO(Executors.newSingleThreadExecutor(runnable => new Thread(runnable, "serenity-paint"))))(executor =>
        IO(executor.shutdown())
      )
      .map(ExecutionContext.fromExecutorService)
