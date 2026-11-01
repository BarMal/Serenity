package com.serenity.state.manager

import java.util.concurrent.{Executors, TimeUnit}

import scala.concurrent.duration.FiniteDuration

/** Runs a task once nothing has happened for a while. */
trait QuietTimer:

  /** Runs `task` after `delay`; the function returned cancels it. */
  def after(delay: FiniteDuration)(task: Runnable): () => Unit

object QuietTimer:

  /** Daemon, so a timer still waiting can never keep the process alive once the window has closed (#1543). */
  lazy val shared: QuietTimer =
    val scheduler = Executors.newSingleThreadScheduledExecutor { runnable =>
      val thread = new Thread(runnable, "serenity-scroll-quiet")
      thread.setDaemon(true)
      thread
    }
    new QuietTimer:
      def after(delay: FiniteDuration)(task: Runnable): () => Unit =
        val scheduled = scheduler.schedule(task, delay.toNanos, TimeUnit.NANOSECONDS)
        () => scheduled.cancel(false): Unit
