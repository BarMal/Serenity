package com.serenity.state.manager

import java.util.concurrent.{Executor, Executors}

/** The one thread that measures the rows a frame left unmeasured. */
object ScrollCountExecutor:

  /** Daemon, so a count still running can never keep the process alive once the window has closed (#1543); a count
    * checks for its replacement between lines, so none outlives its use for long.
    */
  lazy val shared: Executor =
    Executors.newSingleThreadExecutor { runnable =>
      val thread = new Thread(runnable, "serenity-scroll-count")
      thread.setDaemon(true)
      thread
    }
