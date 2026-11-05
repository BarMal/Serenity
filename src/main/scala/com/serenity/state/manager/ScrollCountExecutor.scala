package com.serenity.state.manager

import java.util.concurrent.atomic.AtomicInteger
import java.util.concurrent.{Executor, LinkedBlockingQueue, ThreadPoolExecutor, TimeUnit}

import scala.util.Try

/** The threads that measure the rows a frame left unmeasured, so panes shown together are counted side by side (#2483).
  */
object ScrollCountExecutor:

  /** Beyond four, a count is unlikely to find a pane to run for (a document over `PaneScroll.ExactLineLimit` is never
    * counted late), while every thread adds traffic on the monitors the row indexes and wrap caches share.
    */
  val MaxThreads: Int = 4

  /** Cores left to the render and window threads. Counts are pure CPU, so a pool as wide as the machine would compete
    * with the frame they follow.
    */
  val ReservedCores: Int = 1

  /** The system property that sets the number of count threads; for measuring a size, not a user setting. */
  val ThreadsProperty: String = "serenity.scroll_count.threads"

  def defaultThreads(cores: Int): Int = (cores - ReservedCores).max(1).min(MaxThreads)

  /** `configured` when it is a number, held to between one and [[MaxThreads]]; the default for `cores` otherwise. */
  def threadsFor(cores: Int, configured: Option[String]): Int =
    configured
      .flatMap(text => Try(text.trim.toInt).toOption)
      .map(_.max(1).min(MaxThreads))
      .getOrElse(defaultThreads(cores))

  /** Daemon, so a count still running can never keep the process alive once the window has closed (#1543); a count
    * checks for its replacement between lines, so none outlives its use for long. Counts start in the order they are
    * handed in, and idle threads end.
    */
  def pool(threads: Int): Executor =
    val named = new AtomicInteger(0)
    val width = threads.max(1)
    val workers = new ThreadPoolExecutor(
      width,
      width,
      IdleSeconds,
      TimeUnit.SECONDS,
      new LinkedBlockingQueue[Runnable](),
      runnable =>
        val thread = new Thread(runnable, s"serenity-scroll-count-${named.incrementAndGet()}")
        thread.setDaemon(true)
        thread
    )
    workers.allowCoreThreadTimeOut(true)
    workers

  private val IdleSeconds = 30L

  lazy val shared: Executor =
    pool(threadsFor(Runtime.getRuntime.availableProcessors, Option(System.getProperty(ThreadsProperty))))
