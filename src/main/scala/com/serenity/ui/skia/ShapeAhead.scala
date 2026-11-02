package com.serenity.ui.skia

import java.util.concurrent.atomic.{AtomicBoolean, AtomicInteger, AtomicLong}
import java.util.concurrent.locks.LockSupport
import java.util.concurrent.{LinkedBlockingQueue, ThreadFactory, ThreadPoolExecutor, TimeUnit}

import scala.annotation.tailrec
import scala.concurrent.duration.FiniteDuration

import org.jetbrains.skia.Font
import org.jetbrains.skia.shaper.Shaper

/** Shapes `(text, font)` requests on worker threads into a [[SkiaShapedTextCache]], so that a frame which asks for the
  * same text finds it shaped. The cache holds a pure function of its key, so what a worker puts there is what the frame
  * would have shaped itself and a frame never waits for a worker: on a miss it shapes, as it always has.
  *
  * Each worker has a shaper of its own, since one holds state between calls; they take none of the cache's locks while
  * they shape. Workers end after a few idle seconds and are made again by the next request, so a cache that is left
  * behind leaves no thread behind.
  */
final class ShapeAhead(cache: SkiaShapedTextCache, workers: Int = ShapeAhead.DefaultWorkers):
  import ShapeAhead.*

  private val generation = new AtomicLong(0L)
  private val pending    = new AtomicInteger(0)
  private val failed     = new AtomicLong(0L)
  private val closed     = new AtomicBoolean(false)
  private val queue      = new LinkedBlockingQueue[Runnable]()
  private val shapers    = new ThreadLocal[Shaper]()

  private val pool =
    val executor = new ThreadPoolExecutor(workers, workers, IdleSeconds, TimeUnit.SECONDS, queue, WorkerThreads)
    executor.allowCoreThreadTimeOut(true)
    executor

  /** Asks for each of `batch` to be shaped, in the order given. Returns at once. */
  def request(batch: Seq[(String, Font)]): Unit =
    if !closed.get then
      val asked = generation.get
      batch.foreach { (text, font) =>
        val _ = pending.incrementAndGet()
        try pool.execute(() => work(asked, text, font))
        catch
          case _: java.util.concurrent.RejectedExecutionException =>
            val _ = pending.decrementAndGet()
      }

  /** What has not started is dropped, and what a worker is shaping at the moment finishes. */
  def cancel(): Unit =
    val _       = generation.incrementAndGet()
    val dropped = new java.util.ArrayList[Runnable]()
    val _       = queue.drainTo(dropped)
    val _       = pending.addAndGet(-dropped.size)

  /** Stops the workers and waits for them. Requests made afterwards are ignored. */
  def close(): Unit =
    if closed.compareAndSet(false, true) then
      cancel()
      pool.shutdownNow()
      val _ = pool.awaitTermination(5, TimeUnit.SECONDS)

  /** Whether every request made so far has been shaped or dropped, waiting up to `timeout` for it. */
  def awaitIdle(timeout: FiniteDuration): Boolean =
    val deadline = System.nanoTime() + timeout.toNanos
    @tailrec def look(): Boolean =
      if pending.get <= 0 then true
      else if System.nanoTime() >= deadline then false
      else
        LockSupport.parkNanos(PollNanos)
        look()
    look()

  /** Worker threads that exist now. */
  def threadsAlive: Int = pool.getPoolSize

  /** Requests whose shaping threw. A failure never stops a worker; it is counted so that a spec can insist on none. */
  def failures: Long = failed.get

  private def work(asked: Long, text: String, font: Font): Unit =
    try
      if asked == generation.get && !closed.get then
        val shaper = Option(shapers.get).getOrElse {
          val made = cache.newShaper()
          shapers.set(made)
          made
        }
        cache.shapeAhead(text, font, shaper)
    catch
      case _: Throwable =>
        val _ = failed.incrementAndGet()
    finally
      val _ = pending.decrementAndGet()

object ShapeAhead:

  /** One fewer than the processors, at most two: shaping ahead must leave the drawing thread its core. A first choice,
    * not a measurement.
    */
  val DefaultWorkers: Int = math.max(1, math.min(2, Runtime.getRuntime.availableProcessors - 1))

  private val IdleSeconds = 5L
  private val PollNanos   = 500_000L

  private val Names = new AtomicInteger(0)

  private object WorkerThreads extends ThreadFactory:

    def newThread(task: Runnable): Thread =
      val thread = new Thread(task, s"serenity-shape-ahead-${Names.incrementAndGet()}")
      thread.setDaemon(true)
      thread.setPriority(Thread.MIN_PRIORITY)
      thread
