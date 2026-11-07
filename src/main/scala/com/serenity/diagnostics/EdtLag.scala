package com.serenity.diagnostics

import java.util.concurrent.atomic.{AtomicBoolean, AtomicReference}

import scala.concurrent.duration.*

import cats.effect.IO
import fs2.Stream

/** How late the event thread runs what is posted to it: the wait a key event would see if it arrived then, which no
  * stage of a frame's own timing covers when the thread is busy elsewhere (#2489). Off until frame timing is on.
  */
final class EdtLagTrace:
  private val enabled = new AtomicBoolean(false)
  private val samples = new AtomicReference[Vector[Long]](Vector.empty)

  def isEnabled: Boolean = enabled.get()

  /** Starts from an empty window either way, so nothing sampled before a toggle is reported after it. */
  def setEnabled(on: Boolean): Unit =
    samples.set(Vector.empty)
    enabled.set(on)

  def record(lagNanos: Long): Unit =
    if enabled.get() then
      val _ = samples.updateAndGet(current => (current :+ lagNanos).takeRight(EdtLagTrace.MaxSamples))

  def drain(): PhaseStats = PhaseStats.of(samples.getAndSet(Vector.empty))

object EdtLagTrace:
  /** A report window holds about a hundred samples; this only stops a window nobody drains from growing. */
  val MaxSamples: Int = 4096

object EdtLagProbe:

  /** Often enough to see a stall of a few tens of milliseconds, rare enough to cost the event thread nothing. */
  val Interval: FiniteDuration = 50.millis

  /** Posts a no-op to the event thread and records how long it waited there before it ran. */
  def sample(post: Runnable => Unit, trace: EdtLagTrace, nanoTime: () => Long): Unit =
    val postedAt = nanoTime()
    post(() => trace.record(nanoTime() - postedAt))

  def stream(trace: EdtLagTrace, post: Runnable => Unit): Stream[IO, Unit] =
    Stream.awakeEvery[IO](Interval).evalMap(_ => IO(sample(post, trace, () => System.nanoTime())))
