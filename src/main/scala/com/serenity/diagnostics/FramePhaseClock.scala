package com.serenity.diagnostics

import java.util.concurrent.atomic.AtomicLong

/** The time one owner's frames have spent building their scene (layout) and drawing it, which the render benchmark
  * reads to split a cold frame. The two phases are timed on whatever backend draws, so Java2D and Skia can be compared.
  */
final class FramePhaseClock(nanoTime: () => Long = () => System.nanoTime()):

  private val layoutNanos = new AtomicLong(0L)
  private val drawNanos   = new AtomicLong(0L)

  def layout[A](work: => A): A = timed(layoutNanos)(work)

  def draw[A](work: => A): A = timed(drawNanos)(work)

  def snapshot: FramePhaseClock.Snapshot = FramePhaseClock.Snapshot(layoutNanos.get, drawNanos.get)

  private def timed[A](total: AtomicLong)(work: => A): A =
    val started = nanoTime()
    try work
    finally
      val _ = total.addAndGet(nanoTime() - started)

object FramePhaseClock:

  final case class Snapshot(layoutNanos: Long, drawNanos: Long)
