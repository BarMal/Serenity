package com.serenity.diagnostics

import java.util.concurrent.atomic.AtomicLong

/** The time one owner's frames have spent building their scene (layout), drawing it and flushing the surface, which the
  * render benchmark reads to split a cold frame and the render loop reads to split each frame in `[FRAME]`. The phases
  * are timed on whatever backend draws, so the Skia layer and the raster can be compared. A flush happens inside a
  * draw, so a draw's time includes its flush; [[FramePhaseClock.split]] takes it out.
  */
final class FramePhaseClock(nanoTime: () => Long = () => System.nanoTime()):

  private val layoutNanos = new AtomicLong(0L)
  private val drawNanos   = new AtomicLong(0L)
  private val flushNanos  = new AtomicLong(0L)

  def layout[A](work: => A): A = timed(layoutNanos)(work)

  def draw[A](work: => A): A = timed(drawNanos)(work)

  def flush[A](work: => A): A = timed(flushNanos)(work)

  def snapshot: FramePhaseClock.Snapshot = FramePhaseClock.Snapshot(layoutNanos.get, drawNanos.get, flushNanos.get)

  private def timed[A](total: AtomicLong)(work: => A): A =
    val started = nanoTime()
    try work
    finally
      val _ = total.addAndGet(nanoTime() - started)

object FramePhaseClock:

  final case class Snapshot(layoutNanos: Long, drawNanos: Long, flushNanos: Long = 0L)

  /** What the frames between two snapshots spent on each stage, the draw without the flush inside it. */
  def split(before: Snapshot, after: Snapshot): Map[FramePhase, Long] =
    val flush = after.flushNanos - before.flushNanos
    Map(
      FramePhase.RenderLayout -> (after.layoutNanos - before.layoutNanos),
      FramePhase.RenderDraw   -> (after.drawNanos - before.drawNanos - flush).max(0L),
      FramePhase.RenderFlush  -> flush
    )
