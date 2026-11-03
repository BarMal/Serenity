package com.serenity.ui.skia

import java.util.concurrent.atomic.AtomicLongArray

/** The parts of presenting a raster frame that cost time of their own. */
enum PresentPhase:
  case Plan, SurfaceMove, ImageShift, CopyOut

/** How long each [[PresentPhase]] has run since it was last [[drain]]ed, so a benchmark can say where a frame's time
  * went. Timing changes nothing a frame draws or copies.
  */
final private[serenity] class PresentPhases:

  private val nanos = new AtomicLongArray(PresentPhase.values.length)

  inline def timed[A](phase: PresentPhase)(body: => A): A =
    val started = System.nanoTime()
    try body
    finally add(phase, System.nanoTime() - started)

  def drain(): Map[PresentPhase, Long] =
    PresentPhase.values.map(phase => phase -> nanos.getAndSet(phase.ordinal, 0L)).toMap

  def add(phase: PresentPhase, elapsedNanos: Long): Unit =
    val _ = nanos.addAndGet(phase.ordinal, elapsedNanos)
