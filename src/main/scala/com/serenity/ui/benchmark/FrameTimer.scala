package com.serenity.ui.benchmark

import java.util.concurrent.atomic.AtomicReference

import scala.annotation.tailrec

/** Times one frame from its start to the last time it was presented, once nothing has been presented for a moment.
  *
  * A presentation counts only if it showed at least the publishes the frame made: a layer can draw an older picture
  * while a slow frame is still being rendered, and ending the frame there would time the old draw, not the frame.
  */
final class FrameTimer(clock: () => Long, pause: () => Unit, quietNanos: Long, dropAfterNanos: Long):

  final private case class Presentation(atNanos: Long, shownPublishes: Int)

  private val seen = new AtomicReference(Vector.empty[Presentation])

  def presented(shownPublishes: Int): Unit =
    val at = clock()
    val _  = seen.updateAndGet(_ :+ Presentation(at, shownPublishes))

  /** The nanoseconds from `start` to the last presentation, or `None` if the frame's publishes were never shown. */
  @tailrec
  def await(start: Long, publishesNeeded: Int): Option[Long] =
    val now     = clock()
    val shown   = seen.get().filter(_.shownPublishes >= publishesNeeded)
    val settled = shown.lastOption.filter(last => now - last.atNanos >= quietNanos)
    if settled.isDefined then settled.map(_.atNanos - start)
    else if now - start > dropAfterNanos then shown.lastOption.map(_.atNanos - start)
    else
      pause()
      await(start, publishesNeeded)
