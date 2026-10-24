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

  /** The frame's time, or `None` if its publishes were never shown. Called once the frame's render call has returned.
    */
  def await(start: Long, publishesNeeded: Int): Option[FrameTimer.Sample] =
    val returned = clock()
    @tailrec
    def loop(): Option[FrameTimer.Sample] =
      val now   = clock()
      val shown = seen.get().filter(_.shownPublishes >= publishesNeeded)
      def sample(last: Presentation) =
        FrameTimer.Sample(last.atNanos - start, endedBeforeReturn = last.atNanos < returned)
      val settled = shown.lastOption.filter(last => now - last.atNanos >= quietNanos)
      if settled.isDefined then settled.map(sample)
      else if now - start > dropAfterNanos then shown.lastOption.map(sample)
      else
        pause()
        loop()
    loop()

object FrameTimer:

  /** The nanoseconds a frame took, and whether its last presentation came before its render call returned. */
  final case class Sample(nanos: Long, endedBeforeReturn: Boolean)
