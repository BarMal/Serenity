package com.serenity.ui.benchmark

import com.serenity.diagnostics.FramePhaseClock

/** Where a cold frame's time goes on one backend, per frame on average: the render call, split into building the scene
  * (layout), drawing it and the rest, and the wait from the call returning to the frame being presented.
  */
final case class ColdFrameSplit(
    frames: Int,
    renderMs: Double,
    layoutMs: Double,
    drawMs: Double,
    otherMs: Double,
    presentMs: Double
)

object ColdFrameSplit:

  private val NanosPerMilli = 1e6

  /** One cold frame: how long its render call took, how long until it was presented, and what the phase clock counted.
    */
  final case class Sample(renderNanos: Long, presentedNanos: Long, phases: FramePhaseClock.Snapshot)

  def of(samples: Seq[Sample]): Option[ColdFrameSplit] =
    Option.when(samples.nonEmpty) {
      def meanMs(nanos: Sample => Long): Double = samples.map(nanos).sum / NanosPerMilli / samples.size
      val render                                = meanMs(_.renderNanos)
      val layout                                = meanMs(_.phases.layoutNanos)
      val draw                                  = meanMs(_.phases.drawNanos)
      ColdFrameSplit(
        frames = samples.size,
        renderMs = render,
        layoutMs = layout,
        drawMs = draw,
        otherMs = (render - layout - draw).max(0.0),
        presentMs = meanMs(sample => (sample.presentedNanos - sample.renderNanos).max(0L))
      )
    }
