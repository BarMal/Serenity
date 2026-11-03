package com.serenity.ui.benchmark

import com.serenity.ui.skia.WarmupStats

/** What the Skia caches did in a cold frame, per frame on average over the frames measured: faces built, shapers made
  * and text shaped, each as a count and the milliseconds it took.
  */
final case class ColdFramePhases(
    frames: Int,
    facesPerFrame: Double,
    faceMsPerFrame: Double,
    shapersPerFrame: Double,
    shaperMsPerFrame: Double,
    shapingsPerFrame: Double,
    shapingMsPerFrame: Double
)

object ColdFramePhases:

  private val NanosPerMilli = 1e6

  /** The phases of `frames` cold frames whose caches did `total` between them. */
  def of(frames: Int, total: WarmupStats.Snapshot): Option[ColdFramePhases] =
    Option.when(frames > 0) {
      def perFrame(value: Double): Double = value / frames
      ColdFramePhases(
        frames = frames,
        facesPerFrame = perFrame(total.faces.toDouble),
        faceMsPerFrame = perFrame(total.faceNanos / NanosPerMilli),
        shapersPerFrame = perFrame(total.shapers.toDouble),
        shaperMsPerFrame = perFrame(total.shaperNanos / NanosPerMilli),
        shapingsPerFrame = perFrame(total.shapings.toDouble),
        shapingMsPerFrame = perFrame(total.shapingNanos / NanosPerMilli)
      )
    }
