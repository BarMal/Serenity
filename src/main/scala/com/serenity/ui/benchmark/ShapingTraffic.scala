package com.serenity.ui.benchmark

import com.serenity.diagnostics.{FrameCounter, FrameCounterStats}

/** How often a scenario's frames had text shaped as they drew, on one backend: per frame on average, and in the worst
  * frame. A promotion is a stretch found in the older generation of the shaped-text cache: shaped before, not shaped
  * again. A frame that shapes on the frame path is paying a cost the same frame would not pay with its text shaped
  * beforehand.
  */
final case class ShapingTraffic(
    frames: Int,
    shapingsMean: Double,
    shapingsMax: Long,
    promotionsMean: Double,
    promotionsMax: Long
)

object ShapingTraffic:

  /** What one frame shaped and promoted. */
  final case class Frame(shapings: Long, promotions: Long)

  object Frame:

    /** The frame the counters closed since they were last drained; none closed means nothing was shaped. */
    def of(drained: FrameCounterStats): Frame =
      Frame(
        drained.totals.getOrElse(FrameCounter.ShapingCalls, 0L),
        drained.totals.getOrElse(FrameCounter.ShapePromotions, 0L)
      )

  def of(frames: Seq[Frame]): Option[ShapingTraffic] =
    Option.when(frames.nonEmpty) {
      ShapingTraffic(
        frames.size,
        frames.map(_.shapings).sum.toDouble / frames.size,
        frames.foldLeft(0L)((largest, frame) => largest.max(frame.shapings)),
        frames.map(_.promotions).sum.toDouble / frames.size,
        frames.foldLeft(0L)((largest, frame) => largest.max(frame.promotions))
      )
    }
