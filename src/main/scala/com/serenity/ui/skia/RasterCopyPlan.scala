package com.serenity.ui.skia

import com.serenity.ui.renderer.DeviceArea

/** Which device areas of the retained raster surface must be copied into the image a frame is presented in.
  *
  * The surface holds the previous frame, but a pooled image holds whichever frame it last carried: the previous one if
  * the pool handed back the image just presented, the one before that if it alternates between two. What it lacks is
  * what changed since, so the copy covers the damage of every frame the image has missed. Whenever that cannot be told
  * the whole frame is copied, which only ever costs time.
  */
private[ui] object RasterCopyPlan:

  /** A frame that was copied into `image`, and `damage` against the frame before it (`None` if unknown). */
  final case class Filled(image: AnyRef, damage: Option[List[DeviceArea]])

  def areas(
    acquired: AnyRef,
    last: Option[Filled],
    beforeLast: Option[Filled],
    damage: Option[List[DeviceArea]],
    whole: DeviceArea
  ): List[DeviceArea] =
    val missed =
      for
        now      <- damage
        previous <- last
        areas <-
          if previous.image eq acquired then Some(now)
          else if beforeLast.exists(_.image eq acquired) then previous.damage.map(now ++ _)
          else None
      yield areas
    missed.getOrElse(List(whole)).filterNot(_.isEmpty)
