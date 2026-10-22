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
    disjoint(missed.getOrElse(List(whole)))

  /** The same pixels as `areas`, as areas that share none and join where they touch: a scroll damages the same large
    * area in both frames, and copying it twice doubled the cost of the frame.
    */
  def disjoint(areas: List[DeviceArea]): List[DeviceArea] =
    val present = areas.filterNot(_.isEmpty)
    val edges   = present.flatMap(area => List(area.top, area.top + area.height)).distinct.sorted
    val bands = edges.zip(edges.drop(1)).flatMap { (top, bottom) =>
      val spans = joined(
        present
          .filter(area => area.top < bottom && area.top + area.height > top)
          .map(area => (area.left, area.left + area.width))
          .sorted
      )
      Option.when(spans.nonEmpty)(Band(top, bottom, spans))
    }
    bands
      .foldLeft(List.empty[Band]) {
        case (latest :: earlier, band) if latest.bottom == band.top && latest.spans == band.spans =>
          latest.copy(bottom = band.bottom) :: earlier
        case (done, band) => band :: done
      }
      .reverse
      .flatMap(band =>
        band.spans.map((left, right) => DeviceArea(left, band.top, right - left, band.bottom - band.top))
      )

  final private case class Band(top: Int, bottom: Int, spans: List[(Int, Int)])

  private def joined(sorted: List[(Int, Int)]): List[(Int, Int)] =
    sorted
      .foldLeft(List.empty[(Int, Int)]) {
        case ((left, right) :: earlier, (nextLeft, nextRight)) if nextLeft <= right =>
          (left, right.max(nextRight)) :: earlier
        case (done, span) => span :: done
      }
      .reverse
