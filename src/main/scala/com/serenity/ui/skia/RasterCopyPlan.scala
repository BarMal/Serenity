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

  /** Rows of the surface moved by `dyPx` device rows before the rest of a frame was painted. */
  final case class Move(area: DeviceArea, dyPx: Int)

  /** A frame that was copied into `image`, and `damage` against the frame before it (`None` if unknown). `moves` are
    * the rows the frame moved before it painted, and `writes` everything it painted after (`None` if unknown): a frame
    * that moved rows and painted over some of them changed, besides the move, only what it painted and what its damage
    * names outside the rows it moved.
    */
  final case class Filled(
      image: AnyRef,
      damage: Option[List[DeviceArea]],
      moves: List[Move] = Nil,
      writes: Option[List[DeviceArea]] = None
  )

  /** What to do to an image before it shows a frame: move rows in it as the frames it missed moved them, then copy
    * these areas from the surface.
    */
  final case class Plan(shifts: List[Move], copies: List[DeviceArea])

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

  /** Like [[areas]], but a frame that scrolled is not copied whole: the image moves the rows itself, as the surface
    * did, and only what the frames painted over them, or changed outside them, comes from the surface. The frames the
    * image has missed are the ones whose moves and paint are known; any that is not, or any case [[areas]] copies
    * whole, copies as [[areas]] does.
    */
  def plan(
    acquired: AnyRef,
    last: Option[Filled],
    beforeLast: Option[Filled],
    now: Filled,
    whole: DeviceArea
  ): Plan =
    val fallback = Plan(Nil, areas(acquired, last, beforeLast, now.damage, whole))
    val planned =
      for
        previous <- last
        current  <- changedBesidesMoves(now, whole)
        plan <-
          if previous.image eq acquired then Option.when(now.moves.nonEmpty)(Plan(now.moves, disjoint(current)))
          else if beforeLast.exists(_.image eq acquired) && (now.moves.nonEmpty || previous.moves.nonEmpty) then
            changedBesidesMoves(previous, whole).map { earlier =>
              Plan(previous.moves ++ now.moves, disjoint(current ++ earlier ++ carried(earlier, now.moves)))
            }
          else None
      yield plan
    planned.getOrElse(fallback)

  /** What `frame` changed other than by moving rows: its damage outside the rows it moved, and what it painted over
    * them.
    */
  private def changedBesidesMoves(frame: Filled, whole: DeviceArea): Option[List[DeviceArea]] =
    if frame.moves.isEmpty then frame.damage
    else
      val moved = DeviceAreas.within(frame.moves.map(move => DeviceAreas.shifted(move.area, move.dyPx)), whole)
      for
        damage <- frame.damage
        writes <- frame.writes
      yield DeviceAreas.subtract(damage, moved) ++ DeviceAreas.intersect(writes, moved)

  /** Where `areas` are after `moves` are made in order: the rows they lay in that a move took, at the place it took
    * them to.
    */
  private def carried(areas: List[DeviceArea], moves: List[Move]): List[DeviceArea] =
    moves.foldLeft(List.empty[DeviceArea]) { (carried, move) =>
      carried ++ DeviceAreas.intersect(areas ++ carried, List(move.area)).map(DeviceAreas.shifted(_, move.dyPx))
    }
