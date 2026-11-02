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
    *
    * The rows are swept once, top to bottom, keeping only the areas that cover the band being built: a typing frame
    * hands it a record per spell-check squiggle, and a pass over all of them for every band cost more than the frame
    * could spare.
    */
  def disjoint(areas: List[DeviceArea]): List[DeviceArea] =
    val present = areas.iterator.filterNot(_.isEmpty).toArray
    java.util.Arrays.sort(present, java.util.Comparator.comparingInt[DeviceArea](_.top))
    val edges = java.util.Arrays
      .stream(Array.tabulate(present.length * 2)(i => present(i / 2).top + (i % 2) * present(i / 2).height))
      .distinct()
      .sorted()
      .toArray
    val swept = Iterator
      .range(0, edges.length - 1)
      .foldLeft(Sweep(next = 0, covering = Vector.empty, bands = Nil)) { (sweep, row) =>
        val top = edges(row)
        val started = present.indexWhere(_.top > top, sweep.next) match
          case -1  => present.length
          case end => end
        val covering = sweep.covering.filter(area => area.top + area.height > top) ++ present.slice(sweep.next, started)
        val band     = Band(top, edges(row + 1), spansOf(covering))
        Sweep(started, covering, sweep.bands.joinedWith(band))
      }
    stacked(
      swept.bands.reverse.flatMap(band =>
        band.spans.grouped(2).map(span => DeviceArea(span(0), band.top, span(1) - span(0), band.bottom - band.top))
      )
    )

  final private case class Sweep(next: Int, covering: Vector[DeviceArea], bands: List[Band])

  /** A band boundary cuts a column through whenever another area starts or ends beside it; each piece would be a copy
    * of its own, so pieces of one column that touch are joined again.
    */
  private def stacked(areas: List[DeviceArea]): List[DeviceArea] =
    areas
      .sortWith(columnThenTop)
      .foldLeft(List.empty[DeviceArea]) {
        case (latest :: earlier, area)
            if latest.left == area.left && latest.width == area.width && latest.top + latest.height == area.top =>
          latest.copy(height = latest.height + area.height) :: earlier
        case (done, area) => area :: done
      }
      .reverse

  /** By column, then from the top, without the key a `sortBy` would build for every comparison. */
  private def columnThenTop(a: DeviceArea, b: DeviceArea): Boolean =
    if a.left != b.left then a.left < b.left
    else if a.width != b.width then a.width < b.width
    else a.top < b.top

  /** The rows from `top` to `bottom`, covered across `spans`: left and right edges, alternating, left to right. */
  final private case class Band(top: Int, bottom: Int, spans: Array[Int]):
    def hasSameSpansAs(other: Band): Boolean = java.util.Arrays.equals(spans, other.spans)

  extension (bands: List[Band])

    private def joinedWith(band: Band): List[Band] =
      if band.spans.isEmpty then bands
      else
        bands match
          case latest :: earlier if latest.bottom == band.top && latest.hasSameSpansAs(band) =>
            latest.copy(bottom = band.bottom) :: earlier
          case _ => band :: bands

  /** What `covering` covers across, as spans that share no column and join where they touch. */
  private def spansOf(covering: Vector[DeviceArea]): Array[Int] =
    val keys = Array.tabulate(covering.length)(i => spanKey(covering(i).left, covering(i).left + covering(i).width))
    java.util.Arrays.sort(keys)
    val merged = scala.collection.mutable.ArrayBuilder.ofInt()
    @annotation.tailrec
    def join(index: Int, left: Int, right: Int): Unit =
      if index == keys.length then
        merged += left
        merged += right
      else
        val nextLeft  = (keys(index) >> 32).toInt
        val nextRight = ((keys(index) & 0xffffffffL) + Int.MinValue).toInt
        if nextLeft <= right then join(index + 1, left, right.max(nextRight))
        else
          merged += left
          merged += right
          join(index + 1, nextLeft, nextRight)
    if keys.nonEmpty then join(1, (keys(0) >> 32).toInt, ((keys(0) & 0xffffffffL) + Int.MinValue).toInt)
    merged.result()

  /** Orders by left, then right, as a pair of Ints would, without boxing one. */
  private def spanKey(left: Int, right: Int): Long = (left.toLong << 32) | (right.toLong - Int.MinValue)

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
          if previous.image eq acquired then
            Option.when(now.moves.nonEmpty)(Plan(now.moves, within(disjoint(current), whole)))
          else if beforeLast.exists(_.image eq acquired) && (now.moves.nonEmpty || previous.moves.nonEmpty) then
            changedBesidesMoves(previous, whole).map { earlier =>
              Plan(
                previous.moves ++ now.moves,
                within(disjoint(current ++ earlier ++ carried(earlier, now.moves)), whole)
              )
            }
          else None
      yield plan
    planned.getOrElse(fallback)

  private def within(areas: List[DeviceArea], whole: DeviceArea): List[DeviceArea] =
    DeviceAreas.within(areas, whole)

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
  private[skia] def carried(areas: List[DeviceArea], moves: List[Move]): List[DeviceArea] =
    moves.foldLeft(List.empty[DeviceArea]) { (carried, move) =>
      carried ++ DeviceAreas.intersect(areas ++ carried, List(move.area)).map(DeviceAreas.shifted(_, move.dyPx))
    }
