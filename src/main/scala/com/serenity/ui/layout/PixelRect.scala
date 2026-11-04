package com.serenity.ui.layout

/** An axis-aligned rectangle in logical (device-scale independent) pixels. */
final case class PixelRect(xPx: Int, yPx: Int, widthPx: Int, heightPx: Int):

  def rightPx: Int = xPx + widthPx

  def bottomPx: Int = yPx + heightPx

  def contains(pixelX: Int, pixelY: Int): Boolean =
    pixelX >= xPx && pixelX < rightPx && pixelY >= yPx && pixelY < bottomPx

  /** Whether this rectangle shares any pixels with `other`. Touching edges (zero-area overlap) do not count. */
  def intersects(other: PixelRect): Boolean =
    xPx < other.rightPx && other.xPx < rightPx && yPx < other.bottomPx && other.yPx < bottomPx

  /** The pixels both rectangles cover, if any. */
  def intersection(other: PixelRect): Option[PixelRect] =
    val left   = xPx.max(other.xPx)
    val top    = yPx.max(other.yPx)
    val right  = rightPx.min(other.rightPx)
    val bottom = bottomPx.min(other.bottomPx)
    Option.when(left < right && top < bottom)(PixelRect(left, top, right - left, bottom - top))

  /** The smallest rectangle covering both, used to fold a set of dirty rows into one repaint region. */
  def union(other: PixelRect): PixelRect =
    val left   = math.min(xPx, other.xPx)
    val top    = math.min(yPx, other.yPx)
    val right  = math.max(rightPx, other.rightPx)
    val bottom = math.max(bottomPx, other.bottomPx)
    PixelRect(left, top, right - left, bottom - top)

object PixelRect:

  def unionOf(rects: Iterable[PixelRect]): Option[PixelRect] =
    rects.reduceOption(_.union(_))

  /** How many separate rects a repaint is split into before [[coalesced]] gives up and covers their union: each rect is
    * its own blit, and past a handful the per-blit cost outweighs the pixels left out between them.
    */
  val RepaintRectLimit: Int = 8

  /** `rects` with every pair that overlaps or shares an edge merged into their union, until no two touch; empty rects
    * are dropped. More than `maxRects` pieces fall back to the union of them all.
    */
  def coalesced(rects: Iterable[PixelRect], maxRects: Int): List[PixelRect] =
    val pieces = rects.iterator
      .filter(rect => rect.widthPx > 0 && rect.heightPx > 0)
      .toList
      .sortBy(rect => (rect.yPx, rect.xPx))
      .foldLeft(List.empty[PixelRect])(absorb)
    if pieces.sizeIs > maxRects then unionOf(pieces).toList
    else pieces.sortBy(rect => (rect.yPx, rect.xPx))

  @annotation.tailrec
  private def absorb(merged: List[PixelRect], rect: PixelRect): List[PixelRect] =
    val (touching, apart) = merged.partition(touches(rect))
    if touching.isEmpty then rect :: apart
    else absorb(apart, touching.foldLeft(rect)(_.union(_)))

  /** Overlapping, or abutting along a shared stretch of edge; rects meeting only at a corner stay apart, since their
    * union would cover two empty quadrants.
    */
  private def touches(a: PixelRect)(b: PixelRect): Boolean =
    val xOverlap = a.xPx < b.rightPx && b.xPx < a.rightPx
    val yOverlap = a.yPx < b.bottomPx && b.yPx < a.bottomPx
    val xMeet    = a.xPx <= b.rightPx && b.xPx <= a.rightPx
    val yMeet    = a.yPx <= b.bottomPx && b.yPx <= a.bottomPx
    (xOverlap && yMeet) || (yOverlap && xMeet)

  /** The pixels of `bounds` no rectangle in `holes` covers, as disjoint rectangles: horizontal bands split at every
    * hole edge, with vertically adjacent bands of the same shape merged into one.
    *
    * The rows are swept once, top to bottom, keeping only the holes that cross the band being built: a frame that keeps
    * rows hands in a hole per kept row, and a pass over all of them for every band was bands times holes.
    */
  def uncoveredWithin(bounds: PixelRect, holes: List[PixelRect]): List[PixelRect] =
    val clipped = holes.iterator.flatMap(clippedTo(bounds)).toArray
    java.util.Arrays.sort(clipped, java.util.Comparator.comparingInt[PixelRect](_.yPx))
    val edges = java.util.Arrays
      .stream(Array.tabulate(clipped.length * 2 + 2) {
        case 0 => bounds.yPx
        case 1 => bounds.bottomPx
        case i => if i % 2 == 0 then clipped(i / 2 - 1).yPx else clipped(i / 2 - 1).bottomPx
      })
      .distinct()
      .sorted()
      .toArray
    val swept = Iterator
      .range(0, edges.length - 1)
      .foldLeft(Sweep(next = 0, crossing = Vector.empty, bands = Nil)) { (sweep, row) =>
        val top = edges(row)
        val started = clipped.indexWhere(_.yPx > top, sweep.next) match
          case -1  => clipped.length
          case end => end
        val crossing = sweep.crossing.filter(_.bottomPx > top) ++ clipped.slice(sweep.next, started)
        val band     = Band(top, edges(row + 1), spansBetween(bounds.xPx, bounds.rightPx, crossing))
        Sweep(started, crossing, sweep.bands.joinedWith(band))
      }
    swept.bands.reverse.flatMap { band =>
      band.spans.grouped(2).map(span => PixelRect(span(0), band.top, span(1) - span(0), band.bottom - band.top))
    }

  final private case class Sweep(next: Int, crossing: Vector[PixelRect], bands: List[Band])

  /** The rows from `top` to `bottom`, with the stretches across them no hole covers: left and right edges, alternating,
    * left to right.
    */
  final private case class Band(top: Int, bottom: Int, spans: Array[Int]):
    def hasSameSpansAs(other: Band): Boolean = java.util.Arrays.equals(spans, other.spans)

  extension (bands: List[Band])

    private def joinedWith(band: Band): List[Band] =
      bands match
        case latest :: earlier if latest.bottom == band.top && latest.hasSameSpansAs(band) =>
          latest.copy(bottom = band.bottom) :: earlier
        case _ => band :: bands

  private def clippedTo(bounds: PixelRect)(rect: PixelRect): Option[PixelRect] =
    val left   = rect.xPx.max(bounds.xPx)
    val top    = rect.yPx.max(bounds.yPx)
    val right  = rect.rightPx.min(bounds.rightPx)
    val bottom = rect.bottomPx.min(bounds.bottomPx)
    Option.when(left < right && top < bottom)(PixelRect(left, top, right - left, bottom - top))

  /** The stretches of `left` to `right` that `covering` leaves bare, as left and right edges, alternating. */
  private def spansBetween(left: Int, right: Int, covering: Vector[PixelRect]): Array[Int] =
    val keys = Array.tabulate(covering.length)(i => spanKey(covering(i).xPx, covering(i).rightPx))
    java.util.Arrays.sort(keys)
    val spans = scala.collection.mutable.ArrayBuilder.ofInt()
    @annotation.tailrec
    def bare(index: Int, cursor: Int): Int =
      if index == keys.length then cursor
      else
        val coveredLeft = (keys(index) >> 32).toInt
        if coveredLeft > cursor then
          spans += cursor
          spans += coveredLeft
        bare(index + 1, cursor.max(((keys(index) & 0xffffffffL) + Int.MinValue).toInt))
    val cursor = bare(0, left)
    if cursor < right then
      spans += cursor
      spans += right
    spans.result()

  /** Orders by left, then right, as a pair of Ints would, without boxing one. */
  private def spanKey(left: Int, right: Int): Long = (left.toLong << 32) | (right.toLong - Int.MinValue)
