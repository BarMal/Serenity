package com.serenity.ui.tui

/** A vertical scroll of the full-width rows `top..bottom` that the terminal performs itself (DECSTBM region plus SU or
  * SD) instead of the diff rewriting every moved row. `lines > 0` moves content up (SU), `lines < 0` moves it down
  * (SD). The rows the scroll exposes hold nothing the diff can rely on, so they are always repainted in full.
  */
final case class ScrollPlan(top: Int, bottom: Int, lines: Int):

  private val Esc = 0x1b.toChar

  /** The region is reset straight away: leaving it set would make a later line feed at its bottom margin scroll. */
  def sequence(screenHeight: Int): String =
    val scroll = s"$Esc[${math.abs(lines)}${if lines > 0 then 'S' else 'T'}"
    if top == 0 && bottom == screenHeight - 1 then scroll
    else s"$Esc[${top + 1};${bottom + 1}r$scroll$Esc[r"

  def rows: Range = top to bottom

  /** What the terminal shows on row `y` once the scroll has run, or `None` for an exposed row. */
  def rowAfterScroll(previous: TerminalFrame, y: Int): Option[Vector[TerminalCell]] =
    if y < top || y > bottom then Some(previous.cells(y))
    else
      val source = y + lines
      Option.when(source >= top && source <= bottom)(previous.cells(source))

object ScrollPlan:

  /** Fewer changed rows than this never repay the region setup. */
  private val MinChangedRows = 3

  final private case class Segment(start: Int, end: Int, gain: Int)

  final private case class Candidate(plan: ScrollPlan, gain: Int)

  /** The scroll that lets the most rows be left alone, if any. It is only a candidate: the caller keeps it only when
    * the bytes it produces really are fewer than the plain diff's.
    */
  def detect(previous: TerminalFrame, next: TerminalFrame, dirtyRows: Option[Set[Int]]): Option[ScrollPlan] =
    val height = next.height
    val changed = dirtyRows match
      case Some(rows) => Vector.tabulate(height)(rows.contains)
      case None       => Vector.tabulate(height)(y => previous.cells(y) != next.cells(y))
    if changed.count(identity) < MinChangedRows then None
    else
      val nextKeys = next.cells.map(_.hashCode)
      val prevKeys = previous.cells.map(_.hashCode)
      (1 until height)
        .flatMap(distance => List(distance, -distance))
        .flatMap(shift => candidate(previous, next, changed, prevKeys, nextKeys, shift))
        .foldLeft(Option.empty[Candidate]) { (best, found) =>
          if best.forall(_.gain < found.gain) then Some(found) else best
        }
        .map(_.plan)

  /** The best run of consecutive rows that is one `shift` away from where it was: a row that already differed and now
    * matches its shifted source gains one, a row that was fine and would now need repainting loses one.
    */
  private def candidate(
    previous: TerminalFrame,
    next: TerminalFrame,
    changed: Vector[Boolean],
    prevKeys: Vector[Int],
    nextKeys: Vector[Int],
    shift: Int
  ): Option[Candidate] =
    val height = next.height

    def weight(y: Int): Int =
      val source  = y + shift
      val matched = nextKeys(y) == prevKeys(source) && next.cells(y) == previous.cells(source)
      (matched, changed(y)) match
        case (true, true)   => 1
        case (false, false) => -1
        case _              => 0

    def extend(running: Segment, y: Int): Segment =
      if running.gain > 0 then Segment(running.start, y, running.gain + weight(y)) else Segment(y, y, weight(y))

    val mappable = math.max(0, -shift) until math.min(height, height - shift)
    val (_, best) = mappable.foldLeft((Segment(0, -1, 0), Segment(0, -1, 0))) {
      case ((running, bestSoFar), y) =>
        val extended = extend(running, y)
        (extended, if extended.gain > bestSoFar.gain then extended else bestSoFar)
    }
    val net = best.gain - math.abs(shift)
    Option.when(net > 0) {
      val plan =
        if shift > 0 then ScrollPlan(best.start, best.end + shift, shift)
        else ScrollPlan(best.start + shift, best.end, shift)
      Candidate(plan, net)
    }
