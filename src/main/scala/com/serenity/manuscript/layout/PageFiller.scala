package com.serenity.manuscript.layout

import scala.annotation.tailrec

/** One line, set but not yet placed on a page. `spaceBefore` is vertical space above it, dropped at the top of a page,
  * and `pitch` the height of its box.
  */
final private[layout] case class SetLine(runs: Vector[PlacedRun], spaceBefore: Float, pitch: Float)

private[layout] enum Opening:
  case Continue
  case NewPage(kind: PageKind, drop: Float)

/** Lines that are placed together. The first `held` of them belong with whatever follows (a heading, a scene break), so
  * a page may only end after them once the lines after them have at least `orphans` lines on that page.
  */
final private[layout] case class Group(opening: Opening, lines: Vector[SetLine], held: Int)

/** Page geometry in points, as the filler needs it. `textTop` is the top margin. */
final private[layout] case class Geometry(
    textTop: Float,
    textHeight: Float,
    descent: Float,
    widows: Int,
    orphans: Int
)

final private[layout] case class FilledPage(kind: PageKind, lines: Vector[PlacedLine])

/** Pours groups of lines onto pages. It decides every page break: new-page openings, widows and orphans, and keeping
  * held lines with the text they introduce.
  */
private[layout] object PageFiller:

  private val Slack = 0.01f

  final private case class Open(kind: PageKind, cursor: Float, lines: Vector[PlacedLine]):
    def isEmpty: Boolean = lines.isEmpty

    private def top(line: SetLine): Float = if isEmpty then cursor else cursor + line.spaceBefore

    def fits(line: SetLine, geometry: Geometry): Boolean =
      top(line) + line.pitch <= geometry.textHeight + Slack

    def add(line: SetLine, geometry: Geometry): Open =
      val lineTop  = top(line)
      val baseline = geometry.textTop + lineTop + line.pitch - geometry.descent
      Open(kind, lineTop + line.pitch, lines :+ PlacedLine(baseline, line.runs))

    def addAll(added: Vector[SetLine], geometry: Geometry): Open =
      added.foldLeft(this)(_.add(_, geometry))

  final private case class State(done: Vector[FilledPage], current: Open):

    def closed: Vector[FilledPage] =
      if current.isEmpty then done else done :+ FilledPage(current.kind, current.lines)

    def startPage(kind: PageKind, drop: Float): State = State(closed, Open(kind, drop, Vector.empty))

    def withLines(lines: Vector[SetLine], geometry: Geometry): State =
      copy(current = current.addAll(lines, geometry))

    def withoutDrop: State = copy(current = current.copy(cursor = 0f))

  def fill(groups: Vector[Group], geometry: Geometry): Vector[FilledPage] =
    groups
      .foldLeft(State(Vector.empty, Open(PageKind.Body, 0f, Vector.empty)))(place(_, _, geometry))
      .closed

  private def place(state: State, group: Group, geometry: Geometry): State =
    val opened = group.opening match
      case Opening.Continue            => state
      case Opening.NewPage(kind, drop) => state.startPage(kind, drop)
    pour(opened, group.lines, group.held, geometry)

  @tailrec private def pour(state: State, lines: Vector[SetLine], held: Int, geometry: Geometry): State =
    if lines.isEmpty then state
    else
      val fits = fitCount(state.current, lines, 0, geometry)
      if fits == lines.size then state.withLines(lines, geometry)
      else
        splitPoint(lines.size, fits, held, geometry) match
          case Some(count) => pour(breakAfter(state, lines, count, geometry), lines.drop(count), 0, geometry)
          case None if state.current.isEmpty && state.current.cursor > 0f =>
            pour(state.withoutDrop, lines, held, geometry)
          case None if state.current.isEmpty =>
            val forced = fits.max(1)
            pour(breakAfter(state, lines, forced, geometry), lines.drop(forced), 0, geometry)
          case None => pour(state.startPage(PageKind.Body, 0f), lines, held, geometry)

  private def breakAfter(state: State, lines: Vector[SetLine], count: Int, geometry: Geometry): State =
    state.withLines(lines.take(count), geometry).startPage(PageKind.Body, 0f)

  /** How many lines to keep on this page: as many as fit, so long as the held lines have `orphans` lines of text beside
    * them and `widows` lines are left for the next page.
    */
  private def splitPoint(total: Int, fits: Int, held: Int, geometry: Geometry): Option[Int] =
    val least = held + geometry.orphans.max(1)
    val most  = fits.min(total - geometry.widows.max(1))
    Option.when(least <= most)(most)

  @tailrec private def fitCount(page: Open, rest: Vector[SetLine], count: Int, geometry: Geometry): Int =
    rest match
      case line +: tail if page.fits(line, geometry) => fitCount(page.add(line, geometry), tail, count + 1, geometry)
      case _                                         => count
