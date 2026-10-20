package com.serenity.ui.openings

import com.serenity.rope.{Balance, ChangeSet, Rope, RopeDiff}

/** An edit that replaced the `oldCount` lines from `first` with `newCount` lines. */
final case class LineEdit(first: Int, oldCount: Int, newCount: Int):
  def delta: Int = newCount - oldCount

  /** This edit enlarged to cover the new lines `from` until `until` as well. */
  def covering(from: Int, until: Int): LineEdit =
    val start = math.min(first, from)
    val end   = math.max(first + newCount, until)
    LineEdit(start, end - start - delta, end - start)

object LineEdit:

  /** The lines that may differ between `before` and `after`, from `change` when it is the edit that made `after`, and
    * otherwise from the text the two ropes share at both ends.
    */
  def between(before: Rope, after: Rope, change: Option[ChangeSet] = None)(using Balance): LineEdit =
    val known = change
      .filter(edit => edit.oldLength == before.weight && edit.newLength == after.weight)
      .flatMap(edit => edit.oldEnvelope.zip(edit.newEnvelope))
    val ranges = known.orElse(RopeDiff.changedOffsetRange(before, after).map { (start, end) =>
      val oldEnd = before.weight - (after.weight - end)
      ((start, oldEnd), (start, end))
    })
    ranges.fold(LineEdit(0, 0, 0)) {
      case ((oldFrom, oldTo), (_, newTo)) =>
        val first   = before.offsetToLineColumn(oldFrom)._1
        val oldLast = before.offsetToLineColumn(oldTo)._1
        val newLast = after.offsetToLineColumn(newTo)._1
        LineEdit(first, oldLast - first + 1, newLast - first + 1)
    }

/** What one scene break or chapter title governs: the search for its opening runs from `anchor` to `end`, the line that
  * ended it (`lineCount` when the text ran out), and `opening` is that line when it opened the paragraph.
  */
final private[openings] case class OpeningSpan(anchor: Int, end: Int, opening: Option[Opening]):

  def shifted(delta: Int): OpeningSpan =
    if delta == 0 then this
    else OpeningSpan(anchor + delta, end + delta, opening.map(found => found.copy(line = found.line + delta)))

/** The openings of one text under one set of rules, with what an edit needs to update them without a rescan. */
final class OpeningLines private[openings] (
    val rules: OpeningRules,
    val lineCount: Int,
    val scanned: Boolean,
    private[openings] val spans: Vector[OpeningSpan]
):

  lazy val openings: Vector[Opening] = spans.flatMap(_.opening)

  lazy val lines: Set[Int] = openings.iterator.map(_.line).toSet

  def at(line: Int): Option[Opening] =
    OpeningLines.search(openings, line)

  def isOpening(line: Int): Boolean = at(line).isDefined

  override def equals(other: Any): Boolean =
    other match
      case that: OpeningLines => rules == that.rules && lineCount == that.lineCount && openings == that.openings
      case _                  => false

  override def hashCode(): Int = (rules, lineCount, openings).hashCode()

  override def toString: String = s"OpeningLines($openings)"

object OpeningLines:

  /** The result for text that is not scanned, such as Markdown in source mode: no line opens. */
  def none(lineCount: Int, rules: OpeningRules): OpeningLines =
    new OpeningLines(rules, lineCount, scanned = false, Vector.empty)

  private def search(openings: Vector[Opening], line: Int): Option[Opening] =
    @annotation.tailrec
    def narrow(low: Int, high: Int): Option[Opening] =
      if low > high then None
      else
        val middle = (low + high) >>> 1
        val found  = openings(middle)
        if found.line == line then Some(found)
        else if found.line < line then narrow(middle + 1, high)
        else narrow(low, middle - 1)
    narrow(0, openings.length - 1)
