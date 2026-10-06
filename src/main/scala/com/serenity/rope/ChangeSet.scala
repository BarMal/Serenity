package com.serenity.rope

/** One edit in the coordinates of the document it applies to: the characters `[from, to)` are replaced by `insert`. */
final case class Replacement(from: Int, to: Int, insert: String):
  def removedLength: Int = to - from
  def delta: Int         = insert.length - removedLength
  def isNoOp: Boolean    = from == to && insert.isEmpty

/** How a position that sits on or inside an edit moves. The modes differ only at an edit's boundary and inside it:
  *
  *   - `Cursor`: collapses to the end of the inserted text, so typing at a cursor leaves it after what was typed.
  *   - `AnchorStart`: stays put under a deletion but moves past text inserted exactly at it, so text typed at the start
  *     of a span is not absorbed into it.
  *   - `AnchorEnd`: stays before text inserted exactly at it, and moves past the end of a replaced region.
  *   - `Point`: a marker that owes writing at its position; currently identical to `AnchorEnd`. It is named separately
  *     because the two answer different questions, and the legacy remap only happens to treat them alike.
  */
enum MapMode:
  case Cursor, AnchorStart, AnchorEnd, Point

/** How [[ChangeSet.fromIntents]] resolves intents that overlap or meet at one point.
  *
  *   - `MergeDeletions`: the intents are deletions; overlapping ranges are unioned and any inserted text is ignored.
  *   - `ConcatAtPoint`: inserted text of intents that meet is concatenated, the intent given last coming first, which
  *     is the order a sequential insert at the same offset produces; overlapping ranges are unioned.
  */
enum OverlapPolicy:
  case MergeDeletions, ConcatAtPoint

/** Old lines `[oldFirst, oldLast]` became new lines `[newFirst, newLast]`; every other line is textually unchanged and
  * lines after `oldLast` moved by `newLast - oldLast`.
  */
final case class LineSpan(oldFirst: Int, oldLast: Int, newFirst: Int, newLast: Int)

/** A pure description of an edit to a document of `oldLength` characters.
  *
  * Always in canonical form: parts sorted, non-overlapping, separated by at least one unchanged character (touching
  * parts are merged), and none a no-op. Canonical form is what makes structural equality meaningful, and it is why
  * every way of building one goes through [[ChangeSet.canonical]].
  */
final case class ChangeSet private (oldLength: Int, parts: Vector[Replacement]):

  lazy val newLength: Int = oldLength + shiftBefore.lastOption.getOrElse(0)

  def isIdentity: Boolean = parts.isEmpty

  /** Cumulative length delta of the parts before each part, then the total: `parts.size + 1` entries. */
  private[rope] lazy val shiftBefore: Vector[Int] = parts.scanLeft(0)(_ + _.delta)

  /** `None` when `rope` is not the document this change was made for. Applies right to left, so each part's offsets are
    * still valid, and through `Rope.delete`/`Rope.insert`, which copy only the path to the edited leaf; rebuilding from
    * slices would rebalance the tree and lose the subtrees an edit did not touch.
    */
  def applyTo(rope: Rope): Option[Rope] =
    if rope.weight != oldLength then None
    else parts.reverseIterator.foldLeft(Option(rope))((acc, part) => acc.flatMap(applyPart(_, part)))

  private def applyPart(rope: Rope, part: Replacement): Option[Rope] =
    val removed = if part.from == part.to then Some(rope) else rope.delete(part.from, part.to)
    if part.insert.isEmpty then removed else removed.flatMap(_.insert(part.from, part.insert))

  def mapPos(position: Int, mode: MapMode): Int = ChangeSetMapping.mapPos(this, position, mode)

  /** [[mapPos]] for ascending `positions`, in one merge pass instead of a search per position. */
  def mapSorted(positions: IArray[Int], mode: MapMode): IArray[Int] = ChangeSetMapping.mapSorted(this, positions, mode)

  /** `None` unless `next` starts from the document this change produces. */
  def compose(next: ChangeSet): Option[ChangeSet] =
    Option.when(newLength == next.oldLength)(ChangeSetCompose.compose(this, next))

  /** The change that undoes this one, which needs the text it removed. `None` unless `before` is the document this
    * change was made for.
    */
  def invert(before: Rope): Option[ChangeSet] =
    Option.when(before.weight == oldLength) {
      val inverted = parts.zip(shiftBefore).map { (part, shift) =>
        val start = part.from + shift
        Replacement(start, start + part.insert.length, before.sliceString(part.from, part.to))
      }
      new ChangeSet(newLength, inverted)
    }

  def oldEnvelope: Option[(Int, Int)] =
    for
      first <- parts.headOption
      last  <- parts.lastOption
    yield (first.from, last.to)

  def newEnvelope: Option[(Int, Int)] =
    for
      first <- newRanges.headOption
      last  <- newRanges.lastOption
    yield (first._1, last._2)

  /** Where each part's inserted text sits in the new document; a pure deletion is an empty range at the cut. */
  def newRanges: Vector[(Int, Int)] =
    parts.zip(shiftBefore).map((part, shift) => (part.from + shift, part.from + shift + part.insert.length))

  /** The lines this change touched, merged where parts share a line. `before` must be the document it applies to. */
  def lineSpans(before: Rope): Vector[LineSpan] =
    parts
      .foldLeft((Vector.empty[LineSpan], 0)) {
        case ((spans, shift), part) =>
          val first     = before.offsetToLineColumn(part.from)._1
          val last      = before.offsetToLineColumn(part.to)._1
          val lineDelta = part.insert.count(_ == '\n') - (last - first)
          val after     = shift + lineDelta
          val next = spans.lastOption match
            case Some(previous) if first <= previous.oldLast =>
              spans.dropRight(1) :+ previous.copy(oldLast = last, newLast = last + after)
            case _ => spans :+ LineSpan(first, last, first + shift, last + after)
          (next, after)
      }
      ._1

object ChangeSet:

  def identity(length: Int): ChangeSet = new ChangeSet(math.max(0, length), Vector.empty)

  def single(oldLength: Int, from: Int, to: Int, insert: String): ChangeSet =
    canonical(oldLength, List(Replacement(from, to, insert)))

  /** One change from edits that were each made against the same document, in the order the caller made them. Offsets
    * outside the document are clamped to it.
    */
  def fromIntents(length: Int, intents: Seq[Replacement], policy: OverlapPolicy): ChangeSet =
    policy match
      case OverlapPolicy.MergeDeletions => canonical(length, intents.map(_.copy(insert = "")))
      case OverlapPolicy.ConcatAtPoint  => canonical(length, intents.reverse)

  /** The change from `before` to `after` as the one region [[RopeDiff]] reports, which may be wider than the exact
    * difference but never narrower.
    */
  def fromDiff(before: Rope, after: Rope)(using Balance): ChangeSet =
    RopeDiff.changedOffsetRange(before, after) match
      case None => identity(before.weight)
      case Some((start, end)) =>
        val oldEnd = before.weight - (after.weight - end)
        canonical(before.weight, List(Replacement(start, oldEnd, after.sliceString(start, end))))

  /** Sorts by position, keeping the given order between parts that start and end together, then unions parts that
    * overlap or touch, concatenating their text in that order.
    */
  private[rope] def canonical(oldLength: Int, replacements: Seq[Replacement]): ChangeSet =
    val length = math.max(0, oldLength)
    val merged = replacements
      .map(clampedTo(length))
      .sortBy(part => (part.from, part.to))
      .foldLeft(Vector.empty[Replacement]) { (acc, next) =>
        acc.lastOption match
          case Some(last) if next.from <= last.to =>
            acc.dropRight(1) :+ Replacement(last.from, math.max(last.to, next.to), last.insert + next.insert)
          case _ => acc :+ next
      }
    new ChangeSet(length, merged.filterNot(_.isNoOp))

  private def clampedTo(length: Int)(part: Replacement): Replacement =
    val from = math.max(0, math.min(part.from, length))
    Replacement(from, math.max(from, math.min(part.to, length)), part.insert)
