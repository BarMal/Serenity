package com.serenity.rope

import scala.annotation.tailrec

import com.serenity.rope.btree.{BTreeBuild, BTreeEdit, BTreeJoin, BTreeSplit}
import com.serenity.text.TextSummary

sealed trait Rope(using balance: Balance):
  def weight: Int

  /** Levels of the tree: 1 for a single leaf, and one more than a child's for a branch, since every leaf is at one
    * depth.
    */
  def height: Int
  def newlineCount: Int
  def lastLineLength: Int
  def endsWithNewline: Boolean

  /** Words and grapheme-cluster characters as `TextCounts` defines them, kept incrementally: a `Branch` joins its
    * children's summaries, recounting only a bounded window around each seam (see `TextSummary`).
    */
  def textSummary: TextSummary

  def wordCount: Int = textSummary.counts.words

  def characterCount: Int = textSummary.counts.characters

  def nonWhitespaceCount: Int = textSummary.counts.nonWhitespaceCharacters

  def concat(that: Rope): Rope = BTreeJoin.concat(this, that)

  def index(i: Int): Option[Char]

  def splitAt(index: Int): Option[(Rope, Rope)]

  /** What [[splitAt]] gives before `index`, without building the half after it; `index` must be within `[0, weight]`.
    */
  private[rope] def prefixTo(index: Int): Rope = splitAt(index).fold(this)((before, _) => before)

  /** What [[splitAt]] gives from `index`, without building the half before it; `index` must be within `[0, weight]`. */
  private[rope] def suffixFrom(index: Int): Rope = splitAt(index).fold(this)((_, after) => after)

  /** `None` when `index` is outside `[0, weight]` -- the range in which every character position and the
    * one-past-the-end append position are both meaningful. Callers decide the out-of-range policy explicitly; this
    * method never silently no-ops. CR and CRLF in `str` become LF, as in [[Rope.apply]].
    */
  def insert(index: Int, str: String): Option[Rope] =
    Option.when(index >= 0 && index <= weight)(BTreeEdit.insert(this, index, LineEndings.normalized(str)))

  /** `None` when the underlying `deleteRight` call fails -- see its doc for the exact range. */
  def delete(startIndex: Int, endIndex: Int): Option[Rope] =
    deleteRight(startIndex, endIndex - startIndex)

  /** Total: unlike `deleteRight`, a `start` beyond the rope is a legitimate (if unusual) position to delete backwards
    * from -- it is clamped to `weight` rather than rejected, since callers such as `dropRight` pass `weight` itself and
    * callers tracking a cursor may briefly overshoot during a multi-step edit.
    */
  def deleteLeft(start: Int, count: Int): Rope =
    if count == 0 then this
    else if count < 0 then deleteRight(start, Math.abs(count)).getOrElse(this) // see deleteRight's own doc
    else if start <= 0 then this
    else
      val clampedStart = Math.min(start, weight)
      val actualCount  = Math.min(count, clampedStart)
      BTreeEdit.deleteRange(this, clampedStart - actualCount, clampedStart)

  /** `None` when `start` is outside `[0, weight]`. A `count` that overruns the end of the rope is clamped rather than
    * rejected -- "delete to the end" is a legitimate, well-defined request, unlike a genuinely invalid start. A
    * negative `count` redirects to `deleteLeft`, which is total.
    */
  def deleteRight(start: Int, count: Int): Option[Rope] =
    if count == 0 then Some(this)
    else if count < 0 then Some(deleteLeft(start, Math.abs(count)))
    else if start < 0 || start > weight then None
    else Some(BTreeEdit.deleteRange(this, start, start + Math.min(count, weight - start)))

  def dropLeft(n: Int): Rope =
    // start = 0 is always in [0, weight], so this can only fail if deleteRight violates its own contract; treated as
    // unreachable rather than threaded through dropLeft's own signature.
    deleteRight(0, n).getOrElse(this)

  def dropRight(n: Int): Rope = deleteLeft(weight, n)

  /** `None` when `index` is outside `[0, weight)` -- unlike `insert`, there is no character to replace at
    * `index == weight`.
    */
  def replace(index: Int, char: Char): Option[Rope] =
    Option.when(index >= 0 && index < weight)(
      BTreeEdit.insert(BTreeEdit.deleteRange(this, index, index + 1), index, char.toString)
    )

  def replaceAll(term: String, replacement: String): Rope =
    searchAll(term).sorted.reverse.foldLeft(this)((rope, index) =>
      rope
        .delete(index, index + term.length)
        .flatMap(_.insert(index, replacement))
        // `index` comes from `searchAll` on `rope` itself and matches are applied highest-offset-first, so every
        // index stays valid for the rope it is applied to; failure here would mean that invariant broke, and
        // skipping the one replacement is safer than corrupting the whole rope.
        .getOrElse(rope)
    )

  def collect(): String =
    val text = new StringBuilder(weight)
    Rope.appendLeaves(this, text)
    text.toString

  /** Visit the leaf content intersecting a half-open rope range from left to right. */
  final private[rope] def chunksInRange(startIndex: Int, endIndex: Int): Iterator[(Int, String)] =
    val start = math.max(0, math.min(startIndex, weight))
    val end   = math.max(start, math.min(endIndex, weight))

    if start == end then Iterator.empty
    else Iterator.unfold(List(this -> 0))(stack => Rope.nextChunk(stack, start, end))

  /** The whole leaf covering `index`, with its own absolute offset -- unlike `chunksInRange`, this never clips to a
    * caller-supplied window, so `RopeCharacterSource` can cache the entire leaf regardless of which direction the next
    * access moves. `None` only for an `index` outside `[0, weight)`.
    */
  private[rope] def leafAt(index: Int): Option[(Int, String)] =
    if index < 0 || index >= weight then None else Some(Rope.descendToLeaf(this, index, 0))

  def contentEquals(text: String): Boolean = RopeContent.equalsText(this, text)

  /** Two ropes are equal when they hold the same text, however their trees are shaped; `hashCode` agrees. */
  override def equals(obj: Any): Boolean =
    obj match
      case that: Rope => RopeContent.equal(this, that)
      case _          => false

  override def hashCode: Int = RopeContent.hash(this)

  override def toString: String = collect()

  def slice(startIndex: Int, endIndex: Int): Rope =
    dropRight(weight - endIndex).dropLeft(startIndex)

  def sliceString(startIndex: Int, endIndex: Int): String =
    val start = math.max(0, math.min(startIndex, weight))
    val end   = math.max(start, math.min(endIndex, weight))
    if start == end then ""
    else
      val value = new StringBuilder(end - start)
      chunksInRange(start, end).foreach { case (_, chunk) => value.append(chunk) }
      value.toString

  def searchAll(term: String): List[Int] =
    if term.isEmpty || term.length > weight then List.empty
    else
      val prefix = searchPrefixTable(term)
      chunksInRange(0, weight)
        .foldLeft((List.empty[Int], 0)) {
          case ((found, matched), (chunkOffset, chunk)) =>
            searchChunk(chunk, chunkOffset, term, prefix, matched, found)
        }
        ._1
        .reverse

  def lineCount: Int =
    newlineCount + 1

  /** #1063: previously walked `Leaf`/`Node` directly via `appendLine`, duplicating the line-boundary scan
    * `linesIteratorFrom` already does over flattened chunks. Delegating keeps the same cost shape -- one
    * `lineColumnToOffset` descent plus a scan bounded by the line's own length -- without a second implementation of
    * "find line N" to keep in sync.
    */
  def getLine(lineIndex: Int): Option[String] =
    linesIteratorFrom(lineIndex).nextOption().map((_, line) => line)

  def linesFrom(lineIndex: Int, maxLines: Int): Vector[String] =
    linesIteratorFrom(lineIndex).take(maxLines).map(_._2).toVector

  def linesIteratorFrom(lineIndex: Int): Iterator[(Int, String)] =
    if lineIndex < 0 || lineIndex > newlineCount then Iterator.empty
    else
      val initial = LineTraversal(chunksInRange(lineColumnToOffset(lineIndex, 0), weight), "", 0, lineIndex, Nil, false)
      Iterator.unfold(initial)(nextLine)

  /** Resolve a logical line and internal UTF-16 column to the corresponding rope offset. Columns are not grapheme
    * counts; they share the same UTF-16 code-unit contract as Java `String` indexes and `CursorPosition.column`.
    */
  def lineColumnToOffset(line: Int, column: Int): Int =
    val targetLine = math.max(0, line)
    val targetCol  = math.max(0, column)

    if targetLine > newlineCount then weight
    else RopeLineConversion.lineColumnToOffset(this, targetLine, targetCol)

  /** Resolve an internal rope offset to a logical line and UTF-16 column pair. The returned column may point inside a
    * multi-code-unit grapheme; user-facing edits should snap through grapheme helpers before mutating text.
    */
  def offsetToLineColumn(offset: Int): (Int, Int) =
    val clamped = math.max(0, math.min(offset, weight))
    RopeLineConversion.offsetToLineColumn(this, clamped)

  /** [[offsetToLineColumn]] for every offset of an ascending list, in one pass that skips the subtrees holding none of
    * them and scans each leaf once, so a sorted batch (find matches, annotations) costs one descent in total.
    */
  def offsetsToLineColumns(ascendingOffsets: IndexedSeq[Int]): Vector[(Int, Int)] =
    RopeLineConversion.offsetsToLineColumns(this, ascendingOffsets.map(offset => math.max(0, math.min(offset, weight))))

  final private case class LineTraversal(
      chunks: Iterator[(Int, String)],
      chunk: String,
      chunkIndex: Int,
      line: Int,
      fragments: List[String],
      finished: Boolean
  )

  @tailrec
  private def nextLine(state: LineTraversal): Option[((Int, String), LineTraversal)] =
    if state.finished then None
    else if state.chunkIndex < state.chunk.length then
      val newlineIndex = state.chunk.indexOf('\n', state.chunkIndex)
      if newlineIndex >= 0 then
        val value = joined(state.chunk.substring(state.chunkIndex, newlineIndex) :: state.fragments)
        Some(
          (state.line -> value) ->
            state.copy(chunkIndex = newlineIndex + 1, line = state.line + 1, fragments = Nil)
        )
      else
        nextLine(
          state.copy(
            chunkIndex = state.chunk.length,
            fragments = state.chunk.substring(state.chunkIndex) :: state.fragments
          )
        )
    else if state.chunks.hasNext then nextLine(state.copy(chunk = state.chunks.next()._2, chunkIndex = 0))
    else Some((state.line -> joined(state.fragments)) -> state.copy(finished = true))

  /** The pieces of a line, last first. A line inside one chunk is a single piece and is returned as it is. */
  private def joined(piecesLastFirst: List[String]): String =
    piecesLastFirst match
      case Nil         => ""
      case only :: Nil => only
      case _           => piecesLastFirst.reverse.mkString

  private def searchChunk(
    chunk: String,
    chunkOffset: Int,
    term: String,
    prefix: Vector[Int],
    matched: Int,
    found: List[Int]
  ): (List[Int], Int) =
    @tailrec
    def scan(index: Int, currentMatch: Int, currentFound: List[Int]): (List[Int], Int) =
      if index >= chunk.length then (currentFound, currentMatch)
      else
        val char          = chunk.charAt(index)
        val matchedPrefix = fallbackMatch(char, term, prefix, currentMatch)
        val nextMatch     = if char == term.charAt(matchedPrefix) then matchedPrefix + 1 else 0
        if nextMatch == term.length then scan(index + 1, 0, chunkOffset + index - term.length + 1 :: currentFound)
        else scan(index + 1, nextMatch, currentFound)

    scan(0, matched, found)

  @tailrec
  private def fallbackMatch(char: Char, term: String, prefix: Vector[Int], matched: Int): Int =
    if matched == 0 || char == term.charAt(matched) then matched
    else fallbackMatch(char, term, prefix, prefix(matched - 1))

  private def searchPrefixTable(term: String): Vector[Int] =
    @tailrec
    def build(index: Int, matched: Int, prefix: Vector[Int]): Vector[Int] =
      if index >= term.length then prefix
      else if term.charAt(index) == term.charAt(matched) then build(index + 1, matched + 1, prefix :+ (matched + 1))
      else if matched > 0 then build(index, prefix(matched - 1), prefix)
      else build(index + 1, 0, prefix :+ 0)

    build(index = 1, matched = 0, prefix = Vector(0))

/** Deliberately not `final`, unlike every other case class in the codebase.
  *
  * Specs subclass this to override `collect()` and `index()` with assertion-throwing versions, proving that search and
  * sequential traversal never materialise the whole rope or walk it character by character. That is the
  * representation-invariance coverage `docs/coding-standards.md` requires, and it needs a real subclass -- a stub
  * cannot observe which methods the rope chose to call.
  *
  * Kept in this file (rather than its own `Leaf.scala`) because `sealed` in Scala requires every direct subtype to be
  * defined in the same source file as the sealed type itself.
  */
@SuppressWarnings(Array("org.wartremover.warts.FinalCaseClass"))
case class Leaf(value: String)(using balance: Balance) extends Rope:
  private val lines: Long = Leaf.scanLines(value)

  override def weight: Int                   = value.length
  override def height: Int                   = 1
  override val newlineCount: Int             = (lines >>> 32).toInt
  override val lastLineLength: Int           = value.length - lines.toInt
  override val endsWithNewline: Boolean      = value.endsWith("\n")
  override lazy val textSummary: TextSummary = TextSummary.of(value)

  override def splitAt(index: Int): Option[(Rope, Rope)] =
    if index < 0 || index > value.length then None
    else Some(Leaf(value.take(index)), Leaf(value.drop(index)))

  override def index(i: Int): Option[Char] =
    Option.when(i >= 0 && i < value.length)(value.charAt(i))

  override def deleteLeft(start: Int, count: Int): Rope =
    val (pre, post) = value.splitAt(start)
    Leaf(pre.dropRight(count) + post)

  override def deleteRight(start: Int, count: Int): Option[Rope] =
    if count == 0 then Some(this)
    else if count < 0 then Some(deleteLeft(start, Math.abs(count)))
    else if start < 0 || start > value.length then None
    else
      val (pre, post) = value.splitAt(start)
      Some(Leaf(pre + post.drop(count)))

  override def collect(): String = value

object Leaf:

  /** The newline count in the high half and the offset just past the last newline (0 when there is none) in the low
    * half, from one pass that jumps from newline to newline.
    */
  private def scanLines(value: String): Long =
    @tailrec
    def go(from: Int, count: Int, pastLast: Int): Long =
      val newline = value.indexOf('\n', from)
      if newline < 0 then (count.toLong << 32) | pastLast.toLong
      else go(newline + 1, count + 1, newline + 1)

    go(0, 0, 0)

/** An internal node of the B-tree: between `Fanout.Min` and `Fanout.Max` children (two or more for the root), all of
  * one height. It caches where each child starts and how many newlines precede it, so a descent by offset or by line
  * picks the child without opening the others.
  *
  * Never mutated after construction: an edit builds a new branch for each node on its path and shares the rest.
  */
final class Branch private (val children: Vector[Rope])(using Balance) extends Rope:
  private val measured: (Array[Int], Array[Int]) = Branch.measure(children)

  /** `starts(i)` is the offset of child `i` within this branch, and `starts(children.size)` its weight. */
  private[rope] val starts: Array[Int] = measured._1

  /** `newlines(i)` is how many newlines precede child `i`, and `newlines(children.size)` is the branch's total. */
  private[rope] val newlines: Array[Int] = measured._2

  override val weight: Int              = starts(children.size)
  override val height: Int              = children(0).height + 1
  override val newlineCount: Int        = newlines(children.size)
  override val lastLineLength: Int      = Branch.trailingLineLength(children)
  override val endsWithNewline: Boolean = Branch.endsWithNewlineOf(children)

  override lazy val textSummary: TextSummary =
    children.drop(1).foldLeft(children(0).textSummary)((joined, child) => TextSummary.join(joined, child.textSummary))

  /** The first child that has a character at `offset`; the last child for an offset at or past the end. */
  private[rope] def childContaining(offset: Int): Int =
    @tailrec
    def go(child: Int): Int =
      if child >= children.size - 1 || offset < starts(child + 1) then child else go(child + 1)

    go(0)

  /** The first child that reaches `offset`, so that an offset on a boundary belongs to the child that ends there. */
  private[rope] def childEndingAtOrAfter(offset: Int): Int =
    @tailrec
    def go(child: Int): Int =
      if child >= children.size - 1 || offset <= starts(child + 1) then child else go(child + 1)

    go(0)

  /** The children that overlap `[from, until)` (offsets within this branch) with their absolute offsets. */
  private[rope] def overlapping(from: Int, until: Int, base: Int): List[(Rope, Int)] =
    (childContaining(math.max(0, from)) until children.size).iterator
      .takeWhile(child => starts(child) < until)
      .map(child => (children(child), base + starts(child)))
      .toList

  /** The first child whose newlines include the `nth` (counting from 1) newline. */
  private[rope] def childHoldingNewline(nth: Int): Int =
    @tailrec
    def go(child: Int): Int =
      if child >= children.size - 1 || nth <= newlines(child + 1) then child else go(child + 1)

    go(0)

  override def index(i: Int): Option[Char] =
    if i < 0 || i >= weight then None
    else
      val child = childContaining(i)
      children(child).index(i - starts(child))

  override def splitAt(index: Int): Option[(Rope, Rope)] =
    if index < 0 || index > weight then None
    else if index == 0 then Some(Leaf(""), this)
    else if index == weight then Some(this, Leaf(""))
    else Some(BTreeSplit.split(this, index))

  override private[rope] def prefixTo(index: Int): Rope =
    if index <= 0 then Leaf("") else if index >= weight then this else BTreeSplit.prefix(this, index)

  override private[rope] def suffixFrom(index: Int): Rope =
    if index <= 0 then this else if index >= weight then Leaf("") else BTreeSplit.suffix(this, index)

object Branch:

  private[rope] def apply(children: Vector[Rope])(using Balance): Branch = new Branch(children)

  private def measure(children: Vector[Rope]): (Array[Int], Array[Int]) =
    val starts   = new Array[Int](children.size + 1)
    val newlines = new Array[Int](children.size + 1)

    @tailrec
    def fill(child: Int): Unit =
      if child < children.size then
        starts(child + 1) = starts(child) + children(child).weight
        newlines(child + 1) = newlines(child) + children(child).newlineCount
        fill(child + 1)

    fill(0)
    (starts, newlines)

  /** The characters after the last newline: a child without newlines extends the line begun in the one before it. */
  private def trailingLineLength(children: Vector[Rope]): Int =
    @tailrec
    def go(child: Int, length: Int): Int =
      if child < 0 then length
      else if children(child).newlineCount > 0 then length + children(child).lastLineLength
      else go(child - 1, length + children(child).lastLineLength)

    go(children.size - 1, 0)

  private def endsWithNewlineOf(children: Vector[Rope]): Boolean =
    @tailrec
    def go(child: Int): Boolean =
      if child < 0 then false
      else if children(child).weight > 0 then children(child).endsWithNewline
      else go(child - 1)

    go(children.size - 1)

object Rope:

  def empty(using balance: Balance): Rope = Leaf("")

  // Normalizes CRLF and bare CR to LF on entry, in one pass, so all downstream code
  // (WrapEngine, RenderEngine, cursor arithmetic) only ever sees '\n'.
  def apply(in: String)(using balance: Balance): Rope =
    BTreeBuild.fromText(LineEndings.normalized(in))

  /** A rope over leaf strings that are already in hand: each is kept as it is unless it exceeds the largest leaf, and
    * empty ones are dropped.
    */
  private[rope] def fromLeafValues(values: Vector[String])(using balance: Balance): Rope =
    BTreeBuild.fromLeafValues(values)

  private def appendLeaves(rope: Rope, text: StringBuilder): Unit =
    rope match
      case Leaf(value)    => val _ = text.append(value)
      case branch: Branch => branch.children.foreach(child => appendLeaves(child, text))

  @tailrec
  private def descendToLeaf(rope: Rope, index: Int, baseOffset: Int): (Int, String) =
    rope match
      case Leaf(value) => (baseOffset, value)
      case branch: Branch =>
        val child = branch.childContaining(index)
        descendToLeaf(branch.children(child), index - branch.starts(child), baseOffset + branch.starts(child))

  @tailrec
  private def nextChunk(
    remaining: List[(Rope, Int)],
    start: Int,
    end: Int
  ): Option[((Int, String), List[(Rope, Int)])] =
    remaining match
      case Nil => None
      case (rope, baseOffset) :: tail if baseOffset >= end || baseOffset + rope.weight <= start =>
        nextChunk(tail, start, end)
      case (branch: Branch, baseOffset) :: tail =>
        nextChunk(branch.overlapping(start - baseOffset, end - baseOffset, baseOffset) ::: tail, start, end)
      case (Leaf(value), baseOffset) :: tail =>
        val chunkStart = math.max(0, start - baseOffset)
        val chunkEnd   = math.min(value.length, end - baseOffset)
        Some((baseOffset + chunkStart, value.substring(chunkStart, chunkEnd)) -> tail)
