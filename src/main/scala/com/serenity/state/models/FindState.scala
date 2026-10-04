package com.serenity.state.models

import scala.collection.Searching.{Found, InsertionPoint}

import com.serenity.rope.{Rope, RopeCharacterSource}
import com.serenity.text.TextEditing

final case class FindResult(line: Int, column: Int)

object FindResult:
  given Ordering[FindResult] = Ordering.by(result => (result.line, result.column))

  def at(position: CursorPosition): FindResult = FindResult(position.line, position.column)

/** Immutable identity for a background find operation. */
final case class FindSearchRequest(
    surfaceId: SurfaceId,
    bufferId: BufferId,
    query: String,
    content: Rope
)

object FindSearch:

  /** Finds whole-grapheme occurrences in deterministic document order.
    *
    * Uses `RopeCharacterSource`'s leaf-caching adapter rather than reading through `Rope.index` directly: the per-match
    * grapheme-boundary check now scans through ICU4J's `BreakIterator` (#1277 step 3), which reads several neighbouring
    * characters per boundary rather than one, so an uncached `O(log n)` re-descent per character turned into the
    * dominant cost over a large document with many matches.
    */
  def results(content: Rope, query: String): Vector[FindResult] =
    if query.isEmpty then Vector.empty
    else
      val source = RopeCharacterSource(content)
      content
        .searchAll(query)
        .iterator
        .collect {
          case offset if TextEditing.isWholeGraphemeRange(source, offset, offset + query.length) =>
            val (line, column) = content.offsetToLineColumn(offset)
            FindResult(line, column)
        }
        .toVector

  /** Whether `result` still marks a whole-grapheme occurrence of `query` in `content`. Stored results are line/column
    * positions taken from an earlier revision, so an edit the find state did not observe can leave them pointing at
    * other text, or at a column the line no longer has (which `lineColumnToOffset` would silently clamp).
    */
  def stillMatches(content: Rope, query: String, result: FindResult): Boolean =
    val offset         = content.lineColumnToOffset(result.line, result.column)
    val end            = offset + query.length
    val (line, column) = content.offsetToLineColumn(offset)
    val samePosition   = line == result.line && column == result.column
    samePosition && content.sliceString(offset, end) == query &&
    TextEditing.isWholeGraphemeRange(RopeCharacterSource(content), offset, end)

final case class FindResultSet private (
    query: String,
    results: Vector[FindResult],
    currentIndex: Int
):
  def selectedResult: Option[FindResult] =
    results.lift(currentIndex)

  def move(delta: Int): FindResultSet =
    FindResultSet.normalized(query, results, currentIndex + delta)

  def selectionSummary: String =
    selectedResult match
      case Some(result) =>
        s"$matchCountLabel, ${currentIndex + 1}/${results.length} at ${result.line + 1}:${result.column + 1}"
      case None =>
        matchCountLabel

  def visibleResults(maxResults: Int): List[(FindResult, Int)] =
    if maxResults <= 0 || results.isEmpty then Nil
    else
      val windowSize = math.min(maxResults, results.length)
      val halfWindow = windowSize / 2
      val maxStart   = results.length - windowSize
      val start      = math.max(0, math.min(currentIndex - halfWindow, maxStart))
      results.slice(start, start + windowSize).zip(start until start + windowSize).toList

  private def matchCountLabel: String =
    results.length match
      case 1     => "1 match"
      case count => s"$count matches"

object FindResultSet:
  val empty: FindResultSet = FindResultSet("", Vector.empty, 0)

  /** Index of the first result at or after `caret`, or 0 when every result lies before it. `results` must be in
    * document order, as `FindSearch.results` produces them.
    */
  def indexAtOrAfter(results: Vector[FindResult], caret: CursorPosition): Int =
    wrapIndex(results.search(FindResult.at(caret)).insertionPoint, results.length)

  /** Index of the first result strictly after `caret`, or 0 when none follows it. */
  def indexAfter(results: Vector[FindResult], caret: CursorPosition): Int =
    val next = results.search(FindResult.at(caret)) match
      case Found(index)          => index + 1
      case InsertionPoint(index) => index
    wrapIndex(next, results.length)

  def normalized(query: String, results: Vector[FindResult], requestedIndex: Int): FindResultSet =
    if query.isEmpty then empty
    else FindResultSet(query, results, wrapIndex(requestedIndex, results.length))

  private def wrapIndex(index: Int, resultCount: Int): Int =
    if resultCount <= 0 then 0
    else
      val raw = index % resultCount
      if raw < 0 then raw + resultCount else raw

final case class FindState(
    query: String,
    results: Vector[FindResult],
    currentIndex: Int
):
  def resultSet: FindResultSet =
    FindResultSet.normalized(query, results, currentIndex)

object FindState:
  def fromResultSet(resultSet: FindResultSet): FindState =
    FindState(resultSet.query, resultSet.results, resultSet.currentIndex)
