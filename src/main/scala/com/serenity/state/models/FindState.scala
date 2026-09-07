package com.serenity.state.models

import com.serenity.rope.Rope
import com.serenity.text.TextEditing

final case class FindResult(line: Int, column: Int)

/** Immutable identity for a background find operation. */
final case class FindSearchRequest(
    surfaceId: SurfaceId,
    bufferId: BufferId,
    query: String,
    content: Rope
)

object FindSearch:

  /** Finds whole-grapheme occurrences in deterministic document order. */
  def results(content: Rope, query: String): List[FindResult] =
    if query.isEmpty then Nil
    else
      content.searchAll(query).collect {
        case offset if TextEditing.isWholeGraphemeRange(RopeCharacterSource(content), offset, offset + query.length) =>
          val (line, column) = content.offsetToLineColumn(offset)
          FindResult(line, column)
      }

  final private case class RopeCharacterSource(content: Rope) extends TextEditing.CharacterSource:
    override def length: Int = content.weight

    // TextEditing's scanners only ever call charAt within [0, length), so the fallback below is never
    // actually exercised in practice. Same idiom as the other RopeCharacterSource implementations in this
    // codebase (e.g. ModalEventReducer, StateManagerWorkflowCapability).
    override def charAt(index: Int): Char =
      content.index(index).getOrElse(' ')

final case class FindResultSet private (
    query: String,
    results: List[FindResult],
    currentIndex: Int
):
  def selectedResult: Option[FindResult] =
    if results.isEmpty then None else results.lift(currentIndex)

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
      results.zipWithIndex.slice(start, start + windowSize)

  private def matchCountLabel: String =
    results.length match
      case 1     => "1 match"
      case count => s"$count matches"

object FindResultSet:
  val empty: FindResultSet = FindResultSet("", Nil, 0)

  def normalized(query: String, results: List[FindResult], requestedIndex: Int): FindResultSet =
    if query.isEmpty then empty
    else FindResultSet(query, results, wrapIndex(requestedIndex, results.length))

  private def wrapIndex(index: Int, resultCount: Int): Int =
    if resultCount <= 0 then 0
    else
      val raw = index % resultCount
      if raw < 0 then raw + resultCount else raw

final case class FindState(
    query: String,
    results: List[FindResult],
    currentIndex: Int
):
  def resultSet: FindResultSet =
    FindResultSet.normalized(query, results, currentIndex)

object FindState:
  def fromResultSet(resultSet: FindResultSet): FindState =
    FindState(resultSet.query, resultSet.results, resultSet.currentIndex)
