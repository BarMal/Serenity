package com.serenity.state.models

import scala.collection.Searching.{Found, InsertionPoint}

import com.serenity.rope.Rope

final case class FindResult(line: Int, column: Int)

object FindResult:
  given Ordering[FindResult] = Ordering.by(result => (result.line, result.column))

  def at(position: CursorPosition): FindResult = FindResult(position.line, position.column)

/** How a find query matches text. Case-insensitive literal matching is the default, as in VS Code. */
final case class FindOptions(matchCase: Boolean = false, wholeWord: Boolean = false, regex: Boolean = false):

  def toggled(option: FindOption): FindOptions =
    option match
      case FindOption.MatchCase => copy(matchCase = !matchCase)
      case FindOption.WholeWord => copy(wholeWord = !wholeWord)
      case FindOption.Regex     => copy(regex = !regex)

  def isOn(option: FindOption): Boolean =
    option match
      case FindOption.MatchCase => matchCase
      case FindOption.WholeWord => wholeWord
      case FindOption.Regex     => regex

object FindOptions:
  val default: FindOptions = FindOptions()

enum FindOption:
  case MatchCase, WholeWord, Regex

/** What landing results do: `Seed` selects the first match at or after the caret and moves the caret there (a new
  * query); `Refresh` re-syncs results after the document changed under an open find, leaving the caret alone.
  */
enum FindSearchPurpose:
  case Seed, Refresh

/** Immutable identity for a background find operation. `anchor` is the offset the search starts from and wraps around,
  * so a capped result window always holds the matches nearest the caret.
  */
final case class FindSearchRequest(
    surfaceId: SurfaceId,
    bufferId: BufferId,
    query: String,
    content: Rope,
    options: FindOptions = FindOptions.default,
    anchor: Int = 0,
    purpose: FindSearchPurpose = FindSearchPurpose.Seed
)

/** At most [[FindSearch.MatchLimit]] matches in document order; `capped` when the document holds more. */
final case class FindMatches(results: Vector[FindResult], capped: Boolean)

object FindMatches:
  val empty: FindMatches = FindMatches(Vector.empty, capped = false)

final case class FindResultSet private (
    query: String,
    results: Vector[FindResult],
    currentIndex: Int,
    capped: Boolean
):
  def selectedResult: Option[FindResult] =
    results.lift(currentIndex)

  def move(delta: Int): FindResultSet =
    FindResultSet.normalized(query, results, currentIndex + delta, capped)

  def selectionSummary: String =
    selectedResult match
      case Some(result) =>
        s"$matchCountLabel, ${currentIndex + 1}/$countText at ${result.line + 1}:${result.column + 1}"
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

  def matchCountLabel: String =
    if capped then s"$countText matches"
    else
      results.length match
        case 1     => "1 match"
        case count => s"$count matches"

  private def countText: String =
    if capped then s"${results.length}+" else results.length.toString

object FindResultSet:
  val empty: FindResultSet = FindResultSet("", Vector.empty, 0, capped = false)

  /** Index of the first result at or after `caret`, or 0 when every result lies before it. `results` must be in
    * document order, as `FindSearch` produces them.
    */
  def indexAtOrAfter(results: Vector[FindResult], caret: CursorPosition): Int =
    wrapIndex(results.search(FindResult.at(caret)).insertionPoint, results.length)

  /** Index of the first result strictly after `caret`, or 0 when none follows it. */
  def indexAfter(results: Vector[FindResult], caret: CursorPosition): Int =
    val next = results.search(FindResult.at(caret)) match
      case Found(index)          => index + 1
      case InsertionPoint(index) => index
    wrapIndex(next, results.length)

  def normalized(
    query: String,
    results: Vector[FindResult],
    requestedIndex: Int,
    capped: Boolean = false
  ): FindResultSet =
    if query.isEmpty then empty
    else FindResultSet(query, results, wrapIndex(requestedIndex, results.length), capped && results.nonEmpty)

  private def wrapIndex(index: Int, resultCount: Int): Int =
    if resultCount <= 0 then 0
    else
      val raw = index % resultCount
      if raw < 0 then raw + resultCount else raw

final case class FindState(
    query: String,
    results: Vector[FindResult],
    currentIndex: Int,
    options: FindOptions = FindOptions.default,
    capped: Boolean = false
):
  def resultSet: FindResultSet =
    FindResultSet.normalized(query, results, currentIndex, capped)

object FindState:
  def fromResultSet(resultSet: FindResultSet, options: FindOptions): FindState =
    FindState(resultSet.query, resultSet.results, resultSet.currentIndex, options, resultSet.capped)
