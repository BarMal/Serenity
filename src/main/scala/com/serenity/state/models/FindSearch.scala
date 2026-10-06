package com.serenity.state.models

import java.util.regex.{Matcher, Pattern, PatternSyntaxException}

import scala.util.Try

import com.serenity.rope.{Rope, RopeCharSequence, RopeCharacterSource}
import com.serenity.text.TextEditing

/** Why a query cannot be searched for: a regular expression that does not compile. */
final case class FindQueryError(message: String)

/** A query compiled with its options, built per search and never kept in state. */
final class FindPattern private (pattern: Pattern, val options: FindOptions):
  def matcher(text: CharSequence): Matcher = pattern.matcher(text)

object FindPattern:

  def compile(query: String, options: FindOptions): Either[FindQueryError, FindPattern] =
    val caseFlags = if options.matchCase then 0 else Pattern.CASE_INSENSITIVE | Pattern.UNICODE_CASE
    val modeFlags = if options.regex then Pattern.MULTILINE else Pattern.LITERAL
    Try(Pattern.compile(query, caseFlags | modeFlags)).toEither.left
      .map {
        case syntax: PatternSyntaxException => FindQueryError(syntax.getDescription)
        case other                          => FindQueryError(String.valueOf(other.getMessage))
      }
      .map(new FindPattern(_, options))

/** A match as UTF-16 offsets into the document, `start < end`. */
final case class FindSpan(start: Int, end: Int)

/** One document scanned with one compiled query. Every match is non-empty, covers whole grapheme clusters and, under
  * whole-word, starts and ends on UAX #29 word boundaries; matches that fail those checks are skipped, not truncated.
  */
final class FindScan(val content: Rope, pattern: FindPattern):
  private val source = RopeCharacterSource(content)
  private val text   = RopeCharSequence(source)

  /** Accepted matches starting at or after `from`, in document order and non-overlapping. */
  def spansFrom(from: Int): Iterator[FindSpan] =
    val matcher = pattern.matcher(text)
    Iterator
      .unfold(math.max(0, from)) { position =>
        Option.when(position <= text.length && matcher.find(position)) {
          val span                       = FindSpan(matcher.start, matcher.end)
          val accepted: Option[FindSpan] = Option.when(accepts(span))(span)
          (accepted, if accepted.isDefined then span.end else span.start + 1)
        }
      }
      .flatten

  /** The accepted match starting exactly at `offset`, if any. */
  def spanAt(offset: Int): Option[FindSpan] =
    Option
      .when(offset >= 0 && offset <= text.length) {
        val matcher =
          pattern.matcher(text).useTransparentBounds(true).useAnchoringBounds(false).region(offset, text.length)
        Option.when(matcher.lookingAt())(FindSpan(matcher.start, matcher.end)).filter(accepts)
      }
      .flatten

  def resultFor(span: FindSpan): FindResult =
    val (line, column) = content.offsetToLineColumn(span.start)
    FindResult(line, column)

  def offsetOf(result: FindResult): Int =
    content.lineColumnToOffset(result.line, result.column)

  /** Whether `result` still marks an accepted match: an edit may have moved other text under it, or left it at a column
    * its line no longer has (which `lineColumnToOffset` would silently clamp).
    */
  def stillMatches(result: FindResult): Boolean =
    val offset = offsetOf(result)
    content.offsetToLineColumn(offset) == (result.line, result.column) && spanAt(offset).isDefined

  private def accepts(span: FindSpan): Boolean =
    span.start < span.end &&
      TextEditing.isWholeGraphemeRange(source, span.start, span.end) &&
      (!pattern.options.wholeWord || TextEditing.isWholeWordRange(source, span.start, span.end))

object FindSearch:

  /** The most matches a search keeps; a document with more reports the count as "1000+". */
  val MatchLimit: Int = 1000

  private val InitialBackwardWindow = 4096

  /** Matches of a literal, case-insensitive `query`, from the top of the document. */
  def results(content: Rope, query: String): Vector[FindResult] =
    search(content, query, FindOptions.default, anchor = 0).results

  /** Whether `result` still marks a match of `query` in `content`; see [[FindScan.stillMatches]]. */
  def stillMatches(
    content: Rope,
    query: String,
    result: FindResult,
    options: FindOptions = FindOptions.default
  ): Boolean =
    query.nonEmpty && FindPattern.compile(query, options).exists(FindScan(content, _).stillMatches(result))

  /** Up to `limit` matches nearest `anchor`, scanning forward from it and wrapping to the top, returned in document
    * order. An invalid query (see [[FindPattern.compile]]) finds nothing.
    */
  def search(
    content: Rope,
    query: String,
    options: FindOptions,
    anchor: Int,
    limit: Int = MatchLimit
  ): FindMatches =
    if query.isEmpty then FindMatches.empty
    else
      FindPattern
        .compile(query, options)
        .fold(_ => FindMatches.empty, pattern => window(FindScan(content, pattern), anchor, limit))

  def window(scan: FindScan, anchor: Int, limit: Int = MatchLimit): FindMatches =
    val from              = math.max(0, math.min(anchor, scan.content.weight))
    val forward           = scan.spansFrom(from).take(limit + 1).toVector
    val firstForwardStart = forward.headOption.fold(Int.MaxValue)(_.start)
    val wrapped =
      if forward.length > limit || from == 0 then Vector.empty
      else
        scan
          .spansFrom(0)
          .takeWhile(span => span.start < from && span.end <= firstForwardStart)
          .take(limit + 1 - forward.length)
          .toVector
    val all = wrapped ++ forward
    FindMatches(all.take(limit).map(scan.resultFor).sorted, capped = all.length > limit)

  /** The next match after `offset`, wrapping to the first in the document. `skipMatchAt` steps past a match starting
    * exactly at `offset` -- the current one -- instead of returning it.
    */
  def nextAfter(scan: FindScan, offset: Int, skipMatchAt: Boolean): Option[FindSpan] =
    val from = if skipMatchAt then scan.spanAt(offset).fold(offset)(_.end) else offset
    scan.spansFrom(from).nextOption().orElse(scan.spansFrom(0).nextOption())

  /** The last match starting before `offset`, wrapping to the last in the document. */
  def previousBefore(scan: FindScan, offset: Int, literal: Boolean): Option[FindSpan] =
    lastStartingBefore(scan, offset, literal).orElse(lastStartingBefore(scan, scan.content.weight + 1, literal))

  /** A literal match cannot begin partway through another, so a literal scan can start from a window just before
    * `limit` and widen only when that window holds no match; a regex scan starts from the top, where its matches are
    * anchored.
    */
  @annotation.tailrec
  private def lastStartingBefore(scan: FindScan, limit: Int, literal: Boolean, window: Long = 0L): Option[FindSpan] =
    val windowSize  = if window <= 0L then InitialBackwardWindow.toLong else window
    val windowStart = if literal then math.max(0L, limit - windowSize).toInt else 0
    val found =
      scan.spansFrom(windowStart).takeWhile(_.start < limit).foldLeft(Option.empty[FindSpan])((_, s) => Some(s))
    if found.nonEmpty || windowStart == 0 then found
    else lastStartingBefore(scan, limit, literal, windowSize * 2)

enum FindDirection:
  case Forward, Backward

/** Moving between matches from the caret. Every step searches the current text, so a step never lands on an offset an
  * edit has invalidated; stored results only supply the index, and are re-found around the target when they no longer
  * hold it or no longer match.
  */
object FindNavigation:

  def step(content: Rope, state: FindState, caret: CursorPosition, direction: FindDirection): Option[FindState] =
    FindPattern.compile(state.query, state.options).toOption.filter(_ => state.query.nonEmpty).flatMap { pattern =>
      val scan        = FindScan(content, pattern)
      val caretOffset = content.lineColumnToOffset(caret.line, caret.column)
      val target = direction match
        case FindDirection.Forward =>
          FindSearch.nextAfter(
            scan,
            caretOffset,
            skipMatchAt = state.resultSet.selectedResult.contains(FindResult.at(caret))
          )
        case FindDirection.Backward =>
          FindSearch.previousBefore(scan, caretOffset, literal = !state.options.regex)
      target.map(span => selecting(scan, state, span))
    }

  private def selecting(scan: FindScan, state: FindState, span: FindSpan): FindState =
    val target = scan.resultFor(span)
    val index  = state.results.indexOf(target)
    if index >= 0 && state.results.forall(scan.stillMatches) then state.copy(currentIndex = index)
    else
      val window = FindSearch.window(scan, span.start)
      state.copy(
        results = window.results,
        currentIndex = math.max(0, window.results.indexOf(target)),
        capped = window.capped
      )

/** One match painted in the text: `current` is the selected one. */
final case class FindHighlight(start: CursorPosition, end: CursorPosition, current: Boolean)

object FindHighlights:

  /** The find state whose matches are painted for `bufferId`: the active buffer's, while a find surface is open. */
  def paintedFindState(state: AppState, bufferId: BufferId): Option[FindState] =
    Option
      .when(findSurfaceOpen(state) && activeBufferId(state).contains(bufferId))(
        state.persisted.buffers.get(bufferId).flatMap(_.findState)
      )
      .flatten
      .filter(found => found.query.nonEmpty && found.results.nonEmpty)

  /** Highlights for the stored results starting on `lines`, each re-matched against `content` so a result an edit has
    * invalidated is never painted.
    */
  def onLines(content: Rope, findState: FindState, lines: Set[Int]): Map[Int, List[FindHighlight]] =
    val none = Map.empty[Int, List[FindHighlight]]
    FindPattern.compile(findState.query, findState.options).toOption.fold(none) { pattern =>
      val scan     = FindScan(content, pattern)
      val selected = findState.resultSet.selectedResult
      val highlights = findState.results.iterator
        .filter(result => lines.contains(result.line))
        .flatMap { result =>
          val offset = scan.offsetOf(result)
          scan.spanAt(offset).filter(_ => content.offsetToLineColumn(offset) == (result.line, result.column)).map {
            span =>
              val (endLine, endColumn) = content.offsetToLineColumn(span.end)
              FindHighlight(
                CursorPosition(result.line, result.column),
                CursorPosition(endLine, endColumn),
                selected.contains(result)
              )
          }
        }
      highlights
        .flatMap(highlight => (highlight.start.line to highlight.end.line).filter(lines.contains).map(_ -> highlight))
        .toList
        .groupMap(_._1)(_._2)
    }

  def findSurfaceOpen(state: AppState): Boolean =
    state.runtime.uiSurfaces.exists {
      case UiSurface(_, SurfaceContent.ModalWorkflow(_: Modal.Find), _, _) => true
      case _                                                               => false
    }

  private def activeBufferId(state: AppState): Option[BufferId] =
    state.persisted.layout.activeEditorPaneId.flatMap(paneId =>
      state.persisted.layout.editorPanes.get(paneId).flatMap(_.bufferId)
    )
