package com.serenity.ui.openings

import scala.annotation.tailrec

/** What one line of text is to the search for chapter openings. */
enum OpeningLine:
  case Blank
  case ChapterTitle
  case SubHeading
  case SceneBreak

  /** An epigraph the rules say to skip. */
  case Epigraph

  /** Text that cannot be an opening and ends the search: a list, table, code, rule or one-character paragraph. */
  case Blocked
  case Body(handDropCap: Boolean)

  /** Whether a search for a chapter's opening goes on past this line. */
  def isSkippedInChapter: Boolean = this match
    case Blank | SubHeading | Epigraph => true
    case _                             => false

/** Finds the openings of a text from its lines' [[OpeningLine]]s, and updates them from an edit.
  *
  * A search starts at a chapter title (or a scene break the rules give an opening) and runs over blank lines, lower
  * headings and epigraphs to the first body paragraph, which is the opening. Anything else, including the next title,
  * ends it with no opening. A search therefore never looks back, and an edit can only change the searches that were
  * pending at it: [[rescan]] redoes the lines from the start of the pending search at the edit, to the first line past
  * the edit where no search is pending, and shifts the rest.
  */
private[openings] object OpeningScan:

  final private case class Pending(anchor: Int, kind: OpeningKind)

  /** `startsBlock` says whether a line begins a block whose classification no earlier line can affect. */
  final private case class Lines(classify: Int => OpeningLine, startsBlock: Int => Boolean)

  /** Lines past an edit that [[rescan]] always reads: classification looks one line ahead and one behind. */
  private val Margin = 2

  def scan(lineCount: Int, rules: OpeningRules, classify: Int => OpeningLine): OpeningLines =
    val (spans, _) = walk(0, Int.MaxValue, lineCount, rules, Lines(classify, _ => true), None, Vector.empty)
    new OpeningLines(rules, lineCount, scanned = true, spans)

  def rescan(
    previous: OpeningLines,
    edit: LineEdit,
    lineCount: Int,
    rules: OpeningRules,
    classify: Int => OpeningLine,
    startsBlock: Int => Boolean = _ => true
  ): OpeningLines =
    if !previous.scanned || previous.rules != rules || previous.lineCount + edit.delta != lineCount then
      scan(lineCount, rules, classify)
    else
      val start = windowStart(edit.first, classify)
      val kept  = previous.spans.take(spansBefore(previous.spans, start))
      val (fresh, end) =
        walk(
          start,
          edit.first + edit.newCount + Margin,
          lineCount,
          rules,
          Lines(classify, startsBlock),
          None,
          Vector.empty
        )
      val resumed = previous.spans.drop(spansBefore(previous.spans, end - edit.delta)).map(_.shifted(edit.delta))
      new OpeningLines(rules, lineCount, scanned = true, kept ++ fresh ++ resumed)

  /** The first line the redo must cover: the nearest line above the edit that is not part of a pending search, with the
    * line right above the edit always redone because the edit can change what that line is.
    */
  private def windowStart(first: Int, classify: Int => OpeningLine): Int =
    @tailrec
    def up(line: Int): Int =
      if line <= 0 then 0 else if classify(line).isSkippedInChapter then up(line - 1) else line
    up(first - 2)

  private def spansBefore(spans: Vector[OpeningSpan], line: Int): Int =
    @tailrec
    def narrow(low: Int, high: Int): Int =
      if low >= high then low
      else
        val middle = (low + high) >>> 1
        if spans(middle).anchor < line then narrow(middle + 1, high) else narrow(low, middle)
    narrow(0, spans.length)

  /** The spans found from `line` on, and the line the walk stopped before: the first at or past `stopFrom` with no
    * search pending, or `lineCount`.
    */
  @tailrec
  private def walk(
    line: Int,
    stopFrom: Int,
    lineCount: Int,
    rules: OpeningRules,
    lines: Lines,
    pending: Option[Pending],
    found: Vector[OpeningSpan]
  ): (Vector[OpeningSpan], Int) =
    if line >= lineCount then (found ++ pending.map(search => OpeningSpan(search.anchor, lineCount, None)), lineCount)
    else if line >= stopFrom && pending.isEmpty && lines.startsBlock(line) then (found, line)
    else
      val (next, closed) = step(line, lines.classify(line), pending, rules)
      walk(line + 1, stopFrom, lineCount, rules, lines, next, found ++ closed)

  private def step(
    line: Int,
    kind: OpeningLine,
    pending: Option[Pending],
    rules: OpeningRules
  ): (Option[Pending], Option[OpeningSpan]) =
    def closedWithout(search: Pending) = Some(OpeningSpan(search.anchor, line, None))
    pending match
      case None => (starting(line, kind, rules), None)
      case Some(search) =>
        kind match
          case OpeningLine.Blank                                                                   => (pending, None)
          case OpeningLine.SubHeading | OpeningLine.Epigraph if search.kind == OpeningKind.Chapter => (pending, None)
          case OpeningLine.Body(handDropCap) =>
            (None, Some(OpeningSpan(search.anchor, line, Some(Opening(line, search.kind, handDropCap)))))
          case _ => (starting(line, kind, rules), closedWithout(search))

  private def starting(line: Int, kind: OpeningLine, rules: OpeningRules): Option[Pending] =
    kind match
      case OpeningLine.ChapterTitle => Some(Pending(line, OpeningKind.Chapter))
      case OpeningLine.SceneBreak =>
        rules.sceneBreak match
          case SceneBreakRule.NoOpening     => None
          case SceneBreakRule.SmallCapsOnly => Some(Pending(line, OpeningKind.SceneBreakSmallCaps))
          case SceneBreakRule.FullOpening   => Some(Pending(line, OpeningKind.SceneBreakFull))
      case _ => None
