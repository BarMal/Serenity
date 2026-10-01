package com.serenity.markdown

import com.serenity.rope.Rope

/** Inline formatting Markdown can spell. Underline has no Markdown syntax, so it uses the HTML tag a `.md` save already
  * writes for it.
  */
enum Emphasis(val open: String, val close: String):
  case Bold      extends Emphasis("**", "**")
  case Italic    extends Emphasis("*", "*")
  case Underline extends Emphasis("<u>", "</u>")

/** A selection, or a caret when empty, as offsets into the source text. */
final case class SourceRange(anchor: Int, focus: Int):
  def start: Int       = math.min(anchor, focus)
  def end: Int         = math.max(anchor, focus)
  def isEmpty: Boolean = anchor == focus

/** Replaces `[start, end)` of the source text with `text`. */
final case class SourceEdit(start: Int, end: Int, text: String)

/** `edits` are in source order and never overlap; two at one offset are inserted in list order. `ranges` are the input
  * ranges as they stand once the edits are applied.
  */
final case class Reformatted(edits: List[SourceEdit], ranges: List[SourceRange])

/** Formatting a Markdown file by editing its source: wrapping words in `**`, `*` or `<u>`, and setting a line's `#`
  * prefix. Emphasis is line-local in Markdown, so a selection spanning lines is formatted a line at a time.
  */
object MarkdownFormatting:

  /** Adds `emphasis` to every selection, or removes it when every selection already has it -- the same toggle rule as
    * rich text. A caret acts on the word it is in, or opens an empty pair when it is in no word.
    */
  def toggle(source: Rope, ranges: List[SourceRange], emphasis: Emphasis): Reformatted =
    val segments = ranges.flatMap(segmentsOf(source, _)).map(absorbInnerMarkers(_, emphasis))
    val removing = segments.nonEmpty && segments.forall(isMarked(_, emphasis))
    val edits =
      if removing then segments.flatMap(unmarking(_, emphasis))
      else segments.filterNot(isMarked(_, emphasis)).flatMap(marking(_, emphasis))
    reformatted(edits, ranges)

  /** The emphasis every selection carries. */
  def emphasisAt(source: Rope, ranges: List[SourceRange]): Set[Emphasis] =
    val segments = ranges.flatMap(segmentsOf(source, _))
    Emphasis.values.toSet.filter(emphasis =>
      segments.nonEmpty && segments.map(absorbInnerMarkers(_, emphasis)).forall(isMarked(_, emphasis))
    )

  /** Gives every line the ranges touch a level-`level` heading prefix, or none for level 0. A multi-line selection
    * leaves its blank lines alone.
    */
  def setHeading(source: Rope, ranges: List[SourceRange], level: Int): Reformatted =
    val prefix = if level > 0 then "#" * level.min(6) + " " else ""
    val edits = ranges
      .flatMap(range => headingLines(source, range).map(_ -> range.isEmpty))
      .distinctBy((line, _) => line)
      .sortBy((line, _) => line)
      .flatMap { (line, caretOnly) =>
        source.getLine(line).filter(text => caretOnly || text.trim.nonEmpty).flatMap { text =>
          val existing  = headingPrefixLength(text)
          val lineStart = source.lineColumnToOffset(line, 0)
          Option.when(text.substring(0, existing) != prefix)(
            Edit(lineStart, lineStart + existing, prefix, landing = prefix.length)
          )
        }
      }
    reformatted(edits, ranges)

  /** The heading level of the first line the ranges touch, 0 for body text. */
  def headingLevelAt(source: Rope, ranges: List[SourceRange]): Int =
    ranges.headOption
      .flatMap(range => source.getLine(source.offsetToLineColumn(range.start)._1))
      .map(text => text.substring(0, headingPrefixLength(text)).count(_ == '#'))
      .getOrElse(0)

  /** An edit plus where an offset exactly at its insertion point, or inside the text it replaces, lands within the new
    * text: 0 keeps it before, `text.length` puts it after.
    */
  final private case class Edit(start: Int, end: Int, text: String, landing: Int)

  /** One line's share of a range, already trimmed of surrounding whitespace, with the line it sits on. */
  final private case class Segment(line: String, lineStart: Int, from: Int, to: Int):
    def before(length: Int): String = line.substring((from - length).max(0), from)
    def after(length: Int): String  = line.substring(to, (to + length).min(line.length))
    def inner: String               = line.substring(from, to)
    def starsBefore: Int            = line.substring(0, from).reverseIterator.takeWhile(_ == '*').size
    def starsAfter: Int             = line.substring(to).iterator.takeWhile(_ == '*').size

  private def segmentsOf(source: Rope, range: SourceRange): List[Segment] =
    val (startLine, startColumn) = source.offsetToLineColumn(range.start)
    val (endLine, endColumn)     = source.offsetToLineColumn(range.end)
    if range.isEmpty then
      source.getLine(startLine).toList.map { line =>
        val lineStart = source.lineColumnToOffset(startLine, 0)
        val from      = Iterator.iterate(startColumn)(_ - 1).find(i => i == 0 || !isWordChar(line(i - 1))).getOrElse(0)
        val to = Iterator.iterate(startColumn)(_ + 1).find(i => i == line.length || !isWordChar(line(i))).getOrElse(0)
        Segment(line, lineStart, from, to)
      }
    else
      (startLine to endLine).toList.flatMap { lineIndex =>
        source.getLine(lineIndex).flatMap { line =>
          val from        = if lineIndex == startLine then startColumn else 0
          val to          = if lineIndex == endLine then endColumn.min(line.length) else line.length
          val trimmedFrom = Iterator.iterate(from)(_ + 1).find(i => i >= to || !line(i).isWhitespace).getOrElse(to)
          val trimmedTo =
            Iterator.iterate(to)(_ - 1).find(i => i <= trimmedFrom || !line(i - 1).isWhitespace).getOrElse(trimmedFrom)
          Option.when(trimmedFrom < trimmedTo)(
            Segment(line, source.lineColumnToOffset(lineIndex, 0), trimmedFrom, trimmedTo)
          )
        }
      }

  private def isWordChar(char: Char): Boolean = char.isLetterOrDigit || char == '_'

  /** A selection that takes in its own markers -- `[**word**]` -- is treated as the marked word inside them. */
  private def absorbInnerMarkers(segment: Segment, emphasis: Emphasis): Segment =
    emphasis match
      case Emphasis.Underline =>
        val inner = segment.inner
        if !isMarked(segment, emphasis) && inner.length > 7 && inner.startsWith("<u>") && inner.endsWith("</u>") then
          segment.copy(from = segment.from + 3, to = segment.to - 4)
        else segment
      case Emphasis.Bold | Emphasis.Italic =>
        if segment.starsBefore > 0 || segment.starsAfter > 0 then segment
        else
          val inner    = segment.inner
          val leading  = inner.takeWhile(_ == '*').length
          val trailing = inner.reverse.takeWhile(_ == '*').length
          val stars    = leading.min(trailing).min((inner.length - 1) / 2)
          segment.copy(from = segment.from + stars, to = segment.to - stars)

  /** Stars are shared between bold and italic: a run of two is bold, one is italic, three is both. */
  private def isMarked(segment: Segment, emphasis: Emphasis): Boolean =
    val stars = segment.starsBefore.min(segment.starsAfter)
    emphasis match
      case Emphasis.Bold      => stars >= 2
      case Emphasis.Italic    => stars == 1 || stars >= 3
      case Emphasis.Underline => segment.before(3) == "<u>" && segment.after(4) == "</u>"

  private def marking(segment: Segment, emphasis: Emphasis): List[Edit] =
    val start = segment.lineStart + segment.from
    val end   = segment.lineStart + segment.to
    if start == end then List(Edit(start, start, emphasis.open + emphasis.close, landing = emphasis.open.length))
    else
      List(
        Edit(start, start, emphasis.open, landing = emphasis.open.length),
        Edit(end, end, emphasis.close, landing = 0)
      )

  private def unmarking(segment: Segment, emphasis: Emphasis): List[Edit] =
    val start = segment.lineStart + segment.from
    val end   = segment.lineStart + segment.to
    List(
      Edit(start - emphasis.open.length, start, "", landing = 0),
      Edit(end, end + emphasis.close.length, "", landing = 0)
    )

  private def headingLines(source: Rope, range: SourceRange): List[Int] =
    val (startLine, _)       = source.offsetToLineColumn(range.start)
    val (endLine, endColumn) = source.offsetToLineColumn(range.end)
    val lastLine             = if endLine > startLine && endColumn == 0 then endLine - 1 else endLine
    (startLine to lastLine).toList

  /** The length of a line's `#` heading prefix and the whitespace after it, or 0 when it is not a heading. */
  private def headingPrefixLength(line: String): Int =
    val hashes = line.takeWhile(_ == '#').length
    val gap    = line.drop(hashes).takeWhile(char => char == ' ' || char == '\t').length
    if hashes >= 1 && hashes <= 6 && (gap > 0 || hashes == line.length) then hashes + gap else 0

  private def reformatted(edits: List[Edit], ranges: List[SourceRange]): Reformatted =
    val ordered = edits.sortBy(edit => (edit.start, edit.end, edit.landing))
    Reformatted(
      ordered.map(edit => SourceEdit(edit.start, edit.end, edit.text)),
      ranges.map(range => SourceRange(mapped(range.anchor, ordered), mapped(range.focus, ordered)))
    )

  private def mapped(offset: Int, edits: List[Edit]): Int =
    edits
      .foldLeft((offset, 0, false)) {
        case ((result, shift, settled), edit) =>
          val delta = edit.text.length - (edit.end - edit.start)
          if settled || offset < edit.start then (result, shift, settled)
          else if offset > edit.end || (offset == edit.end && edit.start < edit.end) then
            (offset + shift + delta, shift + delta, false)
          else (edit.start + shift + edit.landing, shift, true)
      }
      ._1
