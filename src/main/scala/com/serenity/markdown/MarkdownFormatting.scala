package com.serenity.markdown

import scala.annotation.tailrec

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
  * prefix. A selection's lines that run on within one paragraph are wrapped as one span; lines in different paragraphs,
  * list items or other blocks are wrapped one at a time, since emphasis cannot cross them. Code spans and fenced blocks
  * are left alone (#1943): a delimiter there is literal text, not formatting.
  */
object MarkdownFormatting:

  /** Adds `emphasis` to every selection, or removes it when every selection already has it -- the same toggle rule as
    * rich text. A caret acts on the word it is in, or opens an empty pair when it is in no word. Emphasis already
    * spelled with `_` or `__` counts, and is what gets removed.
    */
  def toggle(source: Rope, ranges: List[SourceRange], emphasis: Emphasis): Reformatted =
    val fences   = MarkdownBlockLens.fenceRangeIndex(source.lineCount, source.getLine)
    val fenced   = (line: Int) => fences.rangeAt(line).isDefined
    val segments = ranges.flatMap(segmentsOf(source, _, fenced)).map(absorbInnerMarkers(_, emphasis))
    val removing = segments.nonEmpty && segments.forall(isMarked(_, emphasis))
    val edits =
      if removing then segments.flatMap(unmarking(_, emphasis))
      else segments.filterNot(isMarked(_, emphasis)).flatMap(marking(_, emphasis))
    reformatted(edits, ranges)

  /** The emphasis every selection carries. Fenced blocks are not looked for here: finding them scans the whole
    * document, and the toolbar asks on every frame.
    */
  def emphasisAt(source: Rope, ranges: List[SourceRange]): Set[Emphasis] =
    val segments = ranges.flatMap(segmentsOf(source, _, _ => false))
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

  /** One block's share of a range, trimmed of surrounding whitespace. `text` is the source of the lines it sits on,
    * starting at `textStart`; `from` and `to` index into it.
    */
  final private case class Segment(text: String, textStart: Int, from: Int, to: Int):
    def before(length: Int): String = text.substring((from - length).max(0), from)
    def after(length: Int): String  = text.substring(to, (to + length).min(text.length))
    def inner: String               = text.substring(from, to)

  /** A matched pair of emphasis delimiter runs around a segment, `depth` characters out from it -- `**` on each side is
    * one ring of width two.
    */
  final private case class Ring(delimiter: Char, width: Int, depth: Int)

  private def segmentsOf(source: Rope, range: SourceRange, fenced: Int => Boolean): List[Segment] =
    val (startLine, startColumn) = source.offsetToLineColumn(range.start)
    val (endLine, _)             = source.offsetToLineColumn(range.end)
    val segments =
      if range.isEmpty then
        if fenced(startLine) then Nil
        else
          source.getLine(startLine).toList.map { line =>
            val from = Iterator.iterate(startColumn)(_ - 1).find(i => i == 0 || !isWordChar(line(i - 1))).getOrElse(0)
            val to =
              Iterator.iterate(startColumn)(_ + 1).find(i => i == line.length || !isWordChar(line(i))).getOrElse(0)
            Segment(line, source.lineColumnToOffset(startLine, 0), from, to)
          }
      else paragraphRuns(source, startLine, endLine, fenced).flatMap(selectedSegment(source, range, _))
    segments.filterNot(insideCode)

  /** The selected lines outside fences, grouped into runs one emphasis span can cover: consecutive paragraph lines. */
  private def paragraphRuns(source: Rope, startLine: Int, endLine: Int, fenced: Int => Boolean): List[(Int, Int)] =
    (startLine to endLine).toList
      .filterNot(fenced)
      .flatMap(index => source.getLine(index).map(index -> _))
      .foldLeft(List.empty[(Int, Int, String)]) {
        case ((first, last, lastText) :: done, (index, text))
            if index == last + 1 && continuesParagraph(lastText, text) =>
          (first, index, text) :: done
        case (done, (index, text)) => (index, index, text) :: done
      }
      .reverse
      .map(run => (run._1, run._2))

  private def continuesParagraph(previous: String, next: String): Boolean =
    MarkdownBlockLens.isParagraphLine(previous) && MarkdownBlockLens.isParagraphLine(next)

  private def selectedSegment(source: Rope, range: SourceRange, run: (Int, Int)): Option[Segment] =
    val (firstLine, lastLine) = run
    val textStart             = source.lineColumnToOffset(firstLine, 0)
    val textEnd               = source.lineColumnToOffset(lastLine, 0) + source.getLine(lastLine).fold(0)(_.length)
    val text                  = source.sliceString(textStart, textEnd)
    val from                  = (range.start - textStart).max(0)
    val to                    = (range.end - textStart).min(text.length)
    val trimmedFrom           = Iterator.iterate(from)(_ + 1).find(i => i >= to || !text(i).isWhitespace).getOrElse(to)
    val trimmedTo =
      Iterator.iterate(to)(_ - 1).find(i => i <= trimmedFrom || !text(i - 1).isWhitespace).getOrElse(trimmedFrom)
    Option.when(trimmedFrom < trimmedTo)(Segment(text, textStart, trimmedFrom, trimmedTo))

  private def isWordChar(char: Char): Boolean = char.isLetterOrDigit || char == '_'

  private def isDelimiter(char: Char): Boolean = char == '*' || char == '_'

  /** A delimiter at either end of the segment would land inside a code span, where it is literal text. */
  private def insideCode(segment: Segment): Boolean =
    codeSpans(segment.text).exists((start, end) =>
      (start < segment.from && segment.from < end) || (start < segment.to && segment.to < end)
    )

  /** `[start, end)` of each inline code span in `text`, backticks included: a run of backticks up to the next run of
    * the same length. Outside a span, a backslash escapes the character after it.
    */
  private def codeSpans(text: String): List[(Int, Int)] =
    @tailrec
    def scan(index: Int, found: List[(Int, Int)]): List[(Int, Int)] =
      if index >= text.length then found.reverse
      else if text(index) == '\\' then scan(index + 2, found)
      else if text(index) != '`' then scan(index + 1, found)
      else
        val run = runLength(text, index)
        closingRun(text, index + run, run) match
          case Some(close) => scan(close + run, (index, close + run) :: found)
          case None        => scan(index + run, found)
    scan(0, Nil)

  @tailrec
  private def closingRun(text: String, from: Int, length: Int): Option[Int] =
    val next = text.indexOf('`', from)
    if next < 0 then None
    else
      val run = runLength(text, next)
      if run == length then Some(next) else closingRun(text, next + run, length)

  private def runLength(text: String, from: Int): Int =
    val stop = text.indexWhere(_ != text(from), from)
    (if stop < 0 then text.length else stop) - from

  private def charAt(text: String, index: Int): Option[Char] =
    Option.when(index >= 0 && index < text.length)(text(index))

  /** Preceded by an odd number of backslashes. */
  private def escaped(text: String, index: Int): Boolean =
    Iterator.iterate(index - 1)(_ - 1).takeWhile(i => i >= 0 && text(i) == '\\').size % 2 == 1

  /** The unescaped delimiter run ending at `end`, as its character and length. */
  private def runBefore(text: String, end: Int): Option[(Char, Int)] =
    charAt(text, end - 1).filter(isDelimiter).flatMap { delimiter =>
      val run       = Iterator.iterate(end - 1)(_ - 1).takeWhile(i => i >= 0 && text(i) == delimiter).size
      val unescaped = if escaped(text, end - run) then run - 1 else run
      Option.when(unescaped > 0)(delimiter -> unescaped)
    }

  /** The unescaped delimiter run starting at `start`, as its character and length. */
  private def runAfter(text: String, start: Int): Option[(Char, Int)] =
    charAt(text, start).filter(char => isDelimiter(char) && !escaped(text, start)).map { delimiter =>
      delimiter -> runLength(text, start)
    }

  /** Every ring of delimiters around the segment, innermost first. An underscore run inside a word (`snake_case`) is
    * not emphasis in Markdown, so it ends the search.
    */
  private def rings(segment: Segment): List[Ring] =
    @tailrec
    def outward(depth: Int, found: List[Ring]): List[Ring] =
      (runBefore(segment.text, segment.from - depth), runAfter(segment.text, segment.to + depth)) match
        case (Some((open, openWidth)), Some((close, closeWidth)))
            if open == close && !(open == '_' && intraword(segment, depth, openWidth, closeWidth)) =>
          val width = openWidth.min(closeWidth)
          outward(depth + width, Ring(open, width, depth) :: found)
        case _ => found.reverse
    outward(0, Nil)

  private def intraword(segment: Segment, depth: Int, openWidth: Int, closeWidth: Int): Boolean =
    charAt(segment.text, segment.from - depth - openWidth - 1).exists(_.isLetterOrDigit) ||
      charAt(segment.text, segment.to + depth + closeWidth).exists(_.isLetterOrDigit)

  /** The innermost ring that gives the segment `emphasis`: a run of two is bold, one italic, three both. */
  private def carrier(segment: Segment, emphasis: Emphasis): Option[Ring] =
    rings(segment).find(ring =>
      emphasis match
        case Emphasis.Bold => ring.width >= 2
        case _             => ring.width == 1 || ring.width >= 3
    )

  /** A selection that takes in its own markers -- `[**word**]` -- is treated as the marked word inside them. */
  private def absorbInnerMarkers(segment: Segment, emphasis: Emphasis): Segment =
    emphasis match
      case Emphasis.Underline =>
        val inner = segment.inner
        if !isMarked(segment, emphasis) && inner.length > 7 && inner.startsWith("<u>") && inner.endsWith("</u>") then
          segment.copy(from = segment.from + 3, to = segment.to - 4)
        else segment
      case Emphasis.Bold | Emphasis.Italic =>
        if rings(segment).nonEmpty then segment
        else
          val inner = segment.inner
          inner.headOption.filter(isDelimiter).fold(segment) { delimiter =>
            val leading  = inner.takeWhile(_ == delimiter).length
            val trailing = inner.reverse.takeWhile(_ == delimiter).length
            val width    = leading.min(trailing).min((inner.length - 1) / 2)
            val absorbed = segment.copy(from = segment.from + width, to = segment.to - width)
            if width > 0 && rings(absorbed).nonEmpty then absorbed else segment
          }

  private def isMarked(segment: Segment, emphasis: Emphasis): Boolean =
    emphasis match
      case Emphasis.Underline              => segment.before(3) == "<u>" && segment.after(4) == "</u>"
      case Emphasis.Bold | Emphasis.Italic => carrier(segment, emphasis).isDefined

  private def marking(segment: Segment, emphasis: Emphasis): List[Edit] =
    val start = segment.textStart + segment.from
    val end   = segment.textStart + segment.to
    if start == end then List(Edit(start, start, emphasis.open + emphasis.close, landing = emphasis.open.length))
    else
      List(
        Edit(start, start, emphasis.open, landing = emphasis.open.length),
        Edit(end, end, emphasis.close, landing = 0)
      )

  /** Removes the delimiters that carry `emphasis`, wherever they sit among the rings around the segment. */
  private def unmarking(segment: Segment, emphasis: Emphasis): List[Edit] =
    val start = segment.textStart + segment.from
    val end   = segment.textStart + segment.to
    val depth = emphasis match
      case Emphasis.Underline              => Some(0)
      case Emphasis.Bold | Emphasis.Italic => carrier(segment, emphasis).map(_.depth)
    depth.toList.flatMap { out =>
      List(
        Edit(start - out - emphasis.open.length, start - out, "", landing = 0),
        Edit(end + out, end + out + emphasis.close.length, "", landing = 0)
      )
    }

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
