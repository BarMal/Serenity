package com.serenity.markdown

import scala.annotation.tailrec

object MarkdownBlockLens:

  /** A line's place in a pipe table. */
  enum TableRow:
    case None, Header, Delimiter, Body

  /** A line's place in a setext heading, whose text lines are underlined by a line of `=` or `-`. */
  enum SetextRole:
    case None
    case Text(level: Int)
    case Underline

  /** A setext heading: the paragraph lines and, last, the underline. */
  final case class SetextHeading(lines: Range.Inclusive, level: Int)

  /** The document's fenced code blocks, pipe tables and setext headings, found in one pass over its lines. Despite the
    * name it is the one block index the live preview consults, so no further scan of the document is needed.
    */
  final case class FenceRangeIndex(
      ranges: Vector[Range.Inclusive],
      tables: Vector[Range.Inclusive] = Vector.empty,
      setext: Vector[SetextHeading] = Vector.empty
  ):

    def rangeAt(line: Int): Option[Range.Inclusive] = containing(ranges, line)(identity)

    def tableAt(line: Int): Option[Range.Inclusive] = containing(tables, line)(identity)

    def tableRowAt(line: Int): TableRow =
      tableAt(line).fold(TableRow.None) { table =>
        if line == table.start then TableRow.Header
        else if line == table.start + 1 then TableRow.Delimiter
        else TableRow.Body
      }

    def setextAt(line: Int): SetextRole =
      containing(setext, line)(_.lines).fold(SetextRole.None) { heading =>
        if line == heading.lines.end then SetextRole.Underline else SetextRole.Text(heading.level)
      }

  private def containing[A](items: Vector[A], line: Int)(range: A => Range.Inclusive): Option[A] =
    @annotation.tailrec
    def search(low: Int, high: Int): Option[A] =
      if low > high then None
      else
        val middle = (low + high) / 2
        val item   = items(middle)
        val lines  = range(item)
        if line < lines.start then search(low, middle - 1)
        else if line > lines.end then search(middle + 1, high)
        else Some(item)
    search(0, items.length - 1)

  def fenceRangeIndex(lineCount: Int, lineAt: Int => Option[String]): FenceRangeIndex =
    fenceRangeIndex((0 until lineCount).iterator.map(index => lineAt(index).getOrElse("")))

  final private case class IndexScan(
      fences: Vector[Int],
      tables: Vector[Range.Inclusive],
      inFence: Boolean,
      header: Option[Int],
      tableStart: Option[Int],
      lineCount: Int,
      paragraphStart: Option[Int],
      setext: Vector[SetextHeading]
  ):

    def closeTable(lastLine: Int): IndexScan =
      tableStart.fold(this)(start => copy(tables = tables :+ (start to lastLine), tableStart = None))

    def next(line: String): IndexScan =
      val index = lineCount
      val seen  = copy(lineCount = lineCount + 1)
      if isFenceLine(line) then
        seen
          .closeTable(index - 1)
          .copy(fences = fences :+ index, inFence = !inFence, header = None, paragraphStart = None)
      else if inFence then seen
      else if tableStart.isDefined && MarkdownInlineTablePreview.isTableRow(line) then seen
      else seen.closeTable(index - 1).candidate(line, index).paragraphs(line, index)

    private def paragraphs(line: String, index: Int): IndexScan =
      paragraphStart match
        case _ if tableStart.isDefined => copy(paragraphStart = None)
        case Some(start) if isSetextUnderline(line) =>
          val level = if line.trim.startsWith("=") then 1 else 2
          copy(setext = setext :+ SetextHeading(start to index, level), paragraphStart = None)
        case _ if isParagraphLine(line) => copy(paragraphStart = paragraphStart.orElse(Some(index)))
        case _                          => copy(paragraphStart = None)

    private def candidate(line: String, index: Int): IndexScan =
      if line.indexOf('|') < 0 then copy(header = None)
      else if header.isDefined && MarkdownInlineTablePreview.isTableSeparator(line) then
        copy(tableStart = header, header = None)
      else copy(header = Option.when(MarkdownInlineTablePreview.isTableRow(line))(index))

  /** The fenced blocks of a document given its lines in order, each paired fence line to the next, its tables (a header
    * row, a delimiter row and the rows after them) and its setext headings, outside any fence.
    */
  def fenceRangeIndex(lines: Iterator[String]): FenceRangeIndex =
    val scan =
      lines.foldLeft(IndexScan(Vector.empty, Vector.empty, false, None, None, 0, None, Vector.empty))(_.next(_))
    val done = scan.closeTable(scan.lineCount - 1)
    FenceRangeIndex(
      done.fences.grouped(2).collect { case Vector(open, close) => open to close }.toVector,
      done.tables,
      done.setext
    )

  final private case class LineSource(lineCount: Int, lineAt: Int => Option[String], fenceProbeWindow: Option[Int]):
    def at(index: Int): String =
      lineAt(index).getOrElse("")

  def currentBlock(lines: Vector[String], activeLine: Int): Range.Inclusive =
    currentBlock(LineSource(lines.length, lines.lift, fenceProbeWindow = None), activeLine)

  /** Resolves a block using only the source lines inspected by the block parser. */
  def currentBlock(
    lineCount: Int,
    lineAt: Int => Option[String],
    activeLine: Int
  ): Range.Inclusive =
    currentBlock(LineSource(lineCount, lineAt, fenceProbeWindow = None), activeLine)

  /** Resolves a block with a bounded fence probe for fixed-viewport rendering. */
  def currentBlock(
    lineCount: Int,
    lineAt: Int => Option[String],
    activeLine: Int,
    fenceProbeWindow: Int
  ): Range.Inclusive =
    currentBlock(LineSource(lineCount, lineAt, Some(fenceProbeWindow.max(1))), activeLine)

  private def currentBlock(lines: LineSource, activeLine: Int): Range.Inclusive =
    if lines.lineCount <= 0 then 0 to 0
    else
      val clampedLine = activeLine.max(0).min(lines.lineCount - 1)
      if lines.at(clampedLine).trim.isEmpty then clampedLine to clampedLine
      else
        fencedBlock(lines, clampedLine)
          .orElse(tableBlock(lines, clampedLine))
          .orElse(headingBlock(lines, clampedLine))
          .orElse(setextHeadingBlock(lines, clampedLine))
          .orElse(thematicBreakBlock(lines, clampedLine))
          .orElse(listItemBlock(lines, clampedLine))
          .orElse(blockQuoteBlock(lines, clampedLine))
          .getOrElse(paragraphBlock(lines, clampedLine))

  def activeBlockLineSet(lines: Vector[String], activeLine: Option[Int]): Set[Int] =
    activeLine
      .filter(line => line >= 0 && line < lines.length)
      .map(line => currentBlock(lines, line).toSet)
      .getOrElse(Set.empty)

  private def fencedBlock(lines: LineSource, activeLine: Int): Option[Range.Inclusive] =
    def hasFenceInfo(index: Int): Boolean =
      val trimmed = lines.at(index).trim
      val marker  = trimmed.headOption.filter(ch => ch == '`' || ch == '~')
      marker.exists { ch =>
        val markerLength = trimmed.takeWhile(_ == ch).length
        markerLength >= 3 && trimmed.drop(markerLength).trim.nonEmpty
      }

    def isOpeningFence(index: Int): Boolean =
      if hasFenceInfo(index) then true
      else
        @tailrec
        def countFencesBefore(cursor: Int, crossedBlank: Boolean, count: Int, remaining: Int): Int =
          if cursor < 0 || remaining <= 0 then count
          else
            val line = lines.at(cursor)
            if line.trim.isEmpty then
              if crossedBlank then count
              else countFencesBefore(cursor - 1, crossedBlank = true, count, remaining - 1)
            else
              countFencesBefore(cursor - 1, crossedBlank, count + (if isFenceLine(line) then 1 else 0), remaining - 1)

        countFencesBefore(
          index - 1,
          crossedBlank = false,
          count = 0,
          remaining = lines.fenceProbeWindow.getOrElse(lines.lineCount)
        ) % 2 == 0

    def isClosingFence(index: Int): Boolean = !hasFenceInfo(index)

    def previousFence(index: Int, remaining: Int): Option[Int] =
      if index < 0 || remaining <= 0 then None
      else if isFenceLine(lines.at(index)) then Some(index)
      else previousFence(index - 1, remaining - 1)

    def nextFence(index: Int, remaining: Int): Option[Int] =
      if index >= lines.lineCount || remaining <= 0 then None
      else if isFenceLine(lines.at(index)) then Some(index)
      else nextFence(index + 1, remaining - 1)

    val fenceProbe = lines.fenceProbeWindow.getOrElse(lines.lineCount)

    if isFenceLine(lines.at(activeLine)) then
      if hasFenceInfo(activeLine) then nextFence(activeLine + 1, fenceProbe).filter(isClosingFence).map(activeLine to _)
      else
        previousFence(activeLine - 1, fenceProbe)
          .filter(isOpeningFence)
          .map(_ to activeLine)
          .orElse(nextFence(activeLine + 1, fenceProbe).filter(isClosingFence).map(activeLine to _))
    else
      for
        start <- previousFence(activeLine - 1, fenceProbe).filter(isOpeningFence)
        end   <- nextFence(activeLine + 1, fenceProbe).filter(isClosingFence)
      yield start to end

  private def tableBlock(lines: LineSource, activeLine: Int): Option[Range.Inclusive] =
    if !isTableLine(lines.at(activeLine)) then None
    else contiguousBlock(lines, activeLine, isTableLine)

  private def headingBlock(lines: LineSource, activeLine: Int): Option[Range.Inclusive] =
    Option.when(isHeadingLine(lines.at(activeLine)))(activeLine to activeLine)

  private def setextHeadingBlock(lines: LineSource, activeLine: Int): Option[Range.Inclusive] =
    Option
      .when(isSetextUnderline(lines.at(activeLine)) && activeLine > 0 && isParagraphLine(lines.at(activeLine - 1))) {
        (activeLine - 1) to activeLine
      }
      .orElse {
        Option.when(activeLine + 1 < lines.lineCount && isSetextUnderline(lines.at(activeLine + 1))) {
          activeLine to (activeLine + 1)
        }
      }

  private def thematicBreakBlock(lines: LineSource, activeLine: Int): Option[Range.Inclusive] =
    Option.when(isThematicBreak(lines.at(activeLine)))(activeLine to activeLine)

  private def blockQuoteBlock(lines: LineSource, activeLine: Int): Option[Range.Inclusive] =
    Option.when(isBlockQuoteLine(lines.at(activeLine))) {
      if isBlockQuoteSeparator(lines.at(activeLine)) then activeLine to activeLine
      else blockSpan(lines, activeLine, isBlockQuoteContentLine)
    }

  private def listItemBlock(lines: LineSource, activeLine: Int): Option[Range.Inclusive] =
    listItemStart(lines, activeLine).map { start =>
      val itemIndent = leadingIndent(lines.at(start))
      val end = Iterator
        .iterate(start + 1)(_ + 1)
        .takeWhile(index =>
          index < lines.lineCount &&
            lines.at(index).trim.nonEmpty &&
            !isSiblingListItem(lines.at(index), itemIndent)
        )
        .foldLeft(start)((_, index) => index)
      start to end
    }

  private def listItemStart(lines: LineSource, activeLine: Int): Option[Int] =
    Option.when(isListItemLine(lines.at(activeLine)))(activeLine).orElse {
      val activeIndent = leadingIndent(lines.at(activeLine))
      Iterator
        .iterate(activeLine - 1)(_ - 1)
        .takeWhile(index => index >= 0 && lines.at(index).trim.nonEmpty)
        .collectFirst {
          case index
              if isListItemLine(lines.at(index)) &&
                leadingIndent(lines.at(index)) < activeIndent =>
            index
        }
    }

  private def isSiblingListItem(line: String, itemIndent: Int): Boolean =
    isListItemLine(line) && leadingIndent(line) <= itemIndent

  private def leadingIndent(line: String): Int =
    line.takeWhile(char => char == ' ' || char == '\t').length

  private def contiguousBlock(
    lines: LineSource,
    activeLine: Int,
    belongs: String => Boolean
  ): Option[Range.Inclusive] =
    Option.when(belongs(lines.at(activeLine)))(blockSpan(lines, activeLine, belongs))

  private def paragraphBlock(lines: LineSource, activeLine: Int): Range.Inclusive =
    blockSpan(lines, activeLine, isParagraphLine)

  private def blockSpan(
    lines: LineSource,
    activeLine: Int,
    belongs: String => Boolean
  ): Range.Inclusive =
    blockStart(lines, activeLine, belongs) to blockEnd(lines, activeLine, belongs)

  private def blockStart(
    lines: LineSource,
    activeLine: Int,
    belongs: String => Boolean
  ): Int =
    val firstProbeLine = activeLine - lines.fenceProbeWindow.getOrElse(lines.lineCount)
    Iterator
      .iterate(activeLine)(_ - 1)
      .takeWhile(index => index >= 0 && index >= firstProbeLine && belongs(lines.at(index)))
      .foldLeft(activeLine)((_, index) => index)

  private def blockEnd(
    lines: LineSource,
    activeLine: Int,
    belongs: String => Boolean
  ): Int =
    val lastProbeLine = activeLine + lines.fenceProbeWindow.getOrElse(lines.lineCount)
    Iterator
      .iterate(activeLine)(_ + 1)
      .takeWhile(index => index < lines.lineCount && index <= lastProbeLine && belongs(lines.at(index)))
      .foldLeft(activeLine)((_, index) => index)

  private[markdown] def isParagraphLine(line: String): Boolean =
    val trimmed = line.trim
    trimmed.nonEmpty &&
    !isFenceLine(line) &&
    !isTableLine(line) &&
    !isHeadingLine(line) &&
    !isSetextUnderline(line) &&
    !isThematicBreak(line) &&
    !isListItemLine(line) &&
    !isBlockQuoteLine(line)

  private def isHeadingLine(line: String): Boolean =
    line.trim.matches("""^#{1,6}\s+.*""")

  private[markdown] def isSetextUnderline(line: String): Boolean =
    line.trim.matches("""^(=+|-+)$""")

  private[markdown] def isThematicBreak(line: String): Boolean =
    val markers = line.filterNot(_.isWhitespace)
    markers.length >= 3 && markers.headOption.exists(Set('*', '-', '_').contains) && markers.forall(_ == markers.head)

  private def isListItemLine(line: String): Boolean =
    val trimmed = line.trim
    trimmed.matches("""^([-*+]|\d+\.)\s+.*""")

  private def isBlockQuoteLine(line: String): Boolean =
    line.trim.startsWith(">")

  private def isBlockQuoteContentLine(line: String): Boolean =
    isBlockQuoteLine(line) && !isBlockQuoteSeparator(line)

  private def isBlockQuoteSeparator(line: String): Boolean =
    line.trim.drop(1).trim.isEmpty

  private def isFenceLine(line: String): Boolean =
    val trimmed = line.trim
    trimmed.startsWith("```") || trimmed.startsWith("~~~")

  private def isTableLine(line: String): Boolean =
    val trimmed = line.trim
    trimmed.contains("|") && trimmed.count(_ == '|') >= 2
