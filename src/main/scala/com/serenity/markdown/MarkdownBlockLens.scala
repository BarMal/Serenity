package com.serenity.markdown

import scala.annotation.tailrec

object MarkdownBlockLens:

  private val fenceStateWindow  = 256
  private val fenceLookupWindow = 4_096

  private case class LineSource(lineCount: Int, lineAt: Int => Option[String]):
    def at(index: Int): String =
      lineAt(index).getOrElse("")

  def currentBlock(lines: Vector[String], activeLine: Int): Range.Inclusive =
    currentBlock(lines.length, lines.lift, activeLine)

  /** Resolves a block using only the source lines inspected by the block parser. */
  def currentBlock(
    lineCount: Int,
    lineAt: Int => Option[String],
    activeLine: Int
  ): Range.Inclusive =
    val lines = LineSource(lineCount, lineAt)
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
      val trimmed      = lines.at(index).trim
      val markerLength = if trimmed.startsWith("```") || trimmed.startsWith("~~~") then 3 else 0
      markerLength > 0 && trimmed.drop(markerLength).trim.nonEmpty

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

        countFencesBefore(index - 1, crossedBlank = false, count = 0, remaining = fenceStateWindow) % 2 == 0

    def isClosingFence(index: Int): Boolean = !hasFenceInfo(index)

    def previousFence(index: Int, remaining: Int): Option[Int] =
      if index < 0 || remaining <= 0 then None
      else if isFenceLine(lines.at(index)) then Some(index)
      else previousFence(index - 1, remaining - 1)

    def nextFence(index: Int, remaining: Int): Option[Int] =
      if index >= lines.lineCount || remaining <= 0 then None
      else if isFenceLine(lines.at(index)) then Some(index)
      else nextFence(index + 1, remaining - 1)

    if isFenceLine(lines.at(activeLine)) then
      if hasFenceInfo(activeLine) then
        nextFence(activeLine + 1, fenceLookupWindow).filter(isClosingFence).map(activeLine to _)
      else
        previousFence(activeLine - 1, fenceLookupWindow)
          .filter(isOpeningFence)
          .map(_ to activeLine)
          .orElse(nextFence(activeLine + 1, fenceLookupWindow).filter(isClosingFence).map(activeLine to _))
    else
      for
        start <- previousFence(activeLine - 1, fenceLookupWindow).filter(isOpeningFence)
        end   <- nextFence(activeLine + 1, fenceLookupWindow).filter(isClosingFence)
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
    Iterator
      .iterate(activeLine)(_ - 1)
      .takeWhile(index => index >= 0 && belongs(lines.at(index)))
      .foldLeft(activeLine)((_, index) => index)

  private def blockEnd(
    lines: LineSource,
    activeLine: Int,
    belongs: String => Boolean
  ): Int =
    Iterator
      .iterate(activeLine)(_ + 1)
      .takeWhile(index => index < lines.lineCount && belongs(lines.at(index)))
      .foldLeft(activeLine)((_, index) => index)

  private def isParagraphLine(line: String): Boolean =
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

  private def isSetextUnderline(line: String): Boolean =
    line.trim.matches("""^(=+|-+)$""")

  private def isThematicBreak(line: String): Boolean =
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
