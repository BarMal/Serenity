package com.serenity.markdown

object MarkdownBlockLens:

  def currentBlock(lines: Vector[String], activeLine: Int): Range.Inclusive =
    if lines.isEmpty then 0 to 0
    else
      val clampedLine = activeLine.max(0).min(lines.length - 1)
      if lines(clampedLine).trim.isEmpty then clampedLine to clampedLine
      else
        fencedBlock(lines, clampedLine)
          .orElse(tableBlock(lines, clampedLine))
          .orElse(headingBlock(lines, clampedLine))
          .orElse(contiguousBlock(lines, clampedLine, isListLikeLine))
          .orElse(contiguousBlock(lines, clampedLine, isBlockQuoteLine))
          .getOrElse(paragraphBlock(lines, clampedLine))

  def activeBlockLineSet(lines: Vector[String], activeLine: Option[Int]): Set[Int] =
    activeLine
      .filter(line => line >= 0 && line < lines.length)
      .map(line => currentBlock(lines, line).toSet)
      .getOrElse(Set.empty)

  private def fencedBlock(lines: Vector[String], activeLine: Int): Option[Range.Inclusive] =
    val fenceIndices = lines.zipWithIndex.collect {
      case (line, index) if isFenceLine(line) => index
    }
    fenceIndices.grouped(2).collectFirst {
      case Vector(start, end) if activeLine >= start && activeLine <= end => start to end
      case Vector(start) if activeLine >= start                           => start to (lines.length - 1)
    }

  private def tableBlock(lines: Vector[String], activeLine: Int): Option[Range.Inclusive] =
    if !isTableLine(lines(activeLine)) then None
    else contiguousBlock(lines, activeLine, isTableLine)

  private def headingBlock(lines: Vector[String], activeLine: Int): Option[Range.Inclusive] =
    Option.when(isHeadingLine(lines(activeLine)))(activeLine to activeLine)

  private def contiguousBlock(
    lines: Vector[String],
    activeLine: Int,
    belongs: String => Boolean
  ): Option[Range.Inclusive] =
    Option.when(belongs(lines(activeLine)))(blockSpan(lines, activeLine, belongs))

  private def paragraphBlock(lines: Vector[String], activeLine: Int): Range.Inclusive =
    blockSpan(lines, activeLine, isParagraphLine)

  private def blockSpan(
    lines: Vector[String],
    activeLine: Int,
    belongs: String => Boolean
  ): Range.Inclusive =
    blockStart(lines, activeLine, belongs) to blockEnd(lines, activeLine, belongs)

  private def blockStart(
    lines: Vector[String],
    activeLine: Int,
    belongs: String => Boolean
  ): Int =
    Iterator
      .iterate(activeLine)(_ - 1)
      .takeWhile(index => index >= 0 && belongs(lines(index)))
      .foldLeft(activeLine)((_, index) => index)

  private def blockEnd(
    lines: Vector[String],
    activeLine: Int,
    belongs: String => Boolean
  ): Int =
    Iterator
      .iterate(activeLine)(_ + 1)
      .takeWhile(index => index < lines.length && belongs(lines(index)))
      .foldLeft(activeLine)((_, index) => index)

  private def isParagraphLine(line: String): Boolean =
    val trimmed = line.trim
    trimmed.nonEmpty &&
    !isFenceLine(line) &&
    !isTableLine(line) &&
    !isHeadingLine(line) &&
    !isListLikeLine(line) &&
    !isBlockQuoteLine(line)

  private def isHeadingLine(line: String): Boolean =
    line.trim.matches("""^#{1,6}\s+.*""")

  private def isListLikeLine(line: String): Boolean =
    val trimmed = line.trim
    trimmed.matches("""^([-*+]|\d+\.)\s+.*""") || line.startsWith("  ") || line.startsWith("\t")

  private def isBlockQuoteLine(line: String): Boolean =
    line.trim.startsWith(">")

  private def isFenceLine(line: String): Boolean =
    val trimmed = line.trim
    trimmed.startsWith("```") || trimmed.startsWith("~~~")

  private def isTableLine(line: String): Boolean =
    val trimmed = line.trim
    trimmed.contains("|") && trimmed.count(_ == '|') >= 2
