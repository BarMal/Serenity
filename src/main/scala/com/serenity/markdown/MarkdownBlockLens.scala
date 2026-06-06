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

  private def contiguousBlock(
    lines: Vector[String],
    activeLine: Int,
    belongs: String => Boolean
  ): Option[Range.Inclusive] =
    if !belongs(lines(activeLine)) then None
    else
      val start = Iterator
        .iterate(activeLine)(_ - 1)
        .takeWhile(index => index >= 0 && belongs(lines(index)))
        .toList
        .last
      val end = Iterator
        .iterate(activeLine)(_ + 1)
        .takeWhile(index => index < lines.length && belongs(lines(index)))
        .toList
        .last
      Some(start to end)

  private def paragraphBlock(lines: Vector[String], activeLine: Int): Range.Inclusive =
    val start = Iterator
      .iterate(activeLine)(_ - 1)
      .takeWhile(index => index >= 0 && isParagraphLine(lines(index)))
      .toList
      .last
    val end = Iterator
      .iterate(activeLine)(_ + 1)
      .takeWhile(index => index < lines.length && isParagraphLine(lines(index)))
      .toList
      .last
    start to end

  private def isParagraphLine(line: String): Boolean =
    val trimmed = line.trim
    trimmed.nonEmpty &&
    !isFenceLine(line) &&
    !isTableLine(line) &&
    !isListLikeLine(line) &&
    !isBlockQuoteLine(line)

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
