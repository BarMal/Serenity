package com.serenity.ui.openings

import scala.annotation.tailrec

import com.serenity.document.MarkdownHeadingIndex

/** Classifies the lines of a Markdown text for the search for chapter openings.
  *
  * Headings and fenced code come from the [[MarkdownHeadingIndex]], so a `#` line inside code is no chapter title and a
  * `---` under a paragraph, which Markdown reads as a setext underline, is no scene break. Every other line is judged
  * from its own text and its neighbours', and the first line of a paragraph speaks for the paragraph.
  */
private[openings] object MarkdownOpenings:

  private val ListItem  = """^ {0,3}([-*+]|\d{1,9}[.)])([ \t]|$).*""".r
  private val AtxMarker = """^ {0,3}#{1,6}([ \t].*)?$""".r
  private val TableRule = """^\s*\|?\s*:?-+:?\s*(\|\s*:?-+:?\s*)*\|?\s*$""".r

  def classify(headings: MarkdownHeadingIndex, rules: OpeningRules, explicitEpigraph: Int => Boolean)(
    line: Int
  ): OpeningLine =
    val content               = headings.content
    def text(at: Int): String = content.getLine(at).getOrElse("")

    def blockquoteContinues(from: Int): Boolean =
      @tailrec
      def up(at: Int): Boolean =
        if at < 0 || isBlank(text(at)) || headings.topLevelHeadingAt(at).isDefined || headings.isInFence(at) ||
            structural(text(at))
        then false
        else if isBlockquote(text(at)) then true
        else up(at - 1)
      up(from - 1)

    val own = text(line)
    if headings.isInFence(line) then OpeningLine.Blocked
    else if !isBlank(own) && headings.topLevelHeadingAt(line).isEmpty && inHtmlBlock(headings, line) then
      OpeningLine.Blocked
    else
      headings.topLevelHeadingAt(line) match
        case Some(heading) if line == heading.firstLine =>
          heading.level match
            case 1 => OpeningLine.ChapterTitle
            case _ => OpeningLine.SubHeading
        case Some(_) => OpeningLine.Blank
        case None =>
          val trimmed = own.trim
          if isBlank(own) then OpeningLine.Blank
          else if trimmed == "#" || (!AtxMarker.matches(own) && SceneBreaks.isSceneBreak(trimmed)) then
            OpeningLine.SceneBreak
          else if AtxMarker.matches(own) then OpeningLine.Blank
          else if explicitEpigraph(line) && rules.epigraph == EpigraphRule.ExplicitOnly then OpeningLine.Epigraph
          else if isBlockquote(own) || blockquoteContinues(line) then blockquote(rules.epigraph)
          else if blocked(own, text(line + 1)) then OpeningLine.Blocked
          else OpeningLine.Body(handDropCap = false)

  /** Whether `line` is a blank line's successor, so that nothing above it decides what block it is in. */
  def startsBlock(headings: MarkdownHeadingIndex)(line: Int): Boolean =
    line <= 0 || isBlank(headings.content.getLine(line - 1).getOrElse(""))

  /** An HTML block runs from a line starting with `<` to the next blank line, headings and all. */
  private def inHtmlBlock(headings: MarkdownHeadingIndex, line: Int): Boolean =
    @tailrec
    def blockStart(at: Int): Int =
      if at > 0 && !isBlank(headings.content.getLine(at - 1).getOrElse("")) then blockStart(at - 1) else at
    headings.content.getLine(blockStart(line)).exists(_.trim.startsWith("<"))

  private def blockquote(rule: EpigraphRule): OpeningLine =
    rule match
      case EpigraphRule.AutoDetect => OpeningLine.Epigraph
      case _                       => OpeningLine.Blocked

  private def isBlank(line: String): Boolean = line.forall(char => char == ' ' || char == '\t')

  private def isBlockquote(line: String): Boolean = line.dropWhile(_ == ' ').startsWith(">")

  private def structural(line: String): Boolean =
    ListItem.matches(line) || SceneBreaks.isSceneBreak(line.trim) || AtxMarker.matches(line)

  private def blocked(line: String, next: String): Boolean =
    val trimmed = line.trim
    trimmed.length < 2 || line.startsWith("\t") || line.startsWith("    ") || ListItem.matches(line) ||
    trimmed.startsWith("<") || trimmed.startsWith("![") || (trimmed.startsWith("[") && trimmed.contains("]:")) ||
    isTableRow(trimmed, next)

  private def isTableRow(trimmed: String, next: String): Boolean =
    trimmed.startsWith("|") || (trimmed.contains('|') && (isTableRule(trimmed) || isTableRule(next.trim)))

  private def isTableRule(line: String): Boolean = line.contains('|') && TableRule.matches(line)
