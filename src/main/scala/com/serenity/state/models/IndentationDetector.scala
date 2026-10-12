package com.serenity.state.models

import com.serenity.lsp.config.LanguageId
import com.serenity.rope.Rope

/** Reads how a document is indented from its own text: tabs or spaces by which starts more lines, and the width from
  * the step between neighbouring indented lines. Like VS Code, it only looks at the start of the file, ignores blank
  * lines, and says nothing (an empty hint) when the text is not indented.
  */
object IndentationDetector:

  val SampleLines: Int = 10000

  // The widest step between two lines that is taken to be one level; a bigger jump is a dedent across levels.
  private val MaxStep = 8

  // Wins a tie between widths seen equally often: 4 and 2 are the common ones.
  private val WidthsByPreference = Vector(4, 2, 8, 3, 6, 5, 7)

  private val ListItemText = """(?:[-*+]|\d+[.)]) .*""".r

  def of(content: Rope, language: Option[LanguageId]): IndentationHint =
    detect(content.linesIteratorFrom(0).take(SampleLines).map(_._2), language.contains(LanguageId.Markdown))

  /** In `markdown`, text indented only to line up under a list item says nothing about the document's indentation;
    * nested list items and tabs do.
    */
  def detect(lines: Iterator[String], markdown: Boolean): IndentationHint =
    lines.foldLeft(Tally.Empty)(_.reading(_, markdown)).verdict

  final private case class Tally(
      tabLines: Int,
      spaceLines: Int,
      stepCounts: Vector[Int],
      previousSpaces: Option[Int],
      inListItem: Boolean
  ):

    def reading(line: String, markdown: Boolean): Tally =
      val width = line.indexWhere(char => char != ' ' && char != '\t') match
        case -1    => line.length
        case found => found
      if width == line.length then this
      else if markdown then readingMarkdown(line, width)
      else if continuesBlockComment(line, width) then this
      else counted(line, width)

    private def readingMarkdown(line: String, width: Int): Tally =
      if ListItemText.matches(line.substring(width)) then copy(inListItem = true).counted(line, width)
      else if width > 0 && inListItem then this
      else copy(inListItem = false).counted(line, width)

    private def counted(line: String, width: Int): Tally =
      if width == 0 then copy(previousSpaces = Some(0))
      else if line.charAt(0) == '\t' then copy(tabLines = tabLines + 1, previousSpaces = None)
      else
        val spaces = line.indexWhere(_ != ' ')
        val steps = previousSpaces
          .map(previous => (spaces - previous).abs)
          .filter(step => step >= 1 && step <= MaxStep)
          .fold(stepCounts)(step => stepCounts.updated(step, stepCounts(step) + 1))
        copy(spaceLines = spaceLines + 1, stepCounts = steps, previousSpaces = Some(spaces))

    def verdict: IndentationHint =
      if tabLines == 0 && spaceLines == 0 then IndentationHint.Empty
      else if tabLines > spaceLines then IndentationHint(insertSpaces = Some(false))
      else IndentationHint(insertSpaces = Some(true), tabSize = mostCommonWidth)

    private def mostCommonWidth: Option[Int] =
      WidthsByPreference.filter(stepCounts(_) > 0).maxByOption(stepCounts(_))

  private object Tally:
    val Empty: Tally = Tally(0, 0, Vector.fill(MaxStep + 1)(0), None, inListItem = false)

  // ` * text` and ` */` sit one column in from the code they belong to, which would read as a one-space step.
  private def continuesBlockComment(line: String, width: Int): Boolean =
    line.charAt(width) == '*' &&
      (width + 1 == line.length || line.charAt(width + 1) == ' ' || line.charAt(width + 1) == '/')
