package com.serenity.markdown

import scala.annotation.tailrec

import com.serenity.markdown.MarkdownBlockLens.{SetextRole, TableRow}
import com.serenity.markdown.MarkdownInlineSpans.{Run, Style}

/** The stretches of one line of Markdown a live preview restyles: the block syntax that opens or shapes the line --
  * heading hashes, list bullets, quote marks, a thematic break, table pipes -- together with the inline emphasis and
  * code of [[MarkdownInlineSpans]] inside it. A line is scanned alone; what lies outside it (fences, which row of a
  * table it is) comes from the block index.
  *
  * Hidden markers are marker runs; a bullet, a quote mark or a pipe is a styled content run, since it stays on screen.
  */
object MarkdownBlockSpans:

  private val HeadingScales = Vector(1.8f, 1.5f, 1.25f, 1.1f, 1.0f, 1.0f)

  private val Plain    = Style(bold = false, italic = false, code = false)
  private val Glyph    = Plain.copy(bold = true, muted = true)
  private val Dimmed   = Plain.copy(muted = true)
  private val Quoted   = Plain.copy(italic = true)
  private val Emphatic = Plain.copy(bold = true)

  /** The stretches of a line that stay as they are written, and the style given to the rest of it from `bodyFrom`. */
  final private case class Block(fixed: Vector[Run], body: Option[Style], bodyFrom: Int)

  def headingScale(level: Int): Float = HeadingScales.lift(level - 1).getOrElse(1.0f)

  /** Whether `line` holds anything [[scan]] might find, which lets layout skip prose without a closer look. */
  def mayContainMarkup(line: String): Boolean =
    MarkdownInlineSpans.mayContainMarkup(line) || line.indexOf('|') >= 0 || mayOpenBlock(line)

  def scan(line: String, table: TableRow = TableRow.None, setext: SetextRole = SetextRole.None): Vector[Run] =
    val inline = MarkdownInlineSpans.scan(line)
    blockOf(line, table, setext).fold(inline)(merged(_, inline, line.length))

  /** Whether a live preview draws `line` as a horizontal line when its text is hidden. */
  def isRule(line: String, table: TableRow, setext: SetextRole = SetextRole.None): Boolean =
    line.nonEmpty && (table == TableRow.Delimiter || setext == SetextRole.Underline ||
      (table == TableRow.None && setext == SetextRole.None && MarkdownBlockLens.isThematicBreak(line)))

  /** The columns of the unescaped pipes in `line`, ascending. */
  def pipeColumns(line: String): Vector[Int] =
    @tailrec
    def from(index: Int, found: Vector[Int]): Vector[Int] =
      if index >= line.length then found
      else
        line.charAt(index) match
          case '\\' => from(index + 2, found)
          case '|'  => from(index + 1, found :+ index)
          case _    => from(index + 1, found)
    from(0, Vector.empty)

  private def mayOpenBlock(line: String): Boolean =
    @tailrec
    def from(index: Int): Boolean =
      if index >= line.length then false
      else
        val char = line.charAt(index)
        if char == ' ' || char == '\t' then from(index + 1) else "#>-*+_0123456789".indexOf(char) >= 0
    from(0)

  private def hiddenLine(line: String): Option[Block] =
    Option.when(line.nonEmpty)(Block(Vector(Run(0, line.length, Plain, true)), None, 0))

  private def blockOf(line: String, table: TableRow, setext: SetextRole): Option[Block] =
    setext match
      case SetextRole.Underline   => hiddenLine(line)
      case SetextRole.Text(level) => Some(Block(Vector.empty, Some(headingStyle(level)), 0))
      case SetextRole.None =>
        table match
          case TableRow.Delimiter => hiddenLine(line)
          case TableRow.Header    => Some(tableRow(line, Some(Emphatic)))
          case TableRow.Body      => Some(tableRow(line, None))
          case TableRow.None =>
            if !mayOpenBlock(line) then None
            else if MarkdownBlockLens.isThematicBreak(line) then hiddenLine(line)
            else heading(line).orElse(listItem(line)).orElse(quote(line))

  private def headingStyle(level: Int): Style = Plain.copy(bold = true, scale = headingScale(level))

  private def isBlank(char: Char): Boolean = char == ' ' || char == '\t'

  @tailrec
  private def skipBlanks(line: String, from: Int): Int =
    if from < line.length && isBlank(line.charAt(from)) then skipBlanks(line, from + 1) else from

  @tailrec
  private def skipBack(line: String, until: Int, to: Int, char: Char => Boolean): Int =
    if until > to && char(line.charAt(until - 1)) then skipBack(line, until - 1, to, char) else until

  private def heading(line: String): Option[Block] =
    val indent = line.takeWhile(_ == ' ').length
    val hashes = line.drop(indent).takeWhile(_ == '#').length
    val after  = indent + hashes
    Option.when(indent <= 3 && hashes >= 1 && hashes <= 6 && after < line.length && isBlank(line.charAt(after))) {
      val textStart  = skipBlanks(line, after)
      val textEnd    = skipBack(line, line.length, textStart, isBlank)
      val closeStart = skipBack(line, textEnd, textStart, _ == '#')
      val hasCloser  = closeStart < textEnd && closeStart > textStart && isBlank(line.charAt(closeStart - 1))
      val bodyEnd    = if hasCloser then skipBack(line, closeStart, textStart, isBlank) else line.length
      val closing    = Option.when(hasCloser)(Run(bodyEnd, line.length, Plain, true))
      val style      = headingStyle(hashes)
      Block(Run(indent, textStart, Plain, true) +: closing.toVector, Some(style), textStart)
    }

  private def listItem(line: String): Option[Block] =
    val indent    = skipBlanks(line, 0)
    val markerEnd = listMarkerEnd(line, indent)
    Option.when(markerEnd > indent && markerEnd < line.length && isBlank(line.charAt(markerEnd))) {
      val textStart = skipBlanks(line, markerEnd)
      val marker    = Run(indent, markerEnd, Glyph, false)
      taskBox(line, textStart).fold(Block(Vector(marker), None, textStart)) {
        case (box, checked) =>
          Block(
            Vector(marker, box),
            Option.when(checked)(Dimmed),
            skipBlanks(line, box.endColumn)
          )
      }
    }

  private def listMarkerEnd(line: String, from: Int): Int =
    if from >= line.length then from
    else if "-*+".indexOf(line.charAt(from)) >= 0 then from + 1
    else
      val digits = line.drop(from).takeWhile(_.isDigit).length
      val end    = from + digits
      if digits >= 1 && digits <= 9 && end < line.length && (line.charAt(end) == '.' || line.charAt(end) == ')') then
        end + 1
      else from

  private def taskBox(line: String, from: Int): Option[(Run, Boolean)] =
    val end = from + 3
    Option.when(
      end <= line.length && line.charAt(from) == '[' && line.charAt(from + 2) == ']' &&
        " xX".indexOf(line.charAt(from + 1)) >= 0 && (end == line.length || isBlank(line.charAt(end)))
    )((Run(from, end, Glyph, false), line.charAt(from + 1) != ' '))

  private def quote(line: String): Option[Block] =
    val indent = line.takeWhile(_ == ' ').length

    @tailrec
    def marks(index: Int): Int =
      if index < line.length && line.charAt(index) == '>' then
        marks(if index + 1 < line.length && line.charAt(index + 1) == ' ' then index + 2 else index + 1)
      else index

    val end = marks(indent)
    Option.when(indent <= 3 && end > indent)(Block(Vector(Run(indent, end, Glyph, false)), Some(Quoted), end))

  private def tableRow(line: String, body: Option[Style]): Block =
    Block(pipeColumns(line).map(index => Run(index, index + 1, Dimmed, false)), body, 0)

  private def merged(block: Block, inline: Vector[Run], length: Int): Vector[Run] =
    val pieces = freeStretches(block.fixed, block.bodyFrom, length).flatMap {
      case (from, until) => stretch(from, until, inline, block.body)
    }
    (block.fixed ++ pieces).sortBy(_.startColumn)

  private def freeStretches(fixed: Vector[Run], from: Int, until: Int): Vector[(Int, Int)] =
    val (last, gaps) = fixed.filter(_.endColumn > from).foldLeft((from, Vector.empty[(Int, Int)])) {
      case ((cursor, found), run) =>
        (
          math.max(cursor, run.endColumn),
          if run.startColumn > cursor then found :+ ((cursor, run.startColumn)) else found
        )
    }
    gaps ++ Option.when(last < until)((last, until))

  private def stretch(from: Int, until: Int, inline: Vector[Run], body: Option[Style]): Vector[Run] =
    val inside = inline
      .filter(run => run.endColumn > from && run.startColumn < until)
      .map(run => run.copy(startColumn = math.max(run.startColumn, from), endColumn = math.min(run.endColumn, until)))
    body.fold(inside) { style =>
      val styled = inside.map(run => if run.isMarker then run else run.copy(style = overlaid(style, run.style)))
      val (last, gaps) = inside.foldLeft((from, Vector.empty[Run])) {
        case ((cursor, found), run) =>
          (
            run.endColumn,
            if run.startColumn > cursor then found :+ Run(cursor, run.startColumn, style, false) else found
          )
      }
      styled ++ gaps ++ Option.when(last < until)(Run(last, until, style, false))
    }

  private def overlaid(base: Style, inline: Style): Style =
    Style(
      bold = base.bold || inline.bold,
      italic = base.italic || inline.italic,
      code = inline.code,
      muted = base.muted || inline.muted,
      scale = base.scale,
      strike = base.strike || inline.strike
    )
