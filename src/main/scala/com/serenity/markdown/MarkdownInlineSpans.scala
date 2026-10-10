package com.serenity.markdown

import scala.annotation.tailrec

/** The emphasis and code-span styling on one line of Markdown, as stretches of the line's own columns. Only the inline
  * constructs a live preview restyles are found: strong and emphasised text (`*` and `_`) and code spans. Headings,
  * list markers, quotes, links and block structure belong to the block parser, and a line is scanned alone, so emphasis
  * never spans lines.
  *
  * Delimiter matching follows CommonMark's flanking rules and its delimiter-stack algorithm, so what is hidden is what
  * a Markdown renderer would not print: `2 * 3 * 4`, `snake_case` and `\*escaped\*` stay literal.
  */
object MarkdownInlineSpans:

  /** `scale` is the text's size over the base font's, `muted` paints it in the theme's muted colour. */
  final case class Style(
      bold: Boolean,
      italic: Boolean,
      code: Boolean,
      muted: Boolean = false,
      scale: Float = 1.0f,
      strike: Boolean = false
  )

  /** `[startColumn, endColumn)` of a line drawn in one `style`. A marker run is the delimiter characters themselves,
    * which a live preview hides; its `style` is always plain.
    */
  final case class Run(startColumn: Int, endColumn: Int, style: Style, isMarker: Boolean)

  private val AsciiSymbols = "$+<=>^`|~"

  private val Bold   = 1
  private val Italic = 2
  private val Code   = 4
  private val Marker = 8
  private val Strike = 16

  final private case class Delimiter(
      char: Char,
      start: Int,
      original: Int,
      remaining: Int,
      canOpen: Boolean,
      canClose: Boolean
  ):
    def consumed: Int = original - remaining

  /** Whether `line` holds a character any construct this object finds starts with. A line that does not has no runs,
    * which lets layout skip scanning almost every line of prose.
    */
  def mayContainMarkup(line: String): Boolean =
    @tailrec
    def from(index: Int): Boolean =
      if index >= line.length then false
      else
        val char = line.charAt(index)
        isDelimiterChar(char) || char == '`' || from(index + 1)
    from(0)

  def scan(line: String): Vector[Run] =
    if !mayContainMarkup(line) then Vector.empty
    else
      val flags = new Array[Int](line.length)
      markCodeSpans(line, flags)
      markEmphasis(line, flags)
      runsOf(flags)

  /** The columns a live preview hides when it hides markers, in ascending order. */
  def hiddenColumns(runs: Vector[Run]): Vector[Int] =
    runs.filter(_.isMarker).flatMap(run => run.startColumn until run.endColumn)

  private def isDelimiterChar(char: Char): Boolean = char == '*' || char == '_' || char == '~'

  private def addFlag(flags: Array[Int], from: Int, until: Int, flag: Int): Unit =
    (from until until).foreach(index => flags(index) = flags(index) | flag)

  @tailrec
  private def runLength(line: String, from: Int, char: Char, length: Int = 0): Int =
    if from + length < line.length && line.charAt(from + length) == char then runLength(line, from, char, length + 1)
    else length

  private def markCodeSpans(line: String, flags: Array[Int]): Unit =
    @tailrec
    def from(index: Int): Unit =
      if index < line.length then
        line.charAt(index) match
          case '\\' => from(index + 2)
          case '`' =>
            val fence = runLength(line, index, '`')
            closingFence(line, index + fence, fence) match
              case Some(close) =>
                addFlag(flags, index, index + fence, Marker)
                addFlag(flags, index + fence, close, Code)
                addFlag(flags, close, close + fence, Marker)
                from(close + fence)
              case None => from(index + fence)
          case _ => from(index + 1)
    from(0)

  @tailrec
  private def closingFence(line: String, from: Int, fence: Int): Option[Int] =
    if from >= line.length then None
    else if line.charAt(from) != '`' then closingFence(line, from + 1, fence)
    else
      val length = runLength(line, from, '`')
      if length == fence then Some(from) else closingFence(line, from + length, fence)

  private def markEmphasis(line: String, flags: Array[Int]): Unit =
    val delimiters = delimiterRuns(line, flags)
    if delimiters.nonEmpty then
      val _ = delimiters.foldLeft(Vector.empty[Delimiter]) { (openers, delimiter) =>
        val (leftover, remaining) =
          if delimiter.canClose then close(delimiter, openers, flags) else (delimiter, openers)
        if leftover.remaining > 0 && leftover.canOpen then remaining :+ leftover else remaining
      }

  private def delimiterRuns(line: String, flags: Array[Int]): Vector[Delimiter] =
    @tailrec
    def from(index: Int, found: Vector[Delimiter]): Vector[Delimiter] =
      if index >= line.length then found
      else if flags(index) != 0 then from(index + 1, found)
      else
        val char = line.charAt(index)
        if char == '\\' then from(index + 2, found)
        else if isDelimiterChar(char) then
          val length = runLength(line, index, char)
          // Only a run of exactly two tildes strikes text through.
          if char == '~' && length != 2 then from(index + length, found)
          else from(index + length, found :+ delimiterAt(line, index, length, char))
        else from(index + 1, found)
    from(0, Vector.empty)

  private def delimiterAt(line: String, start: Int, length: Int, char: Char): Delimiter =
    val before = if start == 0 then ' '.toInt else line.codePointBefore(start)
    val after  = if start + length >= line.length then ' '.toInt else line.codePointAt(start + length)
    val left   = !isWhitespace(after) && (!isPunctuation(after) || isWhitespace(before) || isPunctuation(before))
    val right  = !isWhitespace(before) && (!isPunctuation(before) || isWhitespace(after) || isPunctuation(after))
    if char == '*' || char == '~' then Delimiter(char, start, length, length, canOpen = left, canClose = right)
    else
      Delimiter(
        char,
        start,
        length,
        length,
        canOpen = left && (!right || isPunctuation(before)),
        canClose = right && (!left || isPunctuation(after))
      )

  private def isWhitespace(codePoint: Int): Boolean =
    Character.isWhitespace(codePoint) || Character.isSpaceChar(codePoint)

  private def isPunctuation(codePoint: Int): Boolean =
    val category = Character.getType(codePoint)
    category == Character.CONNECTOR_PUNCTUATION || category == Character.DASH_PUNCTUATION ||
    category == Character.START_PUNCTUATION || category == Character.END_PUNCTUATION ||
    category == Character.INITIAL_QUOTE_PUNCTUATION || category == Character.FINAL_QUOTE_PUNCTUATION ||
    category == Character.OTHER_PUNCTUATION || AsciiSymbols.indexOf(codePoint) >= 0

  /** Pairs `closer` with the nearest opener below it on `openers` for as long as it can, marking the delimiters used
    * and styling the text between them. Returns what is left of the closer and the openers still open: the openers
    * above a match can no longer close, so they are dropped.
    */
  @tailrec
  private def close(closer: Delimiter, openers: Vector[Delimiter], flags: Array[Int]): (Delimiter, Vector[Delimiter]) =
    val matching = openers.lastIndexWhere(opener => pairs(opener, closer))
    if closer.remaining == 0 || !closer.canClose || matching < 0 then (closer, openers)
    else
      val opener      = openers(matching)
      val used        = if opener.remaining >= 2 && closer.remaining >= 2 then 2 else 1
      val openerStart = opener.start + opener.remaining - used
      val closerStart = closer.start + closer.consumed
      addFlag(flags, openerStart, openerStart + used, Marker)
      addFlag(flags, closerStart, closerStart + used, Marker)
      addFlag(
        flags,
        openerStart + used,
        closerStart,
        if closer.char == '~' then Strike else if used == 2 then Bold else Italic
      )
      val stillOpen = opener.copy(remaining = opener.remaining - used)
      val below     = openers.take(matching)
      close(
        closer.copy(remaining = closer.remaining - used),
        if stillOpen.remaining > 0 then below :+ stillOpen else below,
        flags
      )

  /** CommonMark's "rule of 3": a delimiter that can both open and close only pairs across a total length that is not a
    * multiple of 3, unless both lengths are.
    */
  private def pairs(opener: Delimiter, closer: Delimiter): Boolean =
    opener.char == closer.char && opener.canOpen && opener.remaining > 0 &&
      !((opener.canClose || closer.canOpen) &&
        (opener.original + closer.original) % 3 == 0 &&
        !(opener.original % 3 == 0 && closer.original % 3 == 0))

  private def runsOf(flags: Array[Int]): Vector[Run] =
    def normalised(flag: Int): Int = if (flag & Marker) != 0 then Marker else flag

    @tailrec
    def runEnd(from: Int, flag: Int): Int =
      if from < flags.length && normalised(flags(from)) == flag then runEnd(from + 1, flag) else from

    @tailrec
    def collect(index: Int, found: Vector[Run]): Vector[Run] =
      if index >= flags.length then found
      else
        val flag = normalised(flags(index))
        if flag == 0 then collect(index + 1, found)
        else
          val end   = runEnd(index, flag)
          val style = Style((flag & Bold) != 0, (flag & Italic) != 0, (flag & Code) != 0, strike = (flag & Strike) != 0)
          collect(end, found :+ Run(index, end, style, isMarker = (flag & Marker) != 0))
    collect(0, Vector.empty)
