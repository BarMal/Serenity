package com.serenity.ui.renderer

import com.serenity.ui.layout.CharWidth

/** A string laid out on a character-cell grid: how many cells it takes and which stretches of it share a baseline x. A
  * narrow run is drawn as one string so the font's own shaping applies; a wide character is drawn alone, with the marks
  * on it, on the cell its caret stop is, because its glyph's own advance need not be two cells. How many cells a
  * character takes is [[CharWidth]]'s answer, here as in the caret stops and the terminal: a combining mark takes none.
  */
private[ui] object CellRuns:

  /** The first codepoint with a wide East Asian Width: text below it is all narrow, which lets a string with no wide
    * character skip the cut.
    */
  private val FirstWide = 0x1100

  /** The first combining mark: text below it takes one cell a character. */
  private val FirstMark = 0x0300

  def cellsIn(text: String): Int =
    if text.forall(_ < FirstMark) then text.length
    else text.codePoints().map(CharWidth.of).sum()

  /** `text` cut into `(cell offset, piece)` where every wide character, with the marks on it, is a piece of its own and
    * the narrow stretches between are kept whole.
    */
  def pieces(text: String): Vector[(Int, String)] =
    if text.forall(_ < FirstWide) then Vector((0, text))
    else
      @annotation.tailrec
      def walk(
        index: Int,
        cell: Int,
        openFrom: Int,
        openCell: Int,
        found: Vector[(Int, String)]
      ): Vector[(Int, String)] =
        def closed: Vector[(Int, String)] =
          if openFrom >= 0 && openFrom < index then found :+ ((openCell, text.substring(openFrom, index))) else found
        if index >= text.length then closed
        else
          val codePoint = text.codePointAt(index)
          val next      = index + Character.charCount(codePoint)
          val cells     = CharWidth.of(codePoint)
          if cells == 2 then
            val end = endOfMarks(text, next)
            walk(end, cell + 2, -1, 0, closed :+ ((cell, text.substring(index, end))))
          else if openFrom >= 0 && openFrom < index then walk(next, cell + cells, openFrom, openCell, found)
          else walk(next, cell + cells, index, cell, found)
      walk(0, 0, -1, 0, Vector.empty)

  /** `text` as one string per character with the marks that follow it, which is what a fallback font draws on its own
    * cell: a mark drawn apart from its base would land on the cell after it. Marks with no base start a unit of their
    * own.
    */
  def clusters(text: String): Vector[String] =
    @annotation.tailrec
    def walk(index: Int, found: Vector[String]): Vector[String] =
      if index >= text.length then found
      else
        val next = endOfMarks(text, index + Character.charCount(text.codePointAt(index)))
        walk(next, found :+ text.substring(index, next))
    walk(0, Vector.empty)

  /** The index after the run of combining marks that starts at `from`. */
  @annotation.tailrec
  def endOfMarks(text: String, from: Int): Int =
    if from < text.length && CharWidth.of(text.codePointAt(from)) == 0 then
      endOfMarks(text, from + Character.charCount(text.codePointAt(from)))
    else from
