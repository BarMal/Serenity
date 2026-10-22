package com.serenity.ui.renderer

import com.serenity.ui.layout.CharWidth

/** A string laid out on a character-cell grid: how many cells it takes and which stretches of it share a baseline x. A
  * narrow run is drawn as one string so the font's own shaping applies; a two-cell cluster (a wide character, an emoji
  * sequence) is drawn alone, with whatever extends it, on the cell its caret stop is, because its glyph's own advance
  * need not be two cells. How many cells a cluster takes is [[CharWidth.ofCluster]]'s answer, here as in the caret
  * stops and the terminal; a cluster is [[CharWidth.clusterEnd]]'s, which is also where the carets stop.
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
    else
      @annotation.tailrec
      def sum(index: Int, cells: Int): Int =
        if index >= text.length then cells
        else
          val end = CharWidth.clusterEnd(text, index)
          sum(end, cells + CharWidth.ofCluster(text, index, end))
      sum(0, 0)

  /** `text` cut into `(cell offset, piece)` where every two-cell cluster is a piece of its own and the narrow stretches
    * between are kept whole.
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
          val end   = CharWidth.clusterEnd(text, index)
          val cells = CharWidth.ofCluster(text, index, end)
          if cells == 2 then walk(end, cell + 2, -1, 0, closed :+ ((cell, text.substring(index, end))))
          else if openFrom >= 0 && openFrom < index then walk(end, cell + cells, openFrom, openCell, found)
          else walk(end, cell + cells, index, cell, found)
      walk(0, 0, -1, 0, Vector.empty)

  /** `text` as one string per grapheme cluster, which is what a fallback font draws on its own cells: a mark or an
    * emoji extension drawn apart from its base would land on the cell after it. Marks with no base start a unit of
    * their own.
    */
  def clusters(text: String): Vector[String] =
    @annotation.tailrec
    def walk(index: Int, found: Vector[String]): Vector[String] =
      if index >= text.length then found
      else
        val end = CharWidth.clusterEnd(text, index)
        walk(end, found :+ text.substring(index, end))
    walk(0, Vector.empty)
