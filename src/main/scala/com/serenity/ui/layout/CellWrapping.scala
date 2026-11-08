package com.serenity.ui.layout

import scala.annotation.tailrec

/** Word wrapping that needs nothing but how wide a row of text is, so a model file can wrap a message without reaching
  * for fonts or the renderer. [[TextWrapping]] adds the pixel measure of a GUI font on top of the same algorithm.
  */
object CellWrapping:

  /** `text` wrapped to `widthCells` terminal cells (at least one). */
  def wrapCells(text: String, widthCells: Int): List[String] =
    wrap(text, fitsCells(math.max(1, widthCells)))

  /** Whether a row is at most `widthCells` terminal cells wide. */
  def fitsCells(widthCells: Int): String => Boolean =
    row => cellsOf(row) <= widthCells

  /** `text` broken into rows that `fits`, at spaces where it can and mid-word where a word alone is too long. A
    * paragraph's leading spaces indent every row it wraps to; an empty text is one empty row.
    */
  def wrap(text: String, fits: String => Boolean): List[String] =
    val indent = text.takeWhile(_ == ' ')
    val prefix = if fits(indent + "x") then indent else ""
    val rows = text
      .split(' ')
      .filter(_.nonEmpty)
      .foldLeft(List.empty[String]) { (rows, word) =>
        rows match
          case current :: earlier if fits(s"$current $word") =>
            s"$current $word" :: earlier
          case _ => breakToFit(word, prefix, fits).reverse ::: rows
      }
      .reverse
    if rows.isEmpty then List("") else rows

  private def cellsOf(row: String): Int =
    @tailrec
    def sum(index: Int, cells: Int): Int =
      if index >= row.length then cells
      else
        val end = CharWidth.clusterEnd(row, index)
        sum(end, cells + CharWidth.ofCluster(row, index, end))
    if row.forall(_ < NarrowBelow) then row.length else sum(0, 0)

  // Nothing below U+0300 is wide, zero-width or extends a cluster, so the common case never reaches the ICU lookups.
  private val NarrowBelow = 0x0300

  /** `word` as rows, each starting with `prefix` and as long as still fits -- one grapheme cluster at least, so a row
    * too narrow for any glyph still makes progress, and a row never ends inside a cluster.
    */
  private def breakToFit(word: String, prefix: String, fits: String => Boolean): List[String] =
    @tailrec
    def fittingEnd(rest: String, end: Int): Int =
      if end >= rest.length then end
      else
        val next = CharWidth.clusterEnd(rest, end)
        if fits(prefix + rest.substring(0, next)) then fittingEnd(rest, next) else end
    @tailrec
    def pieces(rest: String, done: List[String]): List[String] =
      if rest.isEmpty then done.reverse
      else
        val end = math.max(CharWidth.clusterEnd(rest, 0), fittingEnd(rest, 0))
        pieces(rest.substring(end), (prefix + rest.substring(0, end)) :: done)
    pieces(word, Nil)
