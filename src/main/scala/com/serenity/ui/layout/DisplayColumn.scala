package com.serenity.ui.layout

import scala.annotation.tailrec

/** The visual column of a cursor: the cells before it on its line, a tab taken to the next tab stop and every other
  * grapheme cluster at the cells it occupies ([[CharWidth.ofCluster]]). Stored columns are UTF-16 indexes and say
  * nothing of this (docs/text-indexing.md).
  */
object DisplayColumn:

  /** The cells before UTF-16 column `utf16Column` of `line`. A column past the end counts the whole line; one inside a
    * cluster counts only the clusters before it, since the caret cannot stand between a cluster's parts.
    */
  def of(line: String, utf16Column: Int, tabSize: Int): Int =
    val limit = utf16Column.max(0).min(line.length)

    @tailrec def walk(index: Int, cells: Int): Int =
      if index >= limit then cells
      else
        val end = CharWidth.clusterEnd(line, index)
        if end > limit then cells
        else
          val width =
            if line.charAt(index) == '\t' then TabStops.cellsAt(cells, tabSize)
            else CharWidth.ofCluster(line, index, end)
          walk(end, cells + width)

    walk(0, 0)
