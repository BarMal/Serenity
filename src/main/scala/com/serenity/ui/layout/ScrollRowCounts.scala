package com.serenity.ui.layout

import com.serenity.state.models.BufferId

/** The three row counts one pane is scrolled and painted by (#2506): the grid the window reported, the rows the scroll
  * limit assumes fit, and the rows the renderer lays out. On a machine where they differ, the wheel stops short of the
  * end of a document or past it.
  */
final case class ScrollRowCounts(gridLines: Int, limitRows: Int, paintedLines: Int):

  def logLine(buffer: BufferId): String =
    s"[SCROLL-ROWS] buffer=${buffer.value} grid.visibleLines=$gridLines limit.rowsThatFit=$limitRows " +
      s"painted.visibleLines=$paintedLines verdict=$verdict"

  private def verdict: String =
    limitRows - paintedLines match
      case 0                      => "agree"
      case surplus if surplus > 0 => s"limit-over-by-$surplus"
      case shortfall              => s"limit-under-by-${-shortfall}"
