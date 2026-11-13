package com.serenity.ui.layout

import com.serenity.config.{Gutter, TextAreaInsets}

/** How a workspace's width divides between the gutters (the spacers the insets ask for, or the prose column's centring
  * asks for) and the text that is left.
  */
final case class TextArea(leftSpacer: Int, rightSpacer: Int, width: Int)

object TextArea:

  /** The text of a workspace `workspaceWidth` wide, after the line-number blocks it reserves on each side.
    *
    * A gutter that is a share, a count of cells or a count of ch (`chInCells` of the prose font's `0` advance to a grid
    * cell, taken only when one is asked for) keeps the rule it had: a column of `columnCells` is centred in what the
    * blocks leave, and no gutter is narrower than it asked. An `auto` gutter takes whatever the column and the other
    * gutter leave, both together centring the column, and is nothing when there is no column. The text is never less
    * than one cell.
    */
  def horizontal(
    workspaceWidth: Int,
    insets: TextAreaInsets,
    leftBlock: Int,
    rightBlock: Int,
    columnCells: Option[Int],
    chInCells: => Double = 1.0
  ): TextArea =
    val room                       = workspaceWidth - leftBlock - rightBlock
    def cells(gutter: Gutter): Int = resolved(gutter, workspaceWidth, chInCells)
    val (leftSpacer, rightSpacer) =
      (insets.left, insets.right) match
        case (Gutter.Auto, Gutter.Auto) => ProseColumn.spacers(room, 0, 0, columnCells)
        case (Gutter.Auto, fixed) =>
          val right = cells(fixed)
          (spare(room, right, columnCells), right)
        case (fixed, Gutter.Auto) =>
          val left = cells(fixed)
          (left, spare(room, left, columnCells))
        case (left, right) => ProseColumn.spacers(room, cells(left), cells(right), columnCells)
    TextArea(leftSpacer, rightSpacer, math.max(1, workspaceWidth - leftSpacer - rightSpacer - leftBlock - rightBlock))

  /** The top and bottom spacers of `contentHeight` rows. A share is of the height, rounded down; a count is rows. */
  def spacerHeights(contentHeight: Int, insets: TextAreaInsets): (Int, Int) =
    (vertical(insets.top, contentHeight), vertical(insets.bottom, contentHeight))

  /** Where a gutter of the insets is a share, the part of the width that varies with it; what it takes in cells. The
    * rest is a fixed number of cells, so the width a pane must have for its text is the text plus the cells, divided by
    * what the shares leave.
    */
  def reservation(insets: TextAreaInsets, chInCells: => Double): (Double, Int) =
    List(insets.left, insets.right).foldLeft((0.0, 0)) {
      case ((share, cells), Gutter.Percent(fraction)) => (share + fraction, cells)
      case ((share, cells), Gutter.Cells(count))      => (share, cells + count)
      case ((share, cells), Gutter.Ch(count))         => (share, cells + chCells(count, chInCells))
      case (total, Gutter.Auto)                       => total
    }

  private def spare(room: Int, other: Int, columnCells: Option[Int]): Int =
    columnCells.fold(0)(column => (room - other - column).max(0))

  private def resolved(gutter: Gutter, width: Int, chInCells: => Double): Int =
    gutter match
      case Gutter.Percent(share) => (width * share).toInt
      case Gutter.Cells(count)   => count
      case Gutter.Ch(count)      => chCells(count, chInCells)
      case Gutter.Auto           => 0

  private def vertical(gutter: Gutter, height: Int): Int =
    gutter match
      case Gutter.Percent(share) => (height * share).toInt
      case Gutter.Cells(count)   => count
      case Gutter.Ch(count)      => count
      case Gutter.Auto           => 0

  private def chCells(count: Int, chInCells: Double): Int =
    math.ceil(count * chInCells).toInt
