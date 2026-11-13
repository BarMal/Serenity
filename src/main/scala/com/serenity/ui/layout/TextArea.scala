package com.serenity.ui.layout

import com.serenity.config.TextAreaInsets

/** How a workspace's width divides between the gutters and the text: the spacers the insets ask for (or the prose
  * column's centring asks for) and the text that is left.
  */
final case class TextArea(leftSpacer: Int, rightSpacer: Int, width: Int)

object TextArea:

  /** The text of a workspace `workspaceWidth` wide, after the line-number blocks it reserves on each side. A column of
    * `columnCells` is centred in what the blocks leave, but no gutter is narrower than its inset; the text is never
    * less than one cell.
    */
  def horizontal(
    workspaceWidth: Int,
    insets: TextAreaInsets,
    leftBlock: Int,
    rightBlock: Int,
    columnCells: Option[Int]
  ): TextArea =
    val (leftSpacer, rightSpacer) = ProseColumn.spacers(
      workspaceWidth - leftBlock - rightBlock,
      (workspaceWidth * insets.left).toInt,
      (workspaceWidth * insets.right).toInt,
      columnCells
    )
    TextArea(leftSpacer, rightSpacer, math.max(1, workspaceWidth - leftSpacer - rightSpacer - leftBlock - rightBlock))

  /** The top and bottom spacers of `contentHeight` rows. */
  def spacerHeights(contentHeight: Int, insets: TextAreaInsets): (Int, Int) =
    ((contentHeight * insets.top).toInt, (contentHeight * insets.bottom).toInt)
