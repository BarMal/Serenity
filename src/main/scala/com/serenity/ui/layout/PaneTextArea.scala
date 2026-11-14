package com.serenity.ui.layout

import com.serenity.config.TextAreaInsets
import com.serenity.state.models.*

/** Where one pane's `[left spacer][left rail][text][right rail][right spacer]` fall inside its pane rect. Spacers,
  * rails and text are sized from that pane's own width and buffer (#2570), so panes of different widths or documents of
  * different lengths each get their own.
  */
object PaneTextArea:

  /** A horizontal extent in cells. */
  final case class Span(x: Int, width: Int):
    def right: Int = x + width

  final case class Bands(
      leftSpacer: Span,
      leftRail: Option[Span],
      text: Span,
      rightRail: Option[Span],
      rightSpacer: Span
  )

  private val DefaultLineCount = 10

  private def columnModeActive(state: AppState): Boolean =
    state.persisted.config.surfaceConfig.columnModeEnabled && state.persisted.config.surfaceConfig.wordWrapEnabled

  private def railsOn(state: AppState): Boolean = state.chrome.lineNumbers && !columnModeActive(state)

  /** The rail's width in cells for a document of the pane's buffer: its digits plus a spacer cell, at least three. */
  def counterWidth(state: AppState, paneId: PaneId): Int =
    val lines = state.persisted.layout.editorPanes
      .get(paneId)
      .flatMap(_.bufferId)
      .flatMap(state.persisted.buffers.get)
      .map(_.document.content.lineCount)
      .getOrElse(DefaultLineCount)
    math.max(3, lines.toString.length + 1)

  /** Every editor pane's rail width, in pane order, for keying layouts that depend on them. */
  def counterWidths(state: AppState): List[Int] =
    if railsOn(state) then state.persisted.layout.editorPanes.keys.toList.sortBy(_.value).map(counterWidth(state, _))
    else Nil

  private def blocks(state: AppState, paneId: PaneId): (Int, Int, Int) =
    if !railsOn(state) then (0, 0, 0)
    else
      val lineNumberLayout = state.persisted.config.surfaceConfig.lineNumberLayout.normalized
      val counter          = counterWidth(state, paneId)
      val padding          = state.effectiveLineNumberPadding
      val left =
        state.effectiveLineNumberMarginLeft + (if lineNumberLayout.side.showsLeft then counter + padding else 0)
      val right = lineNumberLayout.marginRight + (if lineNumberLayout.side.showsRight then counter + padding else 0)
      (counter, left, right)

  def bands(state: AppState, paneId: PaneId, paneX: Int, paneWidth: Int, insets: TextAreaInsets): Bands =
    val (counter, leftBlock, rightBlock) = blocks(state, paneId)
    val lineNumberLayout                 = state.persisted.config.surfaceConfig.lineNumberLayout.normalized
    val textArea = TextArea.horizontal(
      paneWidth,
      insets,
      leftBlock,
      rightBlock,
      ProseColumn.widthCells(state),
      ProseColumn.chInCells(state)
    )
    val leftSpacer = Span(paneX, textArea.leftSpacer)
    val textX      = leftSpacer.right + leftBlock
    val text       = Span(textX, textArea.width)
    val leftRail =
      Option.when(railsOn(state) && lineNumberLayout.side.showsLeft)(
        Span(leftSpacer.right + state.effectiveLineNumberMarginLeft, counter)
      )
    val rightRail =
      Option.when(railsOn(state) && lineNumberLayout.side.showsRight)(
        Span(text.right + state.effectiveLineNumberPadding, counter)
      )
    Bands(leftSpacer, leftRail, text, rightRail, Span(text.right + rightBlock, textArea.rightSpacer))

  /** The narrowest a pane can be and still give its text `minimumTextWidth` cells: the text plus the cells its rails
    * and fixed-count gutters take, divided by the share the percent gutters leave.
    */
  def minimumWidth(state: AppState, paneId: PaneId, insets: TextAreaInsets, minimumTextWidth: Int): Int =
    val (_, leftBlock, rightBlock) = blocks(state, paneId)
    val (share, cells)             = TextArea.reservation(insets, ProseColumn.chInCells(state))
    val textFraction               = (1.0 - share).max(0.01)
    math.ceil((minimumTextWidth.max(1) + leftBlock + rightBlock + cells) / textFraction).toInt

end PaneTextArea
