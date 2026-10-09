package com.serenity.state.models

final case class Viewport(
    topLine: Int = 0,
    leftColumn: Int = 0,
    visibleLines: Int,
    visibleColumns: Int,
    topVisualLine: Int = 0,
    placement: ViewportPlacement = ViewportPlacement.Placed
):

  /** An explicit scroll: wherever the caret is, this is where the viewport now rests. */
  def scrolledTo(topLine: Int, leftColumn: Int, topVisualLine: Int): Viewport =
    copy(
      topLine = topLine,
      leftColumn = leftColumn,
      topVisualLine = topVisualLine,
      placement = ViewportPlacement.Placed
    )

object Viewport:

  def default: Viewport =
    Viewport(
      topLine = 0,
      leftColumn = 0,
      visibleLines = 24,
      visibleColumns = 80,
      topVisualLine = 0
    )
