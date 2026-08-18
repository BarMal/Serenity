package com.serenity.state.models

final case class Viewport(
    topLine: Int = 0,
    leftColumn: Int = 0,
    visibleLines: Int,
    visibleColumns: Int,
    topVisualLine: Int = 0
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
