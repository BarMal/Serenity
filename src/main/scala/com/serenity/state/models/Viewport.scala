package com.serenity.state.models

case class Viewport(
    topLine: Int = 0,
    leftColumn: Int = 0,
    visibleLines: Int,
    visibleColumns: Int
)

object Viewport:

  def default: Viewport =
    Viewport(
      topLine = 0,
      leftColumn = 0,
      visibleLines = 24,
      visibleColumns = 80
    )
