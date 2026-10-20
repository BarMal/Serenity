package com.serenity.ui.layout

/** Where a tab character ends on a cell grid. Painting, wrapping and caret placement all take a tab's width from here,
  * so a tab is as wide to the wrap as it is on screen. Tab stops count from the start of the row being painted.
  */
object TabStops:

  /** The cells a tab at cell `column` spans: up to the next multiple of `tabSize`, so a tab on a stop spans a full
    * `tabSize`.
    */
  def cellsAt(column: Int, tabSize: Int): Int =
    val size = tabSize.max(1)
    size - column % size

  /** The cells `codePoint` occupies at cell `column`: a tab spans to the next stop, anything else its display width. */
  def advance(codePoint: Int, column: Int, tabSize: Int): Int =
    if codePoint == '\t' then cellsAt(column, tabSize) else CharWidth.of(codePoint)
