package com.serenity.ui.layout

/** Diffs two consecutive frames of one editor pane's text layout and reports which visible rows can no longer reuse the
  * pixels an earlier frame left on the surface.
  *
  * The function is deliberately pure data in, plain row indices out: it has no knowledge of Java2D, surfaces or the
  * renderer, so the rule that governs screen correctness can be tested with fixtures alone.
  *
  * The bias is always towards reporting too much. Redrawing a row that did not change is invisible; skipping a row that
  * did change leaves stale pixels on screen. Every whole-snapshot property that shifts row geometry (panel width, row
  * height, ascent, proportional/measured layout mode, the rich-text document that drives styling) therefore invalidates
  * every row rather than being reasoned about per row.
  */
object DirtyLineDiff:

  /** Rows of `current` whose pixels may differ from `previous`, comparing text layout only.
    *
    * A row is dirty when the previous frame had no row at that index, when its [[TextVisualLine]] differs in any field,
    * or when a snapshot-wide property changed. `None` for `previous` means there is no reusable prior frame, so every
    * row is dirty.
    */
  def dirtyRows(previous: Option[TextLayoutSnapshot], current: TextLayoutSnapshot): Set[Int] =
    previous match
      case None => allRows(current)
      case Some(previousSnapshot) =>
        if !comparableShape(previousSnapshot, current) then allRows(current)
        else
          current.visualLines.indices.filter { row =>
            !previousSnapshot.visualLines.lift(row).contains(current.visualLines(row))
          }.toSet

  /** As [[dirtyRows]], additionally comparing a caller-supplied style key per row.
    *
    * The style key carries everything outside the text layout that feeds a row's pixels — selection spans, cell
    * animations, cursors drawn onto this surface, comment highlights, focused-body dimming. Keys are matched positional
    * by row index; if either key vector does not line up one-to-one with its snapshot's rows the diff gives up and
    * marks every row dirty.
    */
  def dirtyRows[K](
    previous: Option[TextLayoutSnapshot],
    current: TextLayoutSnapshot,
    previousRowKeys: Vector[K],
    currentRowKeys: Vector[K]
  ): Set[Int] =
    val layoutDirty = dirtyRows(previous, current)
    previous match
      case None => layoutDirty
      case Some(previousSnapshot) =>
        val alignedKeys =
          previousRowKeys.length == previousSnapshot.visualLines.length &&
            currentRowKeys.length == current.visualLines.length
        if !alignedKeys then allRows(current)
        else
          layoutDirty ++ current.visualLines.indices.filter { row =>
            !previousRowKeys.lift(row).contains(currentRowKeys(row))
          }
    end match

  /** Grow a dirty set by one row in each direction, bounded to `rowCount`.
    *
    * Glyphs are clipped to the surface grid rather than to their own row, so an ascender or descender on a redrawn row
    * can paint a few pixels into the band of the row above or below. Dilating the dirty set means those neighbours are
    * redrawn too, instead of keeping a preserved band that the neighbour's overflow no longer reaches.
    */
  def dilate(rows: Set[Int], rowCount: Int): Set[Int] =
    if rows.isEmpty then Set.empty
    else rows.flatMap(row => Set(row - 1, row, row + 1)).filter(row => row >= 0 && row < rowCount)

  private def allRows(snapshot: TextLayoutSnapshot): Set[Int] =
    snapshot.visualLines.indices.toSet

  private def comparableShape(previous: TextLayoutSnapshot, current: TextLayoutSnapshot): Boolean =
    previous.panelWidthPx == current.panelWidthPx &&
      previous.lineHeightPx == current.lineHeightPx &&
      previous.ascentPx == current.ascentPx &&
      previous.isProportional == current.isProportional &&
      previous.usesMeasuredLayout == current.usesMeasuredLayout &&
      previous.richTextDocument == current.richTextDocument
