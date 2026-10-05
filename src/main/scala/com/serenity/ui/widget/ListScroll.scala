package com.serenity.ui.widget

/** Where a list panel is scrolled to: the shared scroll model behind the explorer, outline, comments and diagnostics.
  *
  * The offset is stored, not derived from the selection, so the wheel can browse a list without moving its highlight.
  * While `followsSelection`, what is shown moves only as far as keeping the selection in view needs: a keyboard move
  * sets it, the wheel clears it. Following is resolved where the viewport is measured, so a selection made without one
  * -- revealing a file in the explorer, an outline tracking the cursor -- still comes into view when painted.
  */
final case class ListScroll(offset: Int = 0, followsSelection: Boolean = true):

  /** The first of `itemCount` items a viewport of `viewportRows` shows, with `selected` highlighted. */
  def shownOffset(itemCount: Int, selected: Option[Int], viewportRows: Int): Int =
    val anchored = selected.filter(_ => followsSelection).fold(offset)(ListScroll.revealed(offset, _, viewportRows))
    ListScroll.clamped(anchored, itemCount, viewportRows)

  /** The indexes of the items shown, in order. */
  def window(itemCount: Int, selected: Option[Int], viewportRows: Int): Range =
    val first = shownOffset(itemCount, selected, viewportRows)
    first until (first + math.max(1, viewportRows)).min(itemCount)

  /** Scrolled `lines` (positive towards the end) from what is shown, leaving the selection out of view if it goes. */
  def scrolledBy(lines: Int, itemCount: Int, selected: Option[Int], viewportRows: Int): ListScroll =
    val from = shownOffset(itemCount, selected, viewportRows)
    ListScroll(ListScroll.clamped(from + lines, itemCount, viewportRows), followsSelection = false)

  /** Following the selection again from what is shown, as a keyboard move does. */
  def revealing(itemCount: Int, selected: Option[Int], viewportRows: Int): ListScroll =
    ListScroll(copy(followsSelection = true).shownOffset(itemCount, selected, viewportRows))

object ListScroll:

  /** `offset` moved only as far as showing `index` in `viewportRows` needs. */
  def revealed(offset: Int, index: Int, viewportRows: Int): Int =
    val rows = math.max(1, viewportRows)
    if index < offset then index
    else if index >= offset + rows then index - rows + 1
    else offset

  /** `offset` kept within the list: never past the last full page, never before the first row. */
  def clamped(offset: Int, itemCount: Int, viewportRows: Int): Int =
    offset.min(itemCount - math.max(1, viewportRows)).max(0)
