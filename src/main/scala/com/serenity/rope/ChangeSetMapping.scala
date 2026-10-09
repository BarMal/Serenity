package com.serenity.rope

import scala.annotation.tailrec

/** Position mapping for [[ChangeSet]]. The per-mode rules are the ones the editor's annotation and cursor remapping has
  * always applied, so mapping a position through a change set agrees with remapping it edit by edit as long as the
  * edits are separated by at least one unchanged character. Touching edits are merged into one part here, so a position
  * on or inside the merged region follows the merged rule instead.
  */
private[rope] object ChangeSetMapping:

  def mapPos(changes: ChangeSet, position: Int, mode: MapMode): Int =
    mapAt(changes, position, mode, lastPartStartingAtOrBefore(changes.parts, position, 0, changes.parts.size))

  /** `positions` must be in ascending order. */
  def mapSorted(changes: ChangeSet, positions: IArray[Int], mode: MapMode): IArray[Int] =
    val mapped = new Array[Int](positions.length)

    @tailrec
    def loop(index: Int, part: Int): Unit =
      if index < positions.length then
        val position = positions(index)
        val current  = advance(changes.parts, position, part)
        mapped(index) = mapAt(changes, position, mode, current)
        loop(index + 1, current)

    loop(0, -1)
    IArray.unsafeFromArray(mapped)

  /** `part` is the index of the last part starting at or before `position`, or -1 when there is none. */
  private def mapAt(changes: ChangeSet, position: Int, mode: MapMode, part: Int): Int =
    if part < 0 then position
    else
      val edit = changes.parts(part)
      if position > edit.to then position + changes.shiftBefore(part + 1)
      else
        val start       = edit.from + changes.shiftBefore(part)
        val end         = start + edit.insert.length
        val isInsertion = edit.from == edit.to
        mode match
          case MapMode.Cursor      => end
          case MapMode.AnchorStart => if isInsertion then end else start
          case MapMode.AnchorEnd | MapMode.Point =>
            if !isInsertion && position == edit.to then end else start

  @tailrec
  private def advance(parts: Vector[Replacement], position: Int, part: Int): Int =
    if part + 1 < parts.size && parts(part + 1).from <= position then advance(parts, position, part + 1) else part

  /** Binary search over `[low, high)`; the parts are sorted and disjoint, so `from` is increasing. */
  @tailrec
  private def lastPartStartingAtOrBefore(parts: Vector[Replacement], position: Int, low: Int, high: Int): Int =
    if low >= high then low - 1
    else
      val middle = (low + high) >>> 1
      if parts(middle).from <= position then lastPartStartingAtOrBefore(parts, position, middle + 1, high)
      else lastPartStartingAtOrBefore(parts, position, low, middle)
