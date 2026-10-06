package com.serenity.state.models

import scala.util.boundary
import scala.util.boundary.break

/** Commit-time comparisons of two buffer maps that skip every buffer both maps hold as the same object, and allocate
  * nothing per buffer (keyed generically so an id is never re-boxed), so an edit to one buffer costs the same with 1 or
  * 200 others open.
  */
object BufferMapChanges:

  /** Whether `after` holds a buffer that `added` accepts because `before` has none under its id, or that `changed`
    * accepts as a replacement for the different object `before` holds under that id.
    */
  def anyChanged[K](before: Map[K, Buffer], after: Map[K, Buffer])(
    added: Buffer => Boolean,
    changed: (Buffer, Buffer) => Boolean
  ): Boolean =
    (before ne after) && boundary:
      after.foreachEntry { (id, buffer) =>
        if before.contains(id) then
          val previous = before(id)
          if (previous ne buffer) && changed(previous, buffer) then break(true)
        else if added(buffer) then break(true)
      }
      false
