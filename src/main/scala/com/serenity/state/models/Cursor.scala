package com.serenity.state.models

import com.serenity.animation.Tween
import com.serenity.ui.layout.PixelPoint

/** One cursor's complete navigation state: its position, the anchor of an in-flight selection (if any), the preferred
  * column / measured pixel-x a later vertical move should resume from, and its caret-glide animation (if one is mid
  * flight).
  *
  * `EditingState(cursors: NonEmptyList[Cursor])` (`#1577`) is `Buffer`'s single cursor/selection collection --
  * `Buffer.cursorList`/`Buffer.withCursorList` round-trip through it, so `EditorEventReducer` has one code path over
  * `NonEmptyList[Cursor]` regardless of how many cursors a buffer has.
  *
  * `glide` (issue #1085 phase 2) is a GUI-canvas-only animation: it holds the caret's mid-flight pane-relative pixel
  * position whenever `CursorViewport` last moved this cursor with the `Cursor` motion family enabled, and is `None`
  * once the glide completes or motion is off -- both read by the renderer as "paint at the logical position, no
  * offset." TUI/hardware-cursor mode never seeds one (a terminal cursor can't glide sub-cell), so it stays `None` there
  * regardless of motion configuration.
  */
final case class Cursor(
    position: CursorPosition,
    selectionAnchor: Option[CursorPosition] = None,
    preferredColumn: Option[Int] = None,
    preferredXPx: Option[Float] = None,
    glide: Option[Tween[PixelPoint]] = None
):
  def selection: Option[Selection] = selectionAnchor.map(Selection(_, position))

object Cursor:
  def apply(selection: Selection): Cursor = Cursor(selection.focus, Some(selection.anchor))
