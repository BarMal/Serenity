package com.serenity.state.models

/** One cursor's complete navigation state: its position, the anchor of an in-flight selection (if any), and the
  * preferred column / measured pixel-x a later vertical move should resume from.
  *
  * `EditingState(cursors: NonEmptyList[Cursor])` (`#1577`) is `Buffer`'s single cursor/selection collection --
  * `Buffer.cursorList`/`Buffer.withCursorList` round-trip through it, so `EditorEventReducer` has one code path over
  * `NonEmptyList[Cursor]` regardless of how many cursors a buffer has.
  */
final case class Cursor(
    position: CursorPosition,
    selectionAnchor: Option[CursorPosition] = None,
    preferredColumn: Option[Int] = None,
    preferredXPx: Option[Float] = None
):
  def selection: Option[Selection] = selectionAnchor.map(Selection(_, position))

object Cursor:
  def apply(selection: Selection): Cursor = Cursor(selection.focus, Some(selection.anchor))
