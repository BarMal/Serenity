package com.serenity.state.models

/** One cursor's complete navigation state: its position, the anchor of an in-flight selection (if any), and the
  * preferred column / measured pixel-x a later vertical move should resume from.
  *
  * `Buffer` spreads this same information across five separate fields (`cursors`, `selection`, `selections`,
  * `preferredColumn`, `preferredXPx`, `multiCursorVerticalStates`) because splitting `Buffer` into narrower records is
  * `#1002`'s job, not this type's -- restating that split here would duplicate it under a different name. `Cursor` is
  * the per-cursor shape `EditorEventReducer` converts to and from at its own boundary (`Buffer.cursorList` /
  * `Buffer.withCursorList`), so the reducer has one code path over `NonEmptyList[Cursor]` regardless of how many
  * cursors a buffer has, without requiring `Buffer` itself to change shape yet.
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
