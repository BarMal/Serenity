package com.serenity.testkit

import cats.data.NonEmptyList
import com.serenity.state.models.*

/** A single cursor's preferred vertical-navigation state, as it existed as `EditingState`'s own
  * `multiCursorVerticalStates: List[VerticalCursorState]` field before `#1577` folded it into each [[Cursor]]
  * directly. Specs written against that five-field shape still build fixtures with it; kept here, rather than in
  * `Buffer.scala`, so it is clear this is a test-fixture compatibility shape, not part of the current model.
  */
final case class VerticalCursorState(cursor: CursorPosition, preferredColumn: Int, preferredXPx: Float)

/** Builds an [[EditingState]] from the pre-`#1577` five-field shape (`cursors`, `selection`, `selections`,
  * `preferredColumn`, `preferredXPx`, `multiCursorVerticalStates`), for specs whose fixtures still construct buffers
  * that way. Mirrors exactly the conversion `Buffer.cursorList` performed before `#1577` collapsed cursor storage to
  * `NonEmptyList[Cursor]`, so a spec built through this constructor sees the same resulting cursor/selection state it
  * did before -- only the fixture's own construction call changes, not what it asserts.
  */
object EditingStateFixtures:
  def apply(
    cursors: List[CursorPosition] = List(CursorPosition(0, 0)),
    selection: Option[Selection] = None,
    selections: List[Selection] = Nil,
    preferredColumn: Option[Int] = None,
    preferredXPx: Option[Float] = None,
    multiCursorVerticalStates: List[VerticalCursorState] = Nil
  ): EditingState =
    val allSelections = if selections.nonEmpty then selections else selection.toList
    val cursorList =
      if allSelections.nonEmpty then NonEmptyList.fromListUnsafe(allSelections.map(Cursor(_)))
      else if cursors.sizeIs > 1 then
        NonEmptyList.fromListUnsafe(cursors.map { position =>
          multiCursorVerticalStates.find(_.cursor == position) match
            case Some(state) => Cursor(position, None, Some(state.preferredColumn), Some(state.preferredXPx))
            case None        => Cursor(position)
        })
      else
        NonEmptyList.one(
          Cursor(cursors.headOption.getOrElse(CursorPosition(0, 0)), None, preferredColumn, preferredXPx)
        )
    EditingState(cursorList)
