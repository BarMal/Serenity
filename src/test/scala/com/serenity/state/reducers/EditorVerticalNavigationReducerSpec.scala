package com.serenity.state.reducers

import com.serenity.keystroke.events.*
import com.serenity.rope.Balance
import com.serenity.state.manager.EditorGeometryProducer
import com.serenity.state.models.*
import com.serenity.testkit.{EditingStateFixtures, VerticalCursorState}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Dedicated coverage for `EditorVerticalNavigationReducer` (#1442): Up/Down and their selection-extending forms -- the
  * one editor reduction whose result depends on measured text geometry. Exercises the single-cursor, selection
  * (extend), and multi-cursor paths this module dispatches between, plus its "no pane/buffer" no-op.
  */
class EditorVerticalNavigationReducerSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private val bufferId = BufferId(0)
  private val paneId   = PaneId(0)

  private def stateWith(
    text: String,
    cursors: List[CursorPosition],
    selection: Option[Selection] = None,
    selections: List[Selection] = Nil,
    multiCursorVerticalStates: List[VerticalCursorState] = Nil
  ): AppState =
    val buffer = Buffer
      .fromString(bufferId, text)
      .copy(editing =
        EditingStateFixtures(
          cursors = cursors,
          selection = selection,
          selections = selections,
          multiCursorVerticalStates = multiCursorVerticalStates
        )
      )
    val base = AppState.initial
    // Word wrap (and the visual-line cursor navigation it enables) defaults to on -- switch it off so MoveUp/MoveDown
    // take the plain logical-line path these tests exercise, rather than wrapping short lines against the real
    // measured geometry `EditorGeometryProducer.forPane` would otherwise build for this pane's configured width.
    base.copy(
      persisted = base.persisted.copy(
        buffers = Map(bufferId -> buffer),
        config = base.persisted.config.withWordWrap(false)
      )
    )

  private def geometryFor(state: AppState): EditorGeometry =
    EditorGeometryProducer
      .forPane(state, paneId)
      .getOrElse(EditorGeometry(NavigationGeometry(Vector.empty), charWidthPx = 8, panelWidthColumns = 80))

  private def bufferAfter(event: VerticalNavigationEvent, state: AppState): Buffer =
    val geometry = geometryFor(state)
    EditorEventReducer.reduceVerticalNavigation(event, paneId, state, geometry).state.persisted.buffers(bufferId)

  "MoveDown with a single cursor" should "move to the same column on the next line" in {
    val before = stateWith("alpha\nbeta\ngamma", List(CursorPosition(0, 2)))

    bufferAfter(MoveDown, before).editing.cursorPositions shouldBe List(CursorPosition(1, 2))
  }

  it should "clamp to the shorter line's length rather than overshoot" in {
    val before = stateWith("alpha\nb", List(CursorPosition(0, 4)))

    bufferAfter(MoveDown, before).editing.cursorPositions shouldBe List(CursorPosition(1, 1))
  }

  "MoveUp at the top line" should "leave the cursor in place" in {
    val before = stateWith("alpha\nbeta", List(CursorPosition(0, 2)))

    bufferAfter(MoveUp, before).editing.cursorPositions shouldBe List(CursorPosition(0, 2))
  }

  "MoveDown at the last line" should "leave the cursor in place" in {
    val before = stateWith("alpha\nbeta", List(CursorPosition(1, 1)))

    bufferAfter(MoveDown, before).editing.cursorPositions shouldBe List(CursorPosition(1, 1))
  }

  "ExtendSelectionDown" should "extend a selection anchored at the original cursor" in {
    val before = stateWith("alpha\nbeta\ngamma", List(CursorPosition(0, 2)))

    val after = bufferAfter(ExtendSelectionDown, before)
    after.primarySelection shouldBe Some(Selection(CursorPosition(0, 2), CursorPosition(1, 2)))
    after.editing.cursorPositions shouldBe List(CursorPosition(1, 2))
  }

  it should "replace stale in-flight vertical state (a sentinel preferred column/x) when extending" in {
    val staleCursor = Cursor(CursorPosition(0, 2), None, Some(999), Some(999f))
    val staleBuffer =
      Buffer.fromString(bufferId, "alpha\nbeta").copy(editing = EditingState.fromCursors(List(staleCursor)))
    val before = AppState.initial.copy(persisted =
      AppState.initial.persisted.copy(
        buffers = Map(bufferId -> staleBuffer),
        config = AppState.initial.persisted.config.withWordWrap(false)
      )
    )

    val after = bufferAfter(ExtendSelectionDown, before)
    after.editing.cursors.head.preferredColumn should not be Some(999)
    after.editing.cursors.head.preferredXPx should not be Some(999f)
  }

  "MoveDown with multiple cursors" should "move every cursor down independently, deduplicating and sorting" in {
    val before = stateWith("alpha\nbeta\ngamma", List(CursorPosition(0, 0), CursorPosition(0, 3)))

    bufferAfter(MoveDown, before).editing.cursorPositions shouldBe List(CursorPosition(1, 0), CursorPosition(1, 3))
  }

  it should "collapse an active multi-selection to its focuses before moving" in {
    val before = stateWith(
      "alpha\nbeta\ngamma",
      cursors = List(CursorPosition(0, 2), CursorPosition(1, 2)),
      selections = List(
        Selection(CursorPosition(0, 0), CursorPosition(0, 2)),
        Selection(CursorPosition(1, 0), CursorPosition(1, 2))
      )
    )

    val after = bufferAfter(MoveDown, before)
    after.allSelections shouldBe after.primarySelection.toList
    after.primarySelection shouldBe None
    after.editing.cursorPositions shouldBe List(CursorPosition(1, 2), CursorPosition(2, 2))
  }

  "MoveDown for a pane with no buffer" should "leave the state untouched" in {
    val state       = AppState.initial
    val emptyPaneId = PaneId(99)
    val geometry    = geometryFor(state)

    EditorEventReducer.reduceVerticalNavigation(MoveDown, emptyPaneId, state, geometry).state shouldBe state
  }
