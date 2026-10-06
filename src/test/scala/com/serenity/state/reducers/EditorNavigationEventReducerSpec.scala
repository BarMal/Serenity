package com.serenity.state.reducers

import com.serenity.keystroke.events.*
import com.serenity.rope.Balance
import com.serenity.state.models.*
import com.serenity.testkit.EditingStateFixtures
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Dedicated coverage for `EditorNavigationEventReducer` (#1442): caret movement, paging, and select-all -- the family
  * that repositions cursors/selection without touching document content. Focuses on behavior specific to this module
  * (multi-cursor collapsing, downstream affinity normalization, the `MoveToEndOfFile` single-cursor/multi-cursor split)
  * rather than re-asserting dispatch already covered end-to-end elsewhere.
  */
class EditorNavigationEventReducerSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private val bufferId = BufferId(0)
  private val paneId   = PaneId(0)

  private def stateWith(
    text: String,
    cursors: List[CursorPosition],
    selection: Option[Selection] = None,
    selections: List[Selection] = Nil
  ): AppState =
    val buffer = Buffer
      .fromString(bufferId, text)
      .copy(editing = EditingStateFixtures(cursors = cursors, selection = selection, selections = selections))
    AppState.initial.copy(persisted = AppState.initial.persisted.copy(buffers = Map(bufferId -> buffer)))

  private def bufferAfter(event: TextEntryEvent, state: AppState): Buffer =
    EditorEventReducer.reduce(event, paneId, state).state.persisted.buffers(bufferId)

  "MoveRight" should "move a single cursor one column right" in {
    val before = stateWith("hello", List(CursorPosition(0, 0)))

    bufferAfter(MoveRight, before).editing.cursorPositions shouldBe List(CursorPosition(0, 1))
  }

  it should "move every cursor independently when there are several" in {
    val before = stateWith("hello\nworld", List(CursorPosition(0, 0), CursorPosition(1, 0)))

    bufferAfter(MoveRight, before).editing.cursorPositions shouldBe List(CursorPosition(0, 1), CursorPosition(1, 1))
  }

  it should "collapse an active selection to its focus first, rather than moving both ends" in {
    val before =
      stateWith(
        "hello",
        List(CursorPosition(0, 3)),
        selection = Some(Selection(CursorPosition(0, 0), CursorPosition(0, 3)))
      )

    val after = bufferAfter(MoveRight, before)
    after.primarySelection shouldBe None
    after.editing.cursorPositions shouldBe List(CursorPosition(0, 4))
  }

  it should "deduplicate cursors that land on the same position and sort the result" in {
    val before = stateWith("hello", List(CursorPosition(0, 5), CursorPosition(0, 4)))

    // "hello" has 5 characters: the cursor already at column 5 (the end) can't move further right and stays put,
    // while the cursor at column 4 advances onto that same position -- a genuine collision the reducer's
    // `.distinct` must collapse to one cursor rather than leaving a duplicate in the list.
    bufferAfter(MoveRight, before).editing.cursorPositions shouldBe List(CursorPosition(0, 5))
  }

  "MoveLeft at the start of the document" should "leave the cursor in place" in {
    val before = stateWith("hello", List(CursorPosition(0, 0)))

    bufferAfter(MoveLeft, before).editing.cursorPositions shouldBe List(CursorPosition(0, 0))
  }

  "MoveWordRight" should "land on the next word boundary" in {
    val before = stateWith("alpha beta", List(CursorPosition(0, 0)))

    bufferAfter(MoveWordRight, before).editing.cursorPositions shouldBe List(CursorPosition(0, 6))
  }

  "MoveWordLeft" should "land on the previous word boundary" in {
    val before = stateWith("alpha beta", List(CursorPosition(0, 10)))

    bufferAfter(MoveWordLeft, before).editing.cursorPositions shouldBe List(CursorPosition(0, 6))
  }

  "MoveSubWordRight" should "land on the end of the next identifier part" in {
    val before = stateWith("fooBar_baz qux", List(CursorPosition(0, 0)))

    bufferAfter(MoveSubWordRight, before).editing.cursorPositions shouldBe List(CursorPosition(0, 3))
  }

  "MoveSubWordLeft" should "land on the start of the previous identifier part" in {
    val before = stateWith("fooBar_baz qux", List(CursorPosition(0, 11)))

    bufferAfter(MoveSubWordLeft, before).editing.cursorPositions shouldBe List(CursorPosition(0, 7))
  }

  "MoveToStartOfFile" should "move the cursor to line 0, column 0 regardless of starting position" in {
    val before = stateWith("alpha\nbeta\ngamma", List(CursorPosition(2, 3)))

    bufferAfter(MoveToStartOfFile, before).editing.cursorPositions shouldBe List(CursorPosition(0, 0))
  }

  "SelectAll" should "select from the start of the document to the end of the last line" in {
    val before = stateWith("alpha\nbeta", List(CursorPosition(0, 0)))

    val after = bufferAfter(SelectAll, before)
    after.primarySelection shouldBe Some(Selection(CursorPosition(0, 0), CursorPosition(1, 4)))
    after.editing.cursorPositions shouldBe List(CursorPosition(1, 4))
  }

  it should "collapse any existing multi-cursor/selection state before selecting the whole document" in {
    val before = stateWith(
      "alpha\nbeta",
      List(CursorPosition(0, 1), CursorPosition(1, 1)),
      selections = List(Selection(CursorPosition(0, 0), CursorPosition(0, 1)))
    )

    val after = bufferAfter(SelectAll, before)
    after.allSelections shouldBe after.primarySelection.toList
    after.editing.cursorPositions shouldBe List(CursorPosition(1, 4))
  }

  "MoveToEndOfFile without a selection or extra cursors" should "move the cursor to the end of the last line" in {
    val before = stateWith("alpha\nbeta", List(CursorPosition(0, 0)))

    bufferAfter(MoveToEndOfFile, before).editing.cursorPositions shouldBe List(CursorPosition(1, 4))
  }

  "MoveToEndOfFile with an active selection" should "collapse the selection and land at the end of the last line" in {
    val before = stateWith(
      "alpha\nbeta",
      List(CursorPosition(0, 3)),
      selection = Some(Selection(CursorPosition(0, 0), CursorPosition(0, 3)))
    )

    val after = bufferAfter(MoveToEndOfFile, before)
    after.primarySelection shouldBe None
    after.editing.cursorPositions shouldBe List(CursorPosition(1, 4))
  }

  "MoveToEndOfFile with multiple cursors" should "collapse every cursor to the single end-of-file position" in {
    val before = stateWith("alpha\nbeta", List(CursorPosition(0, 1), CursorPosition(1, 2)))

    bufferAfter(MoveToEndOfFile, before).editing.cursorPositions shouldBe List(CursorPosition(1, 4))
  }

  "PageDown" should "scroll the topLine forward and move the cursor down by a page" in {
    val manyLines = (0 until 200).map(i => s"line$i").mkString("\n")
    val before    = stateWith(manyLines, List(CursorPosition(0, 0)))

    val after = bufferAfter(PageDown, before)
    after.editing.cursorPositions.head.line should be > 0
  }

  "PageUp at the top of the document" should "leave the cursor at line 0" in {
    val manyLines = (0 until 200).map(i => s"line$i").mkString("\n")
    val before    = stateWith(manyLines, List(CursorPosition(0, 0)))

    bufferAfter(PageUp, before).editing.cursorPositions shouldBe List(CursorPosition(0, 0))
  }

  // Column-based document layout (issue #1338, Phase 1): ColumnRight/ColumnLeft jump exactly one column's worth of
  // visual rows, the same distance PageDown/PageUp already jump -- they reuse `pageTarget`'s visual-row-walk branch.
  "ColumnRight" should "scroll the topLine forward and move the cursor down by exactly one column's rows" in {
    val manyLines = (0 until 200).map(i => s"line$i").mkString("\n")
    val before    = stateWith(manyLines, List(CursorPosition(0, 0)))

    val after = bufferAfter(ColumnRight, before)
    after.editing.cursorPositions.head.line should be > 0
  }

  "ColumnLeft at the top of the document" should "leave the cursor at line 0" in {
    val manyLines = (0 until 200).map(i => s"line$i").mkString("\n")
    val before    = stateWith(manyLines, List(CursorPosition(0, 0)))

    bufferAfter(ColumnLeft, before).editing.cursorPositions shouldBe List(CursorPosition(0, 0))
  }

  "ColumnRight and PageDown" should "move a cursor by exactly the same amount" in {
    val manyLines = (0 until 200).map(i => s"line$i").mkString("\n")
    val before    = stateWith(manyLines, List(CursorPosition(0, 0)))

    bufferAfter(ColumnRight, before).editing.cursors shouldBe bufferAfter(PageDown, before).editing.cursors
  }

  "An event outside this reducer's family" should "fall through to the no-op default, unchanged" in {
    val before = stateWith("hello", List(CursorPosition(0, 0)))
    val buffer = before.persisted.buffers(bufferId)
    val ctx = EditorCursorSupport.CursorEventContext(buffer, CursorPosition(0, 0), false, false, before, paneId, None)

    EditorNavigationEventReducer.reduce(NewLine, ctx).state shouldBe before
  }

  // -- Geometry injection (#1676) -------------------------------------------------------------------------------------
  //
  // `homeTarget`/`endTarget`/`pageTarget` take the measured geometry as a parameter rather than reaching for
  // `EditorGeometryProducer` themselves, so these specs build the geometry directly -- exactly as
  // `EditorVerticalNavigationReducerSpec` already does for Up/Down -- with no font loading anywhere in the test.

  /** One logical line, "abcdefghij" (10 columns), wrapped at column 5 into two visual rows -- built by hand, not
    * measured, to pin Home/End/paging against a known wrap boundary without a real font.
    */
  private def wrappedRowGeometry: EditorGeometry =
    EditorGeometry(
      NavigationGeometry(
        Vector(
          TextVisualLine(
            bufferLine = 0,
            startColumn = 0,
            endColumn = 5,
            text = "abcde",
            widthPx = 50f,
            caretStops = Vector.empty
          ),
          TextVisualLine(
            bufferLine = 0,
            startColumn = 5,
            endColumn = 10,
            text = "fghij",
            widthPx = 50f,
            caretStops = Vector.empty
          )
        )
      ),
      charWidthPx = 10,
      panelWidthColumns = 5
    )

  "MoveToStart with explicit visual-row geometry" should "land on the start of the cursor's own wrapped row, not the logical line" in {
    val before = stateWith("abcdefghij", List(CursorPosition(0, 7)))

    val after =
      EditorEventReducer.reduce(MoveToStart, paneId, before, Some(wrappedRowGeometry)).state.persisted.buffers(bufferId)

    after.editing.cursorPositions shouldBe List(CursorPosition(0, 5))
  }

  "MoveToStart with no geometry available" should "fall back to the logical line's own start" in {
    val before = stateWith("abcdefghij", List(CursorPosition(0, 7)))

    val after =
      EditorEventReducer.reduce(MoveToStart, paneId, before, geometry = None).state.persisted.buffers(bufferId)

    after.editing.cursorPositions shouldBe List(CursorPosition(0, 0))
  }

  "MoveToEnd with explicit visual-row geometry" should "land on the end of the cursor's own wrapped row, not the logical line's end" in {
    val before = stateWith("abcdefghij", List(CursorPosition(0, 7)))

    val after =
      EditorEventReducer.reduce(MoveToEnd, paneId, before, Some(wrappedRowGeometry)).state.persisted.buffers(bufferId)

    // End always lands with upstream affinity (a second End must be idempotent rather than reading the boundary
    // column as the start of the row below -- see EditorCursorSupport.endTarget).
    after.editing.cursorPositions shouldBe List(CursorPosition(0, 10).upstream)
  }

  "PageDown with explicit page-sized geometry" should "walk visual rows rather than logical lines" in {
    val manyLines = (0 until 10).map(i => s"line$i").mkString("\n")
    val buffer = Buffer
      .fromString(bufferId, manyLines)
      .copy(editing = EditingStateFixtures(cursors = List(CursorPosition(0, 0))))
    val before = AppState.initial.copy(persisted =
      AppState.initial.persisted
        .copy(buffers = Map(bufferId -> buffer.copy(viewport = buffer.viewport.copy(visibleLines = 2))))
    )
    // Every logical line is its own single visual row here (no wrapping), one-to-one with the buffer's ten lines.
    val geometry = EditorGeometry(
      NavigationGeometry(
        (0 until 10).map { line =>
          TextVisualLine(
            bufferLine = line,
            startColumn = 0,
            endColumn = 5,
            text = s"line$line",
            widthPx = 50f,
            caretStops = Vector.empty
          )
        }.toVector
      ),
      charWidthPx = 10,
      panelWidthColumns = 5
    )

    val after = EditorEventReducer.reduce(PageDown, paneId, before, Some(geometry)).state.persisted.buffers(bufferId)

    // visibleLines = 2, so one PageDown from visual row 0 lands on visual row 2 -- bufferLine 2's own start.
    after.editing.cursorPositions shouldBe List(CursorPosition(2, 0))
  }
