package com.serenity.state.reducers

import com.serenity.keystroke.events.*
import com.serenity.rope.Balance
import com.serenity.state.manager.CursorViewport
import com.serenity.state.models.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Coverage for `EditorEventReducer` dispatching a single navigation or shift-extend event to every active cursor or
  * selection at once -- horizontal/vertical movement, line and file boundaries, paging, select-all, and shift
  * navigation all move each one independently rather than only a lone cursor (extracted from the former monolithic
  * `EditorEventReducerSpec`, #1442).
  */
class EditorMultiCursorNavigationSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  "EditorEventReducer" should "move every selection focus by a page when multiple selections are active" in {
    val paneId   = PaneId(0)
    val bufferId = BufferId(0)
    val first    = Selection(CursorPosition(1, 0), CursorPosition(1, 1))
    val second   = Selection(CursorPosition(3, 0), CursorPosition(3, 1))
    val initialState = AppState.initial.copy(
      persisted = AppState.initial.persisted.copy(
        buffers = AppState.initial.persisted.buffers.updated(
          bufferId,
          AppState.initial.persisted
            .buffers(bufferId)
            .copy(
              document = AppState.initial.persisted
                .buffers(bufferId)
                .document
                .copy(content = com.serenity.rope.Rope("0\n1\n2\n3\n4\n5")),
              editing = AppState.initial.persisted
                .buffers(bufferId)
                .editing
                .copy(
                  cursors = List(first.focus, second.focus),
                  selection = Some(first),
                  selections = List(first, second)
                ),
              viewport = AppState.initial.persisted.buffers(bufferId).viewport.copy(visibleLines = 2)
            )
        )
      )
    )

    val updatedState = EditorEventReducer.reduce(PageDown, paneId, initialState).state
    val buffer       = updatedState.persisted.buffers(bufferId)

    buffer.editing.cursors shouldBe List(CursorPosition(3, 0), CursorPosition(5, 0))
    buffer.allSelections shouldBe Nil
  }

  it should "move every selection focus to the start of the file when multiple selections are active" in {
    val paneId   = PaneId(0)
    val bufferId = BufferId(0)
    val first    = Selection(CursorPosition(1, 0), CursorPosition(1, 1))
    val second   = Selection(CursorPosition(3, 0), CursorPosition(3, 1))
    val initialState = AppState.initial.copy(
      persisted = AppState.initial.persisted.copy(
        buffers = AppState.initial.persisted.buffers.updated(
          bufferId,
          AppState.initial.persisted
            .buffers(bufferId)
            .copy(
              document = AppState.initial.persisted
                .buffers(bufferId)
                .document
                .copy(content = com.serenity.rope.Rope("0\n1\n2\n3\n4\n5")),
              editing = AppState.initial.persisted
                .buffers(bufferId)
                .editing
                .copy(
                  cursors = List(first.focus, second.focus),
                  selection = Some(first),
                  selections = List(first, second)
                )
            )
        )
      )
    )

    val updatedState = EditorEventReducer.reduce(MoveToStartOfFile, paneId, initialState).state
    val buffer       = updatedState.persisted.buffers(bufferId)

    buffer.editing.cursors shouldBe List(CursorPosition(0, 0))
    buffer.allSelections shouldBe Nil
  }

  it should "move every selection focus to the end of the file when multiple selections are active" in {
    val paneId   = PaneId(0)
    val bufferId = BufferId(0)
    val first    = Selection(CursorPosition(1, 0), CursorPosition(1, 1))
    val second   = Selection(CursorPosition(3, 0), CursorPosition(3, 1))
    val initialState = AppState.initial.copy(
      persisted = AppState.initial.persisted.copy(
        buffers = AppState.initial.persisted.buffers.updated(
          bufferId,
          AppState.initial.persisted
            .buffers(bufferId)
            .copy(
              document = AppState.initial.persisted
                .buffers(bufferId)
                .document
                .copy(content = com.serenity.rope.Rope("0\n1\n2\n3\n45")),
              editing = AppState.initial.persisted
                .buffers(bufferId)
                .editing
                .copy(
                  cursors = List(first.focus, second.focus),
                  selection = Some(first),
                  selections = List(first, second)
                )
            )
        )
      )
    )

    val updatedState = EditorEventReducer.reduce(MoveToEndOfFile, paneId, initialState).state
    val buffer       = updatedState.persisted.buffers(bufferId)

    buffer.editing.cursors shouldBe List(CursorPosition(4, 2))
    buffer.allSelections shouldBe Nil
  }

  it should "move every cursor left when multiple cursors are active" in {
    val paneId   = PaneId(0)
    val bufferId = BufferId(0)
    val initialState = AppState.initial.copy(
      persisted = AppState.initial.persisted.copy(
        buffers = AppState.initial.persisted.buffers.updated(
          bufferId,
          AppState.initial.persisted
            .buffers(bufferId)
            .copy(
              document =
                AppState.initial.persisted.buffers(bufferId).document.copy(content = com.serenity.rope.Rope("abcd")),
              editing = AppState.initial.persisted
                .buffers(bufferId)
                .editing
                .copy(cursors = List(CursorPosition(0, 2), CursorPosition(0, 4)))
            )
        )
      )
    )

    val updatedState = EditorEventReducer.reduce(MoveLeft, paneId, initialState).state
    updatedState.persisted.buffers(bufferId).editing.cursors shouldBe List(CursorPosition(0, 1), CursorPosition(0, 3))
  }

  it should "move every cursor right when multiple cursors are active" in {
    val paneId   = PaneId(0)
    val bufferId = BufferId(0)
    val initialState = AppState.initial.copy(
      persisted = AppState.initial.persisted.copy(
        buffers = AppState.initial.persisted.buffers.updated(
          bufferId,
          AppState.initial.persisted
            .buffers(bufferId)
            .copy(
              document =
                AppState.initial.persisted.buffers(bufferId).document.copy(content = com.serenity.rope.Rope("abcd")),
              editing = AppState.initial.persisted
                .buffers(bufferId)
                .editing
                .copy(cursors = List(CursorPosition(0, 0), CursorPosition(0, 2)))
            )
        )
      )
    )

    val updatedState = EditorEventReducer.reduce(MoveRight, paneId, initialState).state
    updatedState.persisted.buffers(bufferId).editing.cursors shouldBe List(CursorPosition(0, 1), CursorPosition(0, 3))
  }

  it should "move every cursor to line start when multiple cursors are active" in {
    val paneId   = PaneId(0)
    val bufferId = BufferId(0)
    val initialState = AppState.initial.copy(
      persisted = AppState.initial.persisted.copy(
        buffers = AppState.initial.persisted.buffers.updated(
          bufferId,
          AppState.initial.persisted
            .buffers(bufferId)
            .copy(
              document =
                AppState.initial.persisted.buffers(bufferId).document.copy(content = com.serenity.rope.Rope("abcd")),
              editing = AppState.initial.persisted
                .buffers(bufferId)
                .editing
                .copy(cursors = List(CursorPosition(0, 2), CursorPosition(0, 4)))
            )
        )
      )
    )

    val updatedState = EditorEventReducer.reduce(MoveToStart, paneId, initialState).state
    updatedState.persisted.buffers(bufferId).editing.cursors shouldBe List(CursorPosition(0, 0))
  }

  it should "move every cursor to line end when multiple cursors are active" in {
    val paneId   = PaneId(0)
    val bufferId = BufferId(0)
    val initialState = AppState.initial.copy(
      persisted = AppState.initial.persisted.copy(
        buffers = AppState.initial.persisted.buffers.updated(
          bufferId,
          AppState.initial.persisted
            .buffers(bufferId)
            .copy(
              document =
                AppState.initial.persisted.buffers(bufferId).document.copy(content = com.serenity.rope.Rope("abcd")),
              editing = AppState.initial.persisted
                .buffers(bufferId)
                .editing
                .copy(cursors = List(CursorPosition(0, 0), CursorPosition(0, 2)))
            )
        )
      )
    )

    val updatedState = EditorEventReducer.reduce(MoveToEnd, paneId, initialState).state
    // End pins each cursor to the row it was pressed on (`RowAffinity.Upstream`), which on this unwrapped line is the
    // only row there is.
    updatedState.persisted.buffers(bufferId).editing.cursors shouldBe List(CursorPosition(0, 4).upstream)
  }

  it should "move every cursor down while preserving per-cursor columns when multiple cursors are active" in {
    val paneId   = PaneId(0)
    val bufferId = BufferId(0)
    val initialState = AppState.initial.copy(
      persisted = AppState.initial.persisted.copy(
        buffers = AppState.initial.persisted.buffers.updated(
          bufferId,
          AppState.initial.persisted
            .buffers(bufferId)
            .copy(
              document = AppState.initial.persisted
                .buffers(bufferId)
                .document
                .copy(content = com.serenity.rope.Rope("abcd\nwxyz")),
              editing = AppState.initial.persisted
                .buffers(bufferId)
                .editing
                .copy(cursors = List(CursorPosition(0, 1), CursorPosition(0, 3)))
            )
        )
      )
    )

    val updatedState = com.serenity.VerticalNavSupport.dispatch(MoveDown, paneId, initialState).state
    updatedState.persisted.buffers(bufferId).editing.cursors shouldBe List(CursorPosition(1, 1), CursorPosition(1, 3))
  }

  it should "move every cursor up while preserving per-cursor columns when multiple cursors are active" in {
    val paneId   = PaneId(0)
    val bufferId = BufferId(0)
    val initialState = AppState.initial.copy(
      persisted = AppState.initial.persisted.copy(
        buffers = AppState.initial.persisted.buffers.updated(
          bufferId,
          AppState.initial.persisted
            .buffers(bufferId)
            .copy(
              document = AppState.initial.persisted
                .buffers(bufferId)
                .document
                .copy(content = com.serenity.rope.Rope("abcd\nwxyz")),
              editing = AppState.initial.persisted
                .buffers(bufferId)
                .editing
                .copy(cursors = List(CursorPosition(1, 1), CursorPosition(1, 3)))
            )
        )
      )
    )

    val updatedState = com.serenity.VerticalNavSupport.dispatch(MoveUp, paneId, initialState).state
    updatedState.persisted.buffers(bufferId).editing.cursors shouldBe List(CursorPosition(0, 1), CursorPosition(0, 3))
  }

  it should "move every cursor to the start of the file when multiple cursors are active" in {
    val paneId   = PaneId(0)
    val bufferId = BufferId(0)
    val initialState = AppState.initial.copy(
      persisted = AppState.initial.persisted.copy(
        buffers = AppState.initial.persisted.buffers.updated(
          bufferId,
          AppState.initial.persisted
            .buffers(bufferId)
            .copy(
              document = AppState.initial.persisted
                .buffers(bufferId)
                .document
                .copy(content = com.serenity.rope.Rope("alpha\nbeta\ngamma")),
              editing = AppState.initial.persisted
                .buffers(bufferId)
                .editing
                .copy(cursors = List(CursorPosition(0, 3), CursorPosition(2, 4)))
            )
        )
      )
    )

    val updatedState = EditorEventReducer.reduce(MoveToStartOfFile, paneId, initialState).state
    updatedState.persisted.buffers(bufferId).editing.cursors shouldBe List(CursorPosition(0, 0))
  }

  it should "move every cursor to the end of the file when multiple cursors are active" in {
    val paneId   = PaneId(0)
    val bufferId = BufferId(0)
    val initialState = AppState.initial.copy(
      persisted = AppState.initial.persisted.copy(
        buffers = AppState.initial.persisted.buffers.updated(
          bufferId,
          AppState.initial.persisted
            .buffers(bufferId)
            .copy(
              document = AppState.initial.persisted
                .buffers(bufferId)
                .document
                .copy(content = com.serenity.rope.Rope("alpha\nbeta\ngamma")),
              editing = AppState.initial.persisted
                .buffers(bufferId)
                .editing
                .copy(cursors = List(CursorPosition(0, 1), CursorPosition(1, 2)))
            )
        )
      )
    )

    val updatedState = EditorEventReducer.reduce(MoveToEndOfFile, paneId, initialState).state
    updatedState.persisted.buffers(bufferId).editing.cursors shouldBe List(CursorPosition(2, 5))
  }

  it should "move every cursor up by a visible page when multiple cursors are active" in {
    val paneId   = PaneId(0)
    val bufferId = BufferId(0)
    val initialState = AppState.initial.copy(
      persisted = AppState.initial.persisted.copy(
        buffers = AppState.initial.persisted.buffers.updated(
          bufferId,
          AppState.initial.persisted
            .buffers(bufferId)
            .copy(
              document = AppState.initial.persisted
                .buffers(bufferId)
                .document
                .copy(content = com.serenity.rope.Rope("0\n1\n2\n3\n4\n5")),
              editing = AppState.initial.persisted
                .buffers(bufferId)
                .editing
                .copy(cursors = List(CursorPosition(3, 0), CursorPosition(5, 0))),
              viewport = AppState.initial.persisted.buffers(bufferId).viewport.copy(topLine = 2, visibleLines = 2)
            )
        )
      )
    )

    // The reducer moves the cursors; the viewport is the effect boundary's to place, exactly as it is for every other
    // navigation event (`CursorViewport.ensureVisibleCursors`, composed here the way `StateManagerEventPipeline` and
    // `EditorPaneComponent` compose it).
    val reducedState = EditorEventReducer.reduce(PageUp, paneId, initialState).state
    val updatedState = CursorViewport.ensureVisibleCursors(initialState, reducedState)
    val buffer       = updatedState.persisted.buffers(bufferId)

    buffer.editing.cursors shouldBe List(CursorPosition(1, 0), CursorPosition(3, 0))
    buffer.viewport.topLine shouldBe 0
  }

  it should "select the whole buffer when select-all is pressed with multiple cursors" in {
    val paneId   = PaneId(0)
    val bufferId = BufferId(0)
    val initialState = AppState.initial.copy(
      persisted = AppState.initial.persisted.copy(
        buffers = AppState.initial.persisted.buffers.updated(
          bufferId,
          AppState.initial.persisted
            .buffers(bufferId)
            .copy(
              document = AppState.initial.persisted
                .buffers(bufferId)
                .document
                .copy(content = com.serenity.rope.Rope("alpha\nbeta")),
              editing = AppState.initial.persisted
                .buffers(bufferId)
                .editing
                .copy(cursors = List(CursorPosition(0, 1), CursorPosition(1, 2)))
            )
        )
      )
    )

    val updatedState = EditorEventReducer.reduce(SelectAll, paneId, initialState).state
    val buffer       = updatedState.persisted.buffers(bufferId)

    buffer.editing.cursors shouldBe List(CursorPosition(1, 4))
    buffer.editing.selection shouldBe Some(Selection(CursorPosition(0, 0), CursorPosition(1, 4)))
  }

  it should "select the whole buffer when select-all is pressed with multiple selections" in {
    val paneId   = PaneId(0)
    val bufferId = BufferId(0)
    val first    = Selection(CursorPosition(0, 1), CursorPosition(0, 3))
    val second   = Selection(CursorPosition(1, 0), CursorPosition(1, 2))
    val initialState = AppState.initial.copy(
      persisted = AppState.initial.persisted.copy(
        buffers = AppState.initial.persisted.buffers.updated(
          bufferId,
          AppState.initial.persisted
            .buffers(bufferId)
            .copy(
              document = AppState.initial.persisted
                .buffers(bufferId)
                .document
                .copy(content = com.serenity.rope.Rope("alpha\nbeta")),
              editing = AppState.initial.persisted
                .buffers(bufferId)
                .editing
                .copy(
                  cursors = List(first.focus, second.focus),
                  selection = Some(first),
                  selections = List(first, second)
                )
            )
        )
      )
    )

    val updatedState = EditorEventReducer.reduce(SelectAll, paneId, initialState).state
    val buffer       = updatedState.persisted.buffers(bufferId)

    buffer.editing.cursors shouldBe List(CursorPosition(1, 4))
    buffer.editing.selection shouldBe Some(Selection(CursorPosition(0, 0), CursorPosition(1, 4)))
    buffer.allSelections shouldBe List(Selection(CursorPosition(0, 0), CursorPosition(1, 4)))
  }

  it should "extend a selection horizontally with shift navigation" in {
    val paneId   = PaneId(0)
    val bufferId = BufferId(0)
    val initialState = AppState.initial.copy(
      persisted = AppState.initial.persisted.copy(
        buffers = AppState.initial.persisted.buffers.updated(
          bufferId,
          AppState.initial.persisted
            .buffers(bufferId)
            .copy(
              document =
                AppState.initial.persisted.buffers(bufferId).document.copy(content = com.serenity.rope.Rope("abcd")),
              editing = AppState.initial.persisted.buffers(bufferId).editing.copy(cursors = List(CursorPosition(0, 1)))
            )
        )
      )
    )

    val firstState  = EditorEventReducer.reduce(ExtendSelectionRight, paneId, initialState).state
    val secondState = EditorEventReducer.reduce(ExtendSelectionRight, paneId, firstState).state
    val buffer      = secondState.persisted.buffers(bufferId)

    buffer.editing.cursors shouldBe List(CursorPosition(0, 3))
    buffer.editing.selection shouldBe Some(Selection(CursorPosition(0, 1), CursorPosition(0, 3)))
    buffer.allSelections shouldBe List(Selection(CursorPosition(0, 1), CursorPosition(0, 3)))
  }

  it should "extend a selection vertically with shift navigation" in {
    val paneId   = PaneId(0)
    val bufferId = BufferId(0)
    val initialState = AppState.initial.copy(
      persisted = AppState.initial.persisted.copy(
        buffers = AppState.initial.persisted.buffers.updated(
          bufferId,
          AppState.initial.persisted
            .buffers(bufferId)
            .copy(
              document = AppState.initial.persisted
                .buffers(bufferId)
                .document
                .copy(content = com.serenity.rope.Rope("abc\ndef")),
              editing = AppState.initial.persisted.buffers(bufferId).editing.copy(cursors = List(CursorPosition(1, 1)))
            )
        )
      )
    )

    val updatedState = com.serenity.VerticalNavSupport.dispatch(ExtendSelectionUp, paneId, initialState).state
    val buffer       = updatedState.persisted.buffers(bufferId)

    buffer.editing.cursors shouldBe List(CursorPosition(0, 1))
    buffer.editing.selection shouldBe Some(Selection(CursorPosition(1, 1), CursorPosition(0, 1)))
    buffer.editing.selection.map(_.start) shouldBe Some(CursorPosition(0, 1))
    buffer.editing.selection.map(_.end) shouldBe Some(CursorPosition(1, 1))
  }
