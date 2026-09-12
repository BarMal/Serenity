package com.serenity.state.reducers

import com.serenity.keystroke.events.*
import com.serenity.rope.Balance
import com.serenity.state.models.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Coverage for `EditorEventReducer` dispatching a single edit event against every active *selection* at once:
  * inserting, pasting, indenting/unindenting, and deleting (character and word) each replace or act on all selected
  * ranges together, then clear every selection (extracted from the former monolithic `EditorEventReducerSpec`,
  * #1442).
  */
class EditorMultiSelectionEditSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  "EditorEventReducer" should "replace every active selection when inserting with multiple selections" in {
    val paneId   = PaneId(0)
    val bufferId = BufferId(0)
    val first    = com.serenity.state.models.Selection(CursorPosition(0, 0), CursorPosition(0, 3))
    val second   = com.serenity.state.models.Selection(CursorPosition(0, 8), CursorPosition(0, 11))
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
                .copy(content = com.serenity.rope.Rope("abc def ghi")),
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

    val updatedState = EditorEventReducer.reduce(InsertChar('X'), paneId, initialState).state
    val buffer       = updatedState.persisted.buffers(bufferId)

    buffer.document.content.collect() shouldBe "X def X"
    buffer.editing.cursors shouldBe List(CursorPosition(0, 1), CursorPosition(0, 7))
    buffer.allSelections shouldBe Nil
  }

  it should "indent selected lines when tab is pressed with multiple selections" in {
    val paneId   = PaneId(0)
    val bufferId = BufferId(0)
    val first    = Selection(CursorPosition(0, 2), CursorPosition(1, 2))
    val second   = Selection(CursorPosition(2, 0), CursorPosition(2, 5))
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
                .copy(
                  cursors = List(first.focus, second.focus),
                  selection = Some(first),
                  selections = List(first, second)
                )
            )
        )
      )
    )

    val updatedState = EditorEventReducer.reduce(TabKey, paneId, initialState).state
    val buffer       = updatedState.persisted.buffers(bufferId)

    buffer.document.content.collect() shouldBe "    alpha\n    beta\n    gamma"
    buffer.editing.cursors shouldBe List(CursorPosition(1, 6), CursorPosition(2, 9))
    buffer.allSelections shouldBe Nil
  }

  it should "replace every active selection when pasting with multiple selections" in {
    val paneId   = PaneId(0)
    val bufferId = BufferId(0)
    val first    = Selection(CursorPosition(0, 0), CursorPosition(0, 3))
    val second   = Selection(CursorPosition(0, 8), CursorPosition(0, 11))
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
                .copy(content = com.serenity.rope.Rope("abc def ghi")),
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
      ),
      runtime = AppState.initial.runtime.copy(
        clipboard = Some("ZZ")
      )
    )

    val updatedState = EditorEventReducer.reduce(Paste, paneId, initialState).state
    val buffer       = updatedState.persisted.buffers(bufferId)

    buffer.document.content.collect() shouldBe "ZZ def ZZ"
    buffer.editing.cursors shouldBe List(CursorPosition(0, 2), CursorPosition(0, 9))
    buffer.allSelections shouldBe Nil
  }

  it should "delete every active selection when delete backward is pressed with multiple selections" in {
    val paneId   = PaneId(0)
    val bufferId = BufferId(0)
    val first    = com.serenity.state.models.Selection(CursorPosition(0, 0), CursorPosition(0, 3))
    val second   = com.serenity.state.models.Selection(CursorPosition(0, 8), CursorPosition(0, 11))
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
                .copy(content = com.serenity.rope.Rope("abc def ghi")),
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

    val updatedState = EditorEventReducer.reduce(DeleteBackward, paneId, initialState).state
    val buffer       = updatedState.persisted.buffers(bufferId)

    buffer.document.content.collect() shouldBe " def "
    buffer.editing.cursors shouldBe List(CursorPosition(0, 0), CursorPosition(0, 5))
    buffer.allSelections shouldBe Nil
  }

  it should "unindent selected lines when reverse-tab is pressed with multiple selections" in {
    val paneId   = PaneId(0)
    val bufferId = BufferId(0)
    val first    = Selection(CursorPosition(0, 4), CursorPosition(1, 2))
    val second   = Selection(CursorPosition(2, 0), CursorPosition(2, 6))
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
                .copy(content = com.serenity.rope.Rope("    alpha\n  beta\n\tgamma")),
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

    val updatedState = EditorEventReducer.reduce(ReverseTabKey, paneId, initialState).state
    val buffer       = updatedState.persisted.buffers(bufferId)

    buffer.document.content.collect() shouldBe "alpha\nbeta\ngamma"
    buffer.editing.cursors shouldBe List(CursorPosition(1, 0), CursorPosition(2, 5))
    buffer.allSelections shouldBe Nil
  }

  it should "delete every active selection when delete forward is pressed with multiple selections" in {
    val paneId   = PaneId(0)
    val bufferId = BufferId(0)
    val first    = Selection(CursorPosition(0, 0), CursorPosition(0, 3))
    val second   = Selection(CursorPosition(0, 8), CursorPosition(0, 11))
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
                .copy(content = com.serenity.rope.Rope("abc def ghi")),
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

    val updatedState = EditorEventReducer.reduce(DeleteForward, paneId, initialState).state
    val buffer       = updatedState.persisted.buffers(bufferId)

    buffer.document.content.collect() shouldBe " def "
    buffer.editing.cursors shouldBe List(CursorPosition(0, 0), CursorPosition(0, 5))
    buffer.allSelections shouldBe Nil
  }

  it should "delete every active selection when deleting the next word with multiple selections" in {
    val paneId   = PaneId(0)
    val bufferId = BufferId(0)
    val first    = Selection(CursorPosition(0, 0), CursorPosition(0, 3))
    val second   = Selection(CursorPosition(0, 8), CursorPosition(0, 11))
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
                .copy(content = com.serenity.rope.Rope("abc def ghi")),
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

    val updatedState = EditorEventReducer.reduce(DeleteWordForward, paneId, initialState).state
    val buffer       = updatedState.persisted.buffers(bufferId)

    buffer.document.content.collect() shouldBe " def "
    buffer.editing.cursors shouldBe List(CursorPosition(0, 0), CursorPosition(0, 5))
    buffer.allSelections shouldBe Nil
  }
