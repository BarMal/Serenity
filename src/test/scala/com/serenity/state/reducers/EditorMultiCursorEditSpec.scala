package com.serenity.state.reducers

import com.serenity.keystroke.events.*
import com.serenity.rope.Balance
import com.serenity.state.models.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Coverage for `EditorEventReducer`'s single-event dispatch to every active cursor at once: insertion, newline,
  * indent/unindent, deletion (character and word), and paste all apply per cursor rather than only at a lone one
  * (extracted from the former monolithic `EditorEventReducerSpec`, #1442).
  */
class EditorMultiCursorEditSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  "EditorEventReducer" should "insert characters into the focused pane buffer" in {
    val initialState = AppState.initial
    val paneId       = PaneId(0)

    val updatedState = EditorEventReducer.reduce(InsertChar('x'), paneId, initialState).state
    val bufferId     = updatedState.persisted.layout.editorPanes(paneId).bufferId.get
    val buffer       = updatedState.persisted.buffers(bufferId)

    buffer.document.content.collect() shouldBe "x"
    buffer.editing.cursors.head shouldBe com.serenity.state.models.CursorPosition(0, 1)
    buffer.document.isDirty shouldBe true
  }

  it should "insert characters at every cursor position when multiple cursors are active" in {
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
                .copy(cursors = List(CursorPosition(0, 1), CursorPosition(0, 3)))
            )
        )
      )
    )

    val updatedState = EditorEventReducer.reduce(InsertChar('X'), paneId, initialState).state
    val buffer       = updatedState.persisted.buffers(bufferId)

    buffer.document.content.collect() shouldBe "aXbcXd"
    buffer.editing.cursors shouldBe List(CursorPosition(0, 2), CursorPosition(0, 5))
  }

  it should "insert newlines at every cursor position when multiple cursors are active" in {
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
                .copy(cursors = List(CursorPosition(0, 1), CursorPosition(0, 3)))
            )
        )
      )
    )

    val updatedState = EditorEventReducer.reduce(NewLine, paneId, initialState).state
    val buffer       = updatedState.persisted.buffers(bufferId)

    buffer.document.content.collect() shouldBe "a\nbc\nd"
    buffer.editing.cursors shouldBe List(CursorPosition(1, 0), CursorPosition(2, 0))
  }

  it should "insert fixed spaces at every cursor position when tab is pressed with multiple cursors" in {
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
                .copy(cursors = List(CursorPosition(0, 1), CursorPosition(0, 3)))
            )
        )
      )
    )

    val updatedState = EditorEventReducer.reduce(TabKey, paneId, initialState).state
    val buffer       = updatedState.persisted.buffers(bufferId)

    buffer.document.content.collect() shouldBe "a    bc    d"
    buffer.editing.cursors shouldBe List(CursorPosition(0, 5), CursorPosition(0, 11))
  }

  it should "remove one indentation level when reverse-tab is pressed with a single cursor" in {
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
                AppState.initial.persisted.buffers(bufferId).document.copy(content = com.serenity.rope.Rope("    abc")),
              editing = AppState.initial.persisted.buffers(bufferId).editing.copy(cursors = List(CursorPosition(0, 6)))
            )
        )
      )
    )

    val updatedState = EditorEventReducer.reduce(ReverseTabKey, paneId, initialState).state
    val buffer       = updatedState.persisted.buffers(bufferId)

    buffer.document.content.collect() shouldBe "abc"
    buffer.editing.cursors shouldBe List(CursorPosition(0, 2))
  }

  it should "delete backward at every cursor position when multiple cursors are active" in {
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
                AppState.initial.persisted.buffers(bufferId).document.copy(content = com.serenity.rope.Rope("1a2b3")),
              editing = AppState.initial.persisted
                .buffers(bufferId)
                .editing
                .copy(cursors = List(CursorPosition(0, 2), CursorPosition(0, 4)))
            )
        )
      )
    )

    val updatedState = EditorEventReducer.reduce(DeleteBackward, paneId, initialState).state
    val buffer       = updatedState.persisted.buffers(bufferId)

    buffer.document.content.collect() shouldBe "123"
    buffer.editing.cursors shouldBe List(CursorPosition(0, 1), CursorPosition(0, 2))
  }

  it should "delete forward at every cursor position when multiple cursors are active" in {
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
                AppState.initial.persisted.buffers(bufferId).document.copy(content = com.serenity.rope.Rope("1a2b3")),
              editing = AppState.initial.persisted
                .buffers(bufferId)
                .editing
                .copy(cursors = List(CursorPosition(0, 1), CursorPosition(0, 3)))
            )
        )
      )
    )

    val updatedState = EditorEventReducer.reduce(DeleteForward, paneId, initialState).state
    val buffer       = updatedState.persisted.buffers(bufferId)

    buffer.document.content.collect() shouldBe "123"
    buffer.editing.cursors shouldBe List(CursorPosition(0, 1), CursorPosition(0, 2))
  }

  it should "delete the previous word for a single cursor" in {
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
                .copy(content = com.serenity.rope.Rope("alpha beta gamma")),
              editing = AppState.initial.persisted.buffers(bufferId).editing.copy(cursors = List(CursorPosition(0, 16)))
            )
        )
      )
    )

    val updatedState = EditorEventReducer.reduce(DeleteWordBackward, paneId, initialState).state
    val buffer       = updatedState.persisted.buffers(bufferId)

    buffer.document.content.collect() shouldBe "alpha beta "
    buffer.editing.cursors shouldBe List(CursorPosition(0, 11))
  }

  it should "delete the next word for a single cursor" in {
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
                .copy(content = com.serenity.rope.Rope("alpha beta gamma")),
              editing = AppState.initial.persisted.buffers(bufferId).editing.copy(cursors = List(CursorPosition(0, 6)))
            )
        )
      )
    )

    val updatedState = EditorEventReducer.reduce(DeleteWordForward, paneId, initialState).state
    val buffer       = updatedState.persisted.buffers(bufferId)

    buffer.document.content.collect() shouldBe "alpha gamma"
    buffer.editing.cursors shouldBe List(CursorPosition(0, 6))
  }

  it should "delete the previous word once when multiple cursors overlap the same word" in {
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
                .copy(content = com.serenity.rope.Rope("alpha beta gamma")),
              editing = AppState.initial.persisted
                .buffers(bufferId)
                .editing
                .copy(cursors = List(CursorPosition(0, 8), CursorPosition(0, 10)))
            )
        )
      )
    )

    val updatedState = EditorEventReducer.reduce(DeleteWordBackward, paneId, initialState).state
    val buffer       = updatedState.persisted.buffers(bufferId)

    buffer.document.content.collect() shouldBe "alpha  gamma"
    buffer.editing.cursors shouldBe List(CursorPosition(0, 6))
  }

  it should "delete the next word once when multiple cursors overlap the same word" in {
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
                .copy(content = com.serenity.rope.Rope("alpha beta gamma")),
              editing = AppState.initial.persisted
                .buffers(bufferId)
                .editing
                .copy(cursors = List(CursorPosition(0, 6), CursorPosition(0, 8)))
            )
        )
      )
    )

    val updatedState = EditorEventReducer.reduce(DeleteWordForward, paneId, initialState).state
    val buffer       = updatedState.persisted.buffers(bufferId)

    buffer.document.content.collect() shouldBe "alpha gamma"
    buffer.editing.cursors shouldBe List(CursorPosition(0, 6))
  }

  it should "paste clipboard content at every cursor position when multiple cursors are active" in {
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
                AppState.initial.persisted.buffers(bufferId).document.copy(content = com.serenity.rope.Rope("ab")),
              editing = AppState.initial.persisted
                .buffers(bufferId)
                .editing
                .copy(cursors = List(CursorPosition(0, 0), CursorPosition(0, 2)))
            )
        )
      ),
      runtime = AppState.initial.runtime.copy(
        clipboard = Some("Z")
      )
    )

    val updatedState = EditorEventReducer.reduce(Paste, paneId, initialState).state
    val buffer       = updatedState.persisted.buffers(bufferId)

    buffer.document.content.collect() shouldBe "ZabZ"
    buffer.editing.cursors shouldBe List(CursorPosition(0, 1), CursorPosition(0, 4))
  }

  it should "unindent every cursor line when reverse-tab is pressed with multiple cursors" in {
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
                .copy(content = com.serenity.rope.Rope("    one\n  two\n\tthree")),
              editing = AppState.initial.persisted
                .buffers(bufferId)
                .editing
                .copy(cursors = List(CursorPosition(0, 4), CursorPosition(1, 2), CursorPosition(2, 6)))
            )
        )
      )
    )

    val updatedState = EditorEventReducer.reduce(ReverseTabKey, paneId, initialState).state
    val buffer       = updatedState.persisted.buffers(bufferId)

    buffer.document.content.collect() shouldBe "one\ntwo\nthree"
    buffer.editing.cursors shouldBe List(CursorPosition(0, 0), CursorPosition(1, 0), CursorPosition(2, 5))
  }
